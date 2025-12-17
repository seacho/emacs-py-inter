#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>

int plugin_is_GPL_compatible;

/* ---------------------------
   Globals
--------------------------- */
static PyObject *py_main_dict = NULL;     // borrowed (dict of __main__)
static PyObject *py_main_mod  = NULL;     // borrowed (module __main__)
static PyObject *g_last_error = NULL;     // owned (unicode or NULL)

/* ---------------------------
   Emacs helpers
--------------------------- */
static char *emacs_string_to_c(emacs_env *env, emacs_value value) {
    ptrdiff_t size = 0;
    env->copy_string_contents(env, value, NULL, &size);
    char *buf = (char *)malloc((size_t)size);
    env->copy_string_contents(env, value, buf, &size);
    return buf;
}

static emacs_value emacs_make_string(emacs_env *env, const char *s) {
    if (!s) s = "";
    return env->make_string(env, s, (ptrdiff_t)strlen(s));
}

/* ---------------------------
   Python helpers
--------------------------- */
static void set_last_error(PyObject *unicode_or_null) {
    Py_XDECREF(g_last_error);
    g_last_error = unicode_or_null;
    Py_XINCREF(g_last_error);
}

static PyObject *format_current_exception(void) {
    // Returns unicode traceback string (new ref), or unicode message if failed.
    PyObject *traceback_mod = PyImport_ImportModule("traceback");
    if (!traceback_mod) {
        PyErr_Clear();
        return PyUnicode_FromString("Python error (failed to import traceback)");
    }

    PyObject *fmt_exc = PyObject_GetAttrString(traceback_mod, "format_exc");
    Py_DECREF(traceback_mod);
    if (!fmt_exc) {
        PyErr_Clear();
        return PyUnicode_FromString("Python error (failed to get traceback.format_exc)");
    }

    PyObject *s = PyObject_CallNoArgs(fmt_exc);
    Py_DECREF(fmt_exc);
    if (!s) {
        PyErr_Clear();
        return PyUnicode_FromString("Python error (failed to call traceback.format_exc)");
    }

    // s should be a unicode string already
    if (!PyUnicode_Check(s)) {
        Py_DECREF(s);
        return PyUnicode_FromString("Python error (traceback.format_exc returned non-string)");
    }
    return s; // new ref
}

static PyObject *capture_exec_file_input(const char *code_utf8) {
    // Returns unicode output (stdout+stderr) (new ref).
    // On exception: output contains captured output + traceback, and g_last_error is set.
    // Requires GIL held.
    set_last_error(NULL);

    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *io  = PyImport_ImportModule("io");
    if (!sys || !io) {
        Py_XDECREF(sys);
        Py_XDECREF(io);
        PyObject *e = format_current_exception();
        set_last_error(e);
        Py_DECREF(e);
        return PyUnicode_FromString("Failed to import sys/io");
    }

    PyObject *stringio_cls = PyObject_GetAttrString(io, "StringIO");
    Py_DECREF(io);
    if (!stringio_cls) {
        Py_DECREF(sys);
        PyObject *e = format_current_exception();
        set_last_error(e);
        Py_DECREF(e);
        return PyUnicode_FromString("Failed to get io.StringIO");
    }

    PyObject *buf_out = PyObject_CallNoArgs(stringio_cls);
    PyObject *buf_err = PyObject_CallNoArgs(stringio_cls);
    Py_DECREF(stringio_cls);

    if (!buf_out || !buf_err) {
        Py_XDECREF(buf_out);
        Py_XDECREF(buf_err);
        Py_DECREF(sys);
        PyObject *e = format_current_exception();
        set_last_error(e);
        Py_DECREF(e);
        return PyUnicode_FromString("Failed to create StringIO buffers");
    }

    PyObject *old_stdout = PyObject_GetAttrString(sys, "stdout");
    PyObject *old_stderr = PyObject_GetAttrString(sys, "stderr");

    // Redirect sys.stdout/sys.stderr
    if (PyObject_SetAttrString(sys, "stdout", buf_out) < 0 ||
        PyObject_SetAttrString(sys, "stderr", buf_err) < 0) {

        // restore best-effort
        PyErr_Clear();
    }

    // Execute code as file_input (statements)
    PyObject *res = PyRun_StringFlags(code_utf8, Py_file_input, py_main_dict, py_main_dict, NULL);

    // Always restore stdout/stderr
    if (old_stdout) PyObject_SetAttrString(sys, "stdout", old_stdout);
    if (old_stderr) PyObject_SetAttrString(sys, "stderr", old_stderr);

    Py_XDECREF(old_stdout);
    Py_XDECREF(old_stderr);
    Py_DECREF(sys);

    // Collect outputs
    PyObject *out_str = PyObject_CallMethod(buf_out, "getvalue", NULL);
    PyObject *err_str = PyObject_CallMethod(buf_err, "getvalue", NULL);
    Py_DECREF(buf_out);
    Py_DECREF(buf_err);

    if (!out_str) { PyErr_Clear(); out_str = PyUnicode_FromString(""); }
    if (!err_str) { PyErr_Clear(); err_str = PyUnicode_FromString(""); }

    // Merge stdout + stderr
    PyObject *merged = PyUnicode_Concat(out_str, err_str);
    Py_DECREF(out_str);
    Py_DECREF(err_str);
    if (!merged) {
        PyErr_Clear();
        merged = PyUnicode_FromString("");
    }

    if (!res) {
        // Append traceback to merged
        PyObject *tb = format_current_exception(); // new ref
        set_last_error(tb);

        PyObject *sep = PyUnicode_FromString("\n");
        PyObject *tmp = PyUnicode_Concat(merged, sep);
        Py_DECREF(sep);
        Py_DECREF(merged);
        merged = tmp ? tmp : PyUnicode_FromString("");

        PyObject *tmp2 = PyUnicode_Concat(merged, tb);
        Py_DECREF(merged);
        Py_DECREF(tb);
        merged = tmp2 ? tmp2 : PyUnicode_FromString("Python exception (failed to format)");

        // Clear error indicator after formatting
        PyErr_Clear();
    } else {
        Py_DECREF(res); // file_input returns None; still own it
    }

    return merged; // new ref
}

static void init_python_runtime(const char* venv_path)
{
    PyStatus status;
    PyConfig config;

    PyConfig_InitPythonConfig(&config);

    config.use_environment = 0;

    /* sys.path / site */
    config._init_main = 1;
    config.site_import = 1;


    status = PyConfig_SetString(&config, &config.executable, L"/Users/dh/pyenv/venv/bin/python");
    if (PyStatus_Exception(status)) goto fail;


    status = Py_InitializeFromConfig(&config);
    if (PyStatus_Exception(status)) goto fail;


    Py_Finalize();
    PyConfig_Clear(&config);
    return;

fail:
    PyConfig_Clear(&config);
    /* 这里不要 Py_ExitStatus(status)，Emacs 进程不能 exit */
    fprintf(stderr, "[pyembed] Python init failed\n");
}


/* ---------------------------
   Emacs module functions
--------------------------- */

static emacs_value F_py_init(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)args; (void)data;


    if (!Py_IsInitialized()) {
        // Py_Initialize();

        
        init_python_runtime();
        // In embedded environments, ensure GIL is ready (harmless on new Pythons)
        PyEval_InitThreads();
    }
    PyGILState_STATE g = PyGILState_Ensure();
    
    py_main_mod = PyImport_AddModule("__main__"); // borrowed
    if (!py_main_mod) {
        PyGILState_Release(g);
        return env->intern(env, "nil");
    }
    py_main_dict = PyModule_GetDict(py_main_mod); // borrowed
    if (!py_main_dict) {
        PyGILState_Release(g);
        return env->intern(env, "nil");
    }

    PyGILState_Release(g);
    return env->intern(env, "t");
}

/* (py-exec "code...") -> captured stdout+stderr (+ traceback if error) */
static emacs_value F_py_exec(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    if (!py_main_dict) {
        // auto-init
        emacs_value r = F_py_init(env, 0, NULL, NULL);
        (void)r;
    }

    char *code = emacs_string_to_c(env, args[0]);

    PyGILState_STATE g = PyGILState_Ensure();
    PyObject *out_u = capture_exec_file_input(code);
    PyGILState_Release(g);

    free(code);

    const char *utf8 = PyUnicode_AsUTF8(out_u);
    emacs_value ret = emacs_make_string(env, utf8 ? utf8 : "");

    Py_DECREF(out_u);
    return ret;
}

/* (py-last-error) -> traceback string or "" */
static emacs_value F_py_last_error(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)args; (void)data;
    PyGILState_STATE g = PyGILState_Ensure();

    if (!g_last_error || !PyUnicode_Check(g_last_error)) {
        PyGILState_Release(g);
        return emacs_make_string(env, "");
    }

    const char *utf8 = PyUnicode_AsUTF8(g_last_error);
    emacs_value ret = emacs_make_string(env, utf8 ? utf8 : "");

    PyGILState_Release(g);
    return ret;
}
static PyObject *g_repl_buffer = NULL;  // Python unicode

static emacs_value F_py_repl_input(emacs_env *env,
                                   ptrdiff_t nargs,
                                   emacs_value *args,
                                   void *data)
{
    (void)nargs; (void)data;

    if (!py_main_dict) {
        emacs_value r = F_py_init(env, 0, NULL, NULL);
        (void)r;
    }

    char *line = emacs_string_to_c(env, args[0]);

    PyGILState_STATE g = PyGILState_Ensure();

    if (!g_repl_buffer) {
        g_repl_buffer = PyUnicode_FromString("");
    }

    /* buffer += line + "\n" */
    PyObject *py_line = PyUnicode_FromString(line);
    PyObject *nl = PyUnicode_FromString("\n");
    PyObject *tmp = PyUnicode_Concat(g_repl_buffer, py_line);
    Py_DECREF(g_repl_buffer);
    g_repl_buffer = PyUnicode_Concat(tmp, nl);
    Py_DECREF(tmp);
    Py_DECREF(py_line);
    Py_DECREF(nl);

    /* Try compile as single_input */
    PyObject *code = Py_CompileString(
        PyUnicode_AsUTF8(g_repl_buffer),
        "<repl>",
        Py_single_input
    );

    if (!code) {
        /* Compilation failed — maybe incomplete? */
        PyObject *etype, *evalue, *etb;
        PyErr_Fetch(&etype, &evalue, &etb);
        PyErr_NormalizeException(&etype, &evalue, &etb);

        const char *msg = evalue ? PyUnicode_AsUTF8(evalue) : "";

        /* Heuristic: incomplete input */
        if (msg &&
            (strstr(msg, "unexpected EOF") ||
             strstr(msg, "expected an indented block") ||
             strstr(msg, "EOF while scanning"))) {

            PyErr_Clear();
            PyGILState_Release(g);

            free(line);
            return emacs_make_string(env, "... ");  // prompt
        }

        /* Real syntax error */
        PyErr_Restore(etype, evalue, etb);
        PyObject *tb = format_current_exception();
        set_last_error(tb);

        /* reset buffer */
        Py_CLEAR(g_repl_buffer);

        const char *out = PyUnicode_AsUTF8(tb);
        Py_DECREF(tb);
        PyGILState_Release(g);

        free(line);
        return emacs_make_string(env, out);
    }

    /* Compilation OK → execute */
    Py_DECREF(code);

    PyObject *out = capture_exec_file_input(
        PyUnicode_AsUTF8(g_repl_buffer)
    );

    /* reset buffer */
    Py_CLEAR(g_repl_buffer);

    const char *utf8 = PyUnicode_AsUTF8(out);
    emacs_value ret = emacs_make_string(env, utf8 ? utf8 : "");

    Py_DECREF(out);
    PyGILState_Release(g);
    free(line);

    return ret;
}



/* ---------------------------
   Module init
--------------------------- */
static void bind_function(emacs_env *env, const char *name,
                          emacs_value (*fn)(emacs_env*, ptrdiff_t, emacs_value*, void*),
                          int min, int max, const char *doc) {
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value sym = env->intern(env, name);
    emacs_value func = env->make_function(env, min, max, fn, doc, NULL);
    emacs_value args[] = { sym, func };
    env->funcall(env, Qfset, 2, args);
}

int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);

    bind_function(env, "py-init",      F_py_init,      0, 0, "Initialize embedded Python.");
    bind_function(env, "py-exec",      F_py_exec,      1, 1, "Execute Python code (statements) and capture stdout/stderr.");
    bind_function(env, "py-last-error",F_py_last_error,0, 0, "Get last Python traceback as string (or empty).");
    bind_function(env, "py-repl-input",
                  F_py_repl_input, 1, 1,
                  "REPL-style Python input (>>> / ...).");
    
    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value feat = env->intern(env, "inline-python-core");
    env->funcall(env, Qprovide, 1, &feat);

    return 0;
}


struct fun_ptr_tab{
    const char* module_name;
    size_t name_len;
    void* reverse;
    void* reverse2;
    void(*PyEval_SetTrace)();
    void(*PyRun_SimpleStringFlags)();
    void(*PyRun_SimpleString)();
    void(*Py_CompileStringExFlags)();
    void(*Py_FunctionNew)();
    void(*Py_Function_GetCode)();
    void(*_PyLong_AsByteArray)();
    void* reverse3;
    void* reverse4;
    void(*Py_NoSiteFlag)();
};
