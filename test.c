// embed_python.c
#include <Python.h>
#include <stdio.h>

/* int main(int argc, char *argv[]) { */

/*     if (!Py_IsInitialized()) { */
        
/*         PyConfig config; */
/*         PyConfig_InitPythonConfig(&config); */
/*         config._init_main = 1; */
/*         config.site_import = 0; */

/*         Py_BytesMain(argc, argv); */
        
/*         PyConfig_SetString(&config, &config.program_name, L"/Users/dh/pyenv/frida/bin/python3"); */
/*         //PyConfig_SetString(&config, &config.base_executable, L"/Users/dh/pyenv/frida/bin/python3"); */
         
/*         PyConfig_SetString(&config, &config.pythonpath_env, L"/Users/dh/pyenv/frida"); */
/*         PyConfig_SetString(&config, &config.home, L"/Users/dh/pyenv/frida"); */
/*         PyConfig_SetString(&config, &config.platlibdir, L"/opt/homebrew/Cellar/python@3.14/3.14.0_1/Frameworks/Python.framework/Versions/3.14/lib/python3.14"); */
/*         PyWideStringList_Append(&config.module_search_paths, */
/*                                 L"/opt/homebrew/Cellar/python@3.14/3.14.0_1/Frameworks/Python.framework/Versions/3.14/lib/python3.14" */
/*                                 ); */
      
/*         PyStatus status = Py_InitializeFromConfig(&config); */
/*         if (PyStatus_Exception(status)) { */
/*             Py_ExitStatusException(status); */
/*         } */


/*         PyConfig_Clear(&config); */

/*     } */


    
/*     PyRun_SimpleString("print('Hello from C-embedded Python!')"); */
/*     PyRun_SimpleString("import sys; print('Version:', sys.path)"); */

/*     Py_Finalize(); */
/*     return 0; */
/* } */


int main() {


    if (!Py_IsInitialized()) {

        //p_Py_InitializeEx(0);
        printf("re initialized\n");  
        
        PyConfig config;
        PyConfig_InitPythonConfig(&config);
        config._init_main = 1;
        config.site_import = 1;

        //PyConfig_SetString(&config, &config.prefix, L"/Users/mf-lidezhou/Desktop/pyvenv2");
        //PyConfig_SetString(&config, &config.program_name, );
        PyConfig_SetString(&config, &config.executable, L"/Users/dh/pyenv/venv/bin/python3");
        //PyWideStringList_Append(&config.module_search_paths,
        //                        L"/opt/homebrew/Cellar/python@3.14/3.14.0_1/Frameworks/Python.framework/Versions/3.14/lib/python3.14");
        //PyWideStringList_Append(&config.module_search_paths,
        //                        L"/Users/mf-lidezhou/Desktop/pyvenv2/lib/python3.14/site-packages");
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        /* PyConfig_SetString(&config, &config.); */
        
        Py_InitializeFromConfig(&config);
        //PyRun_SimpleStringFlags("import sys\nsys.executable = \"/Users/mf-lidezhou/Desktop/pyvenv2/bin/python3\"\nimport site\nsite.main()\n", 0);
        PyRun_SimpleStringFlags("import sys\nprint(sys.prefix); print(sys.path)", 0);
        PyRun_SimpleStringFlags("import requests", 0);
        // init_python_runtime();
        // In embedded environments, ensure GIL is ready (harmless on new Pythons)
        //p_PyEval_InitThreads();
        
        Py_Finalize();
        PyConfig_Clear(&config);
    }


    
    /* if (!Py_IsInitialized()) { */
    /*     PyConfig config; */
    /*     PyConfig_InitPythonConfig(&config); */

    /*     // Point to your virtual environment's python executable */
    /*     const wchar_t* venv_executable = L"/Users/dh/pyenv/venv/bin/python3"; */
    /*     const wchar_t* venv_root = L"/Users/dh/pyenv/venv"; */

    /*     // Required: set executable */
    /*     PyConfig_SetString(&config, &config.executable, venv_executable); */

    /*     // CRITICAL: Set prefix and exec_prefix to venv root */
    /*     // PyConfig_SetString(&config, &config.prefix, venv_root); */
    /*     // PyConfig_SetString(&config, &config.exec_prefix, venv_root); */

    /*     // Enable site module (this runs site.main() on init) */
    /*     //config.site_import = 1; */

    /*     // Optional: ensure we initialize __main__ */
    /*     config._init_main = 1; */

    /*     // Initialize Python with this config */
    /*     PyStatus status = Py_InitializeFromConfig(&config); */
    /*     if (PyStatus_Exception(status)) { */
    /*         fprintf(stderr, "Py_InitializeFromConfig failed: %s\n", status.err_msg); */
    /*         PyConfig_Clear(&config); */
    /*         return 1; */
    /*     } */

    /*     // Now test imports */
    /*     PyRun_SimpleString("import sys; print('sys.executable:', sys.executable)"); */
    /*     PyRun_SimpleString("print('sys.prefix:', sys.prefix)"); */
    /*     PyRun_SimpleString("print('sys.path:', '\\n'.join(sys.path))"); */

    /*     // This should now work if 'requests' is installed in the venv */
    /*     PyRun_SimpleString("import frida; print('requests imported successfully!')"); */

    /*     Py_Finalize(); */
    /*     PyConfig_Clear(&config); */
    /* } */
    return 0;
}
