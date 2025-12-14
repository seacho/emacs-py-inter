// embed_python.c
#include <Python.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    Py_Initialize();

    if (!Py_IsInitialized()) {
        fprintf(stderr, "Failed to initialize Python.\n");
        return 1;
    }

    PyRun_SimpleString("print('Hello from C-embedded Python!')");
    PyRun_SimpleString("import sys; print('Version:', sys.version)");

    Py_Finalize();
    return 0;
}
