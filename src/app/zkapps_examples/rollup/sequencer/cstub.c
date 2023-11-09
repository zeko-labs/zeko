#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

void applySignedCommand(char* inp) {
    static const value* closure = NULL;
    if (closure == NULL)
        closure = caml_named_value("applySignedCommand");

    caml_callback_exn(*closure, caml_copy_string(inp));
}

void init() {
    char** argv = malloc(sizeof(char*) * 2);
    *argv = "argv";

    caml_startup(argv);
}
