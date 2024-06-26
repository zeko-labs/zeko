#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <stdio.h>

extern void free_string(char* s);

extern int get_batch_data(const char*, const char*, const char*, char** output, char** error);

// string -> string -> string -> (string, string) result
CAMLprim value caml_get_batch_data(value da_websocket, value da_contract_address, value location)
{
    CAMLparam3(da_websocket, da_contract_address, location);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));
    char* location_cstr = caml_stat_strdup(String_val(location));

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char *output, *error;
    int rust_result = get_batch_data(da_websocket_cstr, da_contract_address_cstr, location_cstr, &output, &error);

    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);
    caml_stat_free(location_cstr);

    // Reacquire the ocaml runtime system
    caml_acquire_runtime_system();

    if (rust_result == 0) {
        // Error
        CAMLlocal1(result);
        result = caml_alloc(1, 1); // Allocate a block with 1 field, tag 1 (Error)
        Store_field(result, 0, caml_copy_string(error));
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        free_string(output);
        CAMLreturn(result);
    }
}
