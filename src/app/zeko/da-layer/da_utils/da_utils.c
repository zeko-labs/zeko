#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <stdio.h>

char** caml_string_list_to_c_array(value caml_list, uintptr_t* len)
{
    CAMLparam1(caml_list);
    CAMLlocal2(head, tail);

    uintptr_t list_length = 0;
    value list = caml_list;

    // count the length of the list
    while (list != Val_emptylist) {
        list_length++;
        list = Field(list, 1);
    }

    // Allocate array of char*
    char** c_array = (char**)malloc(list_length * sizeof(char*));

    // Reset list and fill the array
    list = caml_list;
    for (int i = 0; i < list_length; i++) {
        head = Field(list, 0);
        tail = Field(list, 1);

        c_array[i] = caml_stat_strdup(String_val(head));

        list = tail;
    }

    // Set the length
    *len = list_length;

    CAMLreturnT(char**, c_array);
}

extern void free_string(char* s);

extern int post_batch(
    const char* da_websocket,
    const char* da_contract_address,
    const char* da_private_key,
    const char* batch_data,
    char** sig_data,
    uintptr_t sig_data_len,
    char** output_ptr,
    char** error_ptr);

// string -> string -> string -> string -> string list -> (string, string) result
CAMLprim value caml_post_batch(value da_websocket, value da_contract_address, value da_private_key, value batch_data, value sig_data)
{
    CAMLparam5(da_websocket, da_contract_address, da_private_key, batch_data, sig_data);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));
    char* da_private_key_cstr = caml_stat_strdup(String_val(da_private_key));
    char* batch_data_cstr = caml_stat_strdup(String_val(batch_data));

    // Convert the ocaml list of strings to a C array of strings
    uintptr_t sig_data_len;
    char** sig_data_cstr = caml_string_list_to_c_array(sig_data, &sig_data_len);

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char *output, *error;
    int rust_result = post_batch(da_websocket_cstr, da_contract_address_cstr, da_private_key_cstr, batch_data_cstr, sig_data_cstr, sig_data_len, &output, &error);

    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);
    caml_stat_free(da_private_key_cstr);
    caml_stat_free(batch_data_cstr);

    for (int i = 0; i < sig_data_len; i++) {
        caml_stat_free(sig_data_cstr[i]);
    }

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

extern int get_batch_data(
    const char* da_websocket,
    const char* da_contract_address,
    const char* location,
    char** output_ptr,
    char** error_ptr);

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
