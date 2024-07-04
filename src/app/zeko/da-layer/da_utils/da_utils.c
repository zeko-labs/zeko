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

// Ocaml function signature:
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
    // Call the function
    int rust_result = post_batch(da_websocket_cstr, da_contract_address_cstr, da_private_key_cstr, batch_data_cstr, sig_data_cstr, sig_data_len, &output, &error);

    // Free up the memory
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
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        // This was allocated by rust library, so we need to free it in rust
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

// Ocaml function signature:
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
    // Call the function
    int rust_result = get_batch_data(da_websocket_cstr, da_contract_address_cstr, location_cstr, &output, &error);

    // Free up the memory
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
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        // This was allocated by rust library, so we need to free it in rust
        free_string(output);
        CAMLreturn(result);
    }
}

extern int init_genesis_state(
    const char* da_websocket,
    const char* da_contract_address,
    const char* da_private_key,
    const char* data,
    char** error_ptr);

// Ocaml function signature:
// string -> string -> string -> string -> (unit, string) result
CAMLprim value caml_init_genesis_state(value da_websocket, value da_contract_address, value da_private_key, value data)
{
    CAMLparam4(da_websocket, da_contract_address, da_private_key, data);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));
    char* da_private_key_cstr = caml_stat_strdup(String_val(da_private_key));
    char* data_cstr = caml_stat_strdup(String_val(data));

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char* error;
    // Call the function
    int rust_result = init_genesis_state(da_websocket_cstr, da_contract_address_cstr, da_private_key_cstr, data_cstr, &error);

    // Free up the memory
    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);
    caml_stat_free(da_private_key_cstr);
    caml_stat_free(data_cstr);

    // Reacquire the ocaml runtime system
    caml_acquire_runtime_system();

    if (rust_result == 0) {
        // Error
        CAMLlocal1(result);
        result = caml_alloc(1, 1); // Allocate a block with 1 field, tag 1 (Error)
        Store_field(result, 0, caml_copy_string(error));
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, Val_unit);
        CAMLreturn(result);
    }
}

extern int get_genesis_state(
    const char* da_websocket,
    const char* da_contract_address,
    char** output_ptr,
    char** error_ptr);

// Ocaml function signature:
// string -> string -> (string, string) result
CAMLprim value caml_get_genesis_state(value da_websocket, value da_contract_address)
{
    CAMLparam2(da_websocket, da_contract_address);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char *output, *error;
    // Call the function
    int rust_result = get_genesis_state(da_websocket_cstr, da_contract_address_cstr, &output, &error);

    // Free up the memory
    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);

    // Reacquire the ocaml runtime system
    caml_acquire_runtime_system();

    if (rust_result == 0) {
        // Error
        CAMLlocal1(result);
        result = caml_alloc(1, 1); // Allocate a block with 1 field, tag 1 (Error)
        Store_field(result, 0, caml_copy_string(error));
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        // This was allocated by rust library, so we need to free it in rust
        free_string(output);
        CAMLreturn(result);
    }
}

extern int deploy(
    const char* da_websocket,
    const char* da_private_key,
    u_int64_t quorum,
    char** validators,
    uintptr_t validators_len,
    char** output_ptr,
    char** error_ptr);

// Ocaml function signature:
// string -> string -> uint64 -> string list -> (string, string) result
CAMLprim value caml_deploy(value da_websocket, value da_private_key, value quorum, value validators)
{
    CAMLparam4(da_websocket, da_private_key, quorum, validators);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_private_key_cstr = caml_stat_strdup(String_val(da_private_key));
    u_int64_t quorum_c = Int64_val(quorum);

    uintptr_t validators_len;
    char** validators_cstr = caml_string_list_to_c_array(validators, &validators_len);

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char *output, *error;
    // Call the function
    int rust_result = deploy(da_websocket_cstr, da_private_key_cstr, quorum_c, validators_cstr, validators_len, &output, &error);

    // Free up the memory
    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_private_key_cstr);

    for (int i = 0; i < validators_len; i++) {
        caml_stat_free(validators_cstr[i]);
    }

    // Reacquire the ocaml runtime system
    caml_acquire_runtime_system();

    if (rust_result == 0) {
        // Error
        CAMLlocal1(result);
        result = caml_alloc(1, 1); // Allocate a block with 1 field, tag 1 (Error)
        Store_field(result, 0, caml_copy_string(error));
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        // This was allocated by rust library, so we need to free it in rust
        free_string(output);
        CAMLreturn(result);
    }
}

extern int post_batch_signature(
    const char* da_websocket,
    const char* da_contract_address,
    const char* da_private_key,
    const char* location,
    const char* mina_pk,
    const char* sig_rx,
    const char* sig_s,
    char** error_ptr);

// Ocaml function signature:
// string -> string -> string -> string -> string -> string -> string -> (unit, string) result
CAMLprim value caml_post_batch_signature(
    value da_websocket,
    value da_contract_address,
    value da_private_key,
    value location,
    value mina_pk,
    value sig_rx,
    value sig_s)
{
    CAMLparam5(da_websocket, da_contract_address, da_private_key, location, mina_pk);
    CAMLxparam2(sig_rx, sig_s);

    // Convert the ocaml strings to C strings
    // This needs to happen before releasing the ocaml runtime system
    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));
    char* da_private_key_cstr = caml_stat_strdup(String_val(da_private_key));
    char* location_cstr = caml_stat_strdup(String_val(location));
    char* mina_pk_cstr = caml_stat_strdup(String_val(mina_pk));
    char* sig_rx_cstr = caml_stat_strdup(String_val(sig_rx));
    char* sig_s_cstr = caml_stat_strdup(String_val(sig_s));

    // To make it work with async we need to release the ocaml runtime system
    caml_release_runtime_system();

    char* error;
    // Call the function
    int rust_result = post_batch_signature(da_websocket_cstr, da_contract_address_cstr, da_private_key_cstr, location_cstr, mina_pk_cstr, sig_rx_cstr, sig_s_cstr, &error);

    // Free up the memory
    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);
    caml_stat_free(da_private_key_cstr);
    caml_stat_free(location_cstr);
    caml_stat_free(mina_pk_cstr);
    caml_stat_free(sig_rx_cstr);
    caml_stat_free(sig_s_cstr);

    // Reacquire the ocaml runtime system
    caml_acquire_runtime_system();

    if (rust_result == 0) {
        // Error
        CAMLlocal1(result);
        result = caml_alloc(1, 1); // Allocate a block with 1 field, tag 1 (Error)
        Store_field(result, 0, caml_copy_string(error));
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, Val_unit);
        CAMLreturn(result);
    }
}

CAMLprim value caml_post_batch_signature_bytecode(value* argv, int argn)
{
    return caml_post_batch_signature(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

extern int get_batch_signatures(
    const char* da_websocket,
    const char* da_contract_address,
    const char* location,
    char** output_ptr,
    char** error_ptr);

// Ocaml function signature:
// string -> string -> string -> (string, string) result
CAMLprim value caml_get_batch_signatures(value da_websocket, value da_contract_address, value location)
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
    // Call the function
    int rust_result = get_batch_signatures(da_websocket_cstr, da_contract_address_cstr, location_cstr, &output, &error);

    // Free up the memory
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
        // This was allocated by rust library, so we need to free it in rust
        free_string(error);
        CAMLreturn(result);
    } else {
        // Ok
        CAMLlocal1(result);
        result = caml_alloc(1, 0); // Allocate a block with 1 field, tag 0 (Ok)
        Store_field(result, 0, caml_copy_string(output));
        // This was allocated by rust library, so we need to free it in rust
        free_string(output);
        CAMLreturn(result);
    }
}