#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>

void test_foo(void);

CAMLprim value caml_test_foo(value unit) {
    CAMLparam1(unit);
    test_foo();
    CAMLreturn(Val_unit);
}

int get_batch_data(const char*, const char*, const char*);

CAMLprim value caml_get_batch_data(value da_websocket, value da_contract_address, value location) {
    CAMLparam3(da_websocket, da_contract_address, location);

    char* da_websocket_cstr = caml_stat_strdup(String_val(da_websocket));
    char* da_contract_address_cstr = caml_stat_strdup(String_val(da_contract_address));
    char* location_cstr = caml_stat_strdup(String_val(location));

    caml_release_runtime_system();

    int result = get_batch_data(da_websocket_cstr, da_contract_address_cstr, location_cstr);

    caml_stat_free(da_websocket_cstr);
    caml_stat_free(da_contract_address_cstr);
    caml_stat_free(location_cstr);

    caml_acquire_runtime_system();

    CAMLreturn(Val_int(result));
}
