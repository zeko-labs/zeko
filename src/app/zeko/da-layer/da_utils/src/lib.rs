mod da_layer;

use std::{ffi::CStr, os::raw::c_char};
use tokio::runtime::Runtime;

fn create_runtime() -> Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed to create runtime")
}

#[no_mangle]
pub extern "C" fn test_foo() {
    println!("Hello, world!");
}

/**
 * # Safety
 * this functions accepts dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn get_batch_data(
    da_websocket: *const c_char,
    da_contract_address: *const c_char,
    location: *const c_char,
) -> bool {
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(_) => return false,
    };

    let da_contract_address = match unsafe { CStr::from_ptr(da_contract_address) }.to_str() {
        Ok(s) => s,
        Err(_) => return false,
    };

    let location = match unsafe { CStr::from_ptr(location) }.to_str() {
        Ok(s) => s,
        Err(_) => return false,
    };

    create_runtime()
        .block_on(da_layer::get_batch_data(
            da_websocket,
            da_contract_address,
            location,
        ))
        .is_ok()
}
