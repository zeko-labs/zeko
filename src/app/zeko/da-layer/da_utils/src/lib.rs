mod da_layer;

use da_layer::{DALayerExecutor, DALayerReader};
use mina_signer::{BaseField, PubKey};
use o1_utils::FieldHelpers;
use std::{
    ffi::{CStr, CString},
    os::raw::c_char,
};
use tokio::runtime::Runtime;

fn create_runtime() -> Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed to create runtime")
}

unsafe fn set_c_string(output_ptr: *mut *mut c_char, s: &str) {
    let c_string = CString::new(s).expect("String contained 0 byte");
    let c_ptr = c_string.into_raw();
    unsafe {
        *output_ptr = c_ptr;
    };
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn free_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        // Let borrow checker free the string
        let _ = CString::from_raw(s);
    }
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn post_batch(
    da_websocket: *const c_char,
    da_contract_address: *const c_char,
    da_private_key: *const c_char,
    batch_data: *const c_char,
    sig_data: *const *const c_char,
    sig_data_len: usize,
    output_ptr: *mut *mut c_char,
    error_ptr: *mut *mut c_char,
) -> bool {
    // Convert C strings to &str
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_websocket to string: {}", err),
            );
            return false;
        }
    };

    let da_contract_address = match unsafe { CStr::from_ptr(da_contract_address) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_contract_address to string: {}", err),
            );
            return false;
        }
    };

    let da_private_key = match unsafe { CStr::from_ptr(da_private_key) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_private_key to string: {}", err),
            );
            return false;
        }
    };

    let batch_data = match unsafe { CStr::from_ptr(batch_data) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert batch_data to string: {}", err),
            );
            return false;
        }
    };

    // Convert C strings to Vec<BaseField>
    let sig_data = match (0..sig_data_len)
        .flat_map(|i: usize| unsafe { CStr::from_ptr(*sig_data.add(i)) }.to_str())
        .flat_map(|s| s.parse::<num_bigint::BigUint>())
        .map(|uint| BaseField::from_biguint(&uint))
        .collect::<Result<Vec<BaseField>, _>>()
    {
        Ok(sig_data) => sig_data,
        Err(err) => {
            set_c_string(error_ptr, &format!("{}", err));
            return false;
        }
    };

    create_runtime().block_on(async {
        let da_layer =
            match DALayerExecutor::new(da_websocket, da_contract_address, da_private_key).await {
                Ok(da_layer) => da_layer,
                Err(err) => {
                    set_c_string(error_ptr, &format!("{}", err));
                    return false;
                }
            };

        match da_layer.post_batch(batch_data, sig_data).await {
            Ok(data) => {
                set_c_string(output_ptr, &data);
                true
            }
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                false
            }
        }
    })
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn get_batch_data(
    da_websocket: *const c_char,
    da_contract_address: *const c_char,
    location: *const c_char,
    output_ptr: *mut *mut c_char,
    error_ptr: *mut *mut c_char,
) -> bool {
    // Convert C strings to &str
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_websocket to string: {}", err),
            );
            return false;
        }
    };

    let da_contract_address = match unsafe { CStr::from_ptr(da_contract_address) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_contract_address to string: {}", err),
            );
            return false;
        }
    };

    let location = match unsafe { CStr::from_ptr(location) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert location to string: {}", err),
            );
            return false;
        }
    };

    create_runtime().block_on(async {
        let da_layer = match DALayerReader::new(da_websocket, da_contract_address).await {
            Ok(da_layer) => da_layer,
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                return false;
            }
        };

        match da_layer.get_batch_data(location).await {
            Ok(data) => {
                set_c_string(output_ptr, &data);
                true
            }
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                false
            }
        }
    })
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn init_genesis_state(
    da_websocket: *const c_char,
    da_contract_address: *const c_char,
    da_private_key: *const c_char,
    data: *const c_char,
    error_ptr: *mut *mut c_char,
) -> bool {
    // Convert C strings to &str
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_websocket to string: {}", err),
            );
            return false;
        }
    };

    let da_contract_address = match unsafe { CStr::from_ptr(da_contract_address) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_contract_address to string: {}", err),
            );
            return false;
        }
    };

    let da_private_key = match unsafe { CStr::from_ptr(da_private_key) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_private_key to string: {}", err),
            );
            return false;
        }
    };

    let data = match unsafe { CStr::from_ptr(data) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert data to string: {}", err),
            );
            return false;
        }
    };

    create_runtime().block_on(async {
        let da_layer =
            match DALayerExecutor::new(da_websocket, da_contract_address, da_private_key).await {
                Ok(da_layer) => da_layer,
                Err(err) => {
                    set_c_string(error_ptr, &format!("{}", err));
                    return false;
                }
            };

        match da_layer.init_genesis_state(data).await {
            Ok(()) => true,
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                false
            }
        }
    })
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn get_genesis_state(
    da_websocket: *const c_char,
    da_contract_address: *const c_char,
    output_ptr: *mut *mut c_char,
    error_ptr: *mut *mut c_char,
) -> bool {
    // Convert C strings to &str
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_websocket to string: {}", err),
            );
            return false;
        }
    };

    let da_contract_address = match unsafe { CStr::from_ptr(da_contract_address) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_contract_address to string: {}", err),
            );
            return false;
        }
    };

    create_runtime().block_on(async {
        let da_layer = match DALayerReader::new(da_websocket, da_contract_address).await {
            Ok(da_layer) => da_layer,
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                return false;
            }
        };

        match da_layer.get_genesis_state().await {
            Ok(data) => {
                set_c_string(output_ptr, &data);
                true
            }
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                false
            }
        }
    })
}

/**
 * # Safety
 * this function dereferences C strings
 */
#[no_mangle]
pub unsafe extern "C" fn deploy(
    da_websocket: *const c_char,
    da_private_key: *const c_char,
    quorum: u64,
    validators: *const *const c_char,
    validators_len: usize,
    output_ptr: *mut *mut c_char,
    error_ptr: *mut *mut c_char,
) -> bool {
    // Convert C strings to &str
    let da_websocket = match unsafe { CStr::from_ptr(da_websocket) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_websocket to string: {}", err),
            );
            return false;
        }
    };

    let da_private_key = match unsafe { CStr::from_ptr(da_private_key) }.to_str() {
        Ok(s) => s,
        Err(err) => {
            set_c_string(
                error_ptr,
                &format!("Failed to convert da_private_key to string: {}", err),
            );
            return false;
        }
    };

    // Convert C strings to Vec<PubKey>
    let validators = (0..validators_len)
        .flat_map(|i: usize| unsafe { CStr::from_ptr(*validators.add(i)) }.to_str())
        .flat_map(PubKey::from_address)
        .collect::<Vec<PubKey>>();

    create_runtime().block_on(async {
        match DALayerExecutor::deploy(da_websocket, da_private_key, quorum, validators).await {
            Ok(address) => {
                set_c_string(output_ptr, &address);
                true
            }
            Err(err) => {
                set_c_string(error_ptr, &format!("{}", err));
                false
            }
        }
    })
}
