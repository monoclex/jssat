pub struct Runtime {}

#[no_mangle]
pub extern "C" fn jssatrt_runtime_new() -> *mut Runtime {
    let runtime = Runtime::new();
    Box::into_raw(Box::new(runtime))
}

#[no_mangle]
pub unsafe extern "C" fn jssatrt_runtime_drop(runtime: *mut Runtime) {
    notnull!(runtime);

    let runtime = Box::from_raw(runtime);
    drop(runtime);
}

impl Runtime {
    pub fn new() -> Self {
        Self {}
    }
}
