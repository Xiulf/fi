use core::alloc::Layout;

#[no_mangle]
pub unsafe extern "C" fn box_alloc(size: usize) -> *mut u8 {
    let count = core::mem::size_of::<usize>();
    let align = core::mem::align_of::<usize>();
    let layout = Layout::from_size_align(size + count, align).unwrap();
    let ptr = std::alloc::alloc(layout);
    *(ptr as *mut usize) = 1;
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn box_free(ptr: *mut u8, size: usize, drop: extern "C" fn(*const u8)) {
    *(ptr as *mut usize) = *(ptr as *const usize) - 1;
    if *(ptr as *const usize) == 0 {
        let count = core::mem::size_of::<usize>();
        let align = core::mem::align_of::<usize>();
        let layout = Layout::from_size_align(size + count, align).unwrap();
        drop(ptr.add(count));
        std::alloc::dealloc(ptr, layout)
    }
}

#[no_mangle]
pub unsafe extern "C" fn box_copy(ptr: *mut u8) {
    *(ptr as *mut usize) = *(ptr as *const usize) + 1;
}

#[no_mangle]
pub extern "C" fn drop_nop(_: *mut u8) {
}
