//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use ascent::ascent;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn test_ascent_wasm() {
    ascent! {
        relation r(i32, i32);
        relation tc(i32, i32);

        tc(x, y) <-- r(x, y);
        tc(x, z) <-- r(x, y), tc(y, z);
    }
    let mut prog = AscentProgram::default();
    prog.r = vec![(1, 2), (2, 3)];
    prog.run();

    println!("tc: {:?}", prog.tc);
    assert_eq!(prog.tc.len(), 3);
}
