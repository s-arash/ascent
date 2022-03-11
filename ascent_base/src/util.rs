//! internal utility functions defined here

/// update `reference` in-place using the provided closure
pub fn update<T>(reference: &mut T, f: impl FnOnce(T) -> T) {
   unsafe {
      let ref_taken = std::ptr::read(reference);
      let new_val = f(ref_taken);
      std::ptr::write(reference, new_val);
   }
}

#[test]
fn test_update(){
   let mut vec = vec![1, 2, 3];
   update(&mut vec, |mut v| {v.push(4); v});
   assert_eq!(vec, vec![1, 2, 3, 4]);
}