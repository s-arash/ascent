//! internal utility functions defined here
//!
//! CAUTION: anything defined here is subject to change in semver-compatible releases

/// update `reference` in-place using the provided closure
pub fn update<T: Default>(reference: &mut T, f: impl FnOnce(T) -> T) {
   let ref_taken = std::mem::take(reference);
   let new_val = f(ref_taken);
   *reference = new_val;
}

#[test]
fn test_update() {
   let mut vec = vec![1, 2, 3];
   update(&mut vec, |mut v| {
      v.push(4);
      v
   });
   assert_eq!(vec, vec![1, 2, 3, 4]);
}
