mod check {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/pass/*.rs"]).and_check().expect_pass();
   }

   #[test]
   pub fn fail() {
      tryexpand::check(["tests/macros/fail/*.rs"]).expect_fail();
   }
}
