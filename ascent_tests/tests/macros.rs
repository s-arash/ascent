mod run {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/run/pass/*.rs"]).and_run().expect_pass();
   }

   #[test]
   pub fn fail() {
      // tryexpand::expand(["tests/macros/run/fail/*.rs"]).expect_fail();
   }
}

mod check {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/check/pass/*.rs"]).and_run().expect_pass();
   }

   #[test]
   fn generic_tc() {
      tryexpand::expand(["tests/macros/check/pass/generic_tc/*.rs"]).and_run().expect_pass();
   }

   #[test]
   fn lattices() {
      tryexpand::expand(["tests/macros/check/pass/lattices/*.rs"]).and_run().expect_pass();
   }

   #[test]
   fn tc() {
      tryexpand::expand(["tests/macros/check/pass/tc/*.rs"]).and_run().expect_pass();
   }
}
