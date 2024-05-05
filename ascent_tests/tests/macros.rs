mod check {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/check/pass/*.rs"]).and_check().expect_pass();
   }

   #[test]
   fn generic_tc() {
      tryexpand::expand(["tests/macros/check/pass/generic_tc/*.rs"]).and_check().expect_pass();
   }

   #[test]
   fn lattices() {
      tryexpand::expand(["tests/macros/check/pass/lattices/*.rs"]).and_check().expect_pass();
   }
}

mod run {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/run/pass/*.rs"]).and_run().expect_pass();
   }

   #[test]
   pub fn fail() {
      // tryexpand::expand(["tests/macros/run/fail/*.rs"]).and_run().expect_fail();
   }
}

mod run_tests {
   #[test]
   pub fn pass() {
      tryexpand::expand(["tests/macros/run_tests/pass/*.rs"]).and_run_tests().expect_pass();
   }

   #[test]
   pub fn fail() {
      // tryexpand::expand(["tests/macros/run_tests/fail/*.rs"]).and_run_tests().expect_fail();
   }
}
