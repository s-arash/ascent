//! Empty program
use ascent::ascent_run;
fn main() {
    let _ = {
        pub struct AscentProgram {
            scc_times: [std::time::Duration; 0usize],
            scc_iters: [usize; 0usize],
            pub update_time_nanos: std::sync::atomic::AtomicU64,
            pub update_indices_duration: std::time::Duration,
        }
        impl AscentProgram {
            #[allow(noop_method_call, suspicious_double_ref_op)]
            fn update_indices_priv(&mut self) {
                let before = ::ascent::internal::Instant::now();
                use ascent::internal::ToRelIndex0;
                use ascent::internal::RelIndexWrite;
                self.update_indices_duration += before.elapsed();
            }
            #[deprecated = "Explicit call to update_indices not required anymore."]
            pub fn update_indices(&mut self) {
                self.update_indices_priv();
            }
            fn type_constraints() {}
            pub fn summary(&self) -> &'static str {
                ""
            }
            pub fn relation_sizes_summary(&self) -> String {
                use std::fmt::Write;
                let mut res = String::new();
                res
            }
            pub fn scc_times_summary(&self) -> String {
                use std::fmt::Write;
                let mut res = String::new();
                (&mut res)
                    .write_fmt(
                        format_args!(
                            "update_indices time: {0:?}\n", self.update_indices_duration,
                        ),
                    )
                    .unwrap();
                res
            }
        }
        impl Default for AscentProgram {
            fn default() -> Self {
                let mut _self = AscentProgram {
                    scc_times: [std::time::Duration::ZERO; 0usize],
                    scc_iters: [0; 0usize],
                    update_time_nanos: Default::default(),
                    update_indices_duration: std::time::Duration::default(),
                };
                _self
            }
        }
        let mut __run_res: AscentProgram = AscentProgram::default();
        #[allow(unused_imports, noop_method_call, suspicious_double_ref_op)]
        {
            ascent::internal::comment("running...");
            use core::cmp::PartialEq;
            use ascent::internal::{
                RelIndexRead, RelIndexReadAll, ToRelIndex0, TupleOfBorrowed,
            };
            use ascent::internal::RelIndexWrite;
            let _self = &mut __run_res;
        }
        __run_res
    };
}
