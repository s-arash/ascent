mod tc;
mod gpu;

// https://bheisler.github.io/RustaCUDA/rustacuda/index.html
#[macro_use]
extern crate rustacuda;
extern crate rustacuda_core;

use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Benchmark for tc.rs
    // let start = Instant::now();
    // tc::main()?;
    // let duration = start.elapsed();
    // println!("Time taken for tc.rs: {:?}", duration);

    // Benchmark for gpu.rs
    let start = Instant::now();
    gpu::main()?;
    let duration = start.elapsed();
    println!("Time taken for gpu.rs: {:?}", duration);

    Ok(())
}
