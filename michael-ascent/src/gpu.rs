use rand::Rng;
use rustacuda::function::BlockSize;
use rustacuda::prelude::*;
use rustacuda::function::GridSize;
use std::error::Error;
use std::ffi::CString;
use std::collections::HashSet;

// ascent! {
//     relation edge(i128, i128);
//     relation path(i128, i128);
//     // i128 is 32 bit integer
//     // Rust should make sure the data is 32 bit

//     path(x, y) <-- edge(x, y);
//     path(x, z) <-- edge(x, y), path(y, z);
// }

fn gen_pairs(amnt: i128, edge_amnt: usize) -> Vec<(i128, i128)> {
    /*
       https://www.reddit.com/r/rust/comments/2552cj/let_vs_let_mut_syntax_rather_verbose/

       mut is a key to say it is mutable
    */
    let mut rng = rand::thread_rng();
    let mut edges = HashSet::new();

    while edges.len() < edge_amnt {
        let x = rng.gen_range(0..=amnt);
        let y = rng.gen_range(0..=amnt);
        if x != y {
            edges.insert((x, y));
        }
    }

    edges.into_iter().collect()
}

fn create_adjacency_matrix(nodes: i128, edges: &[(i128, i128)]) -> Vec<bool> {
    let mut matrix = vec![false; ((nodes as u128) * (nodes as u128)) as usize];
    for &(src, dest) in edges {
        if src < nodes && dest < nodes {
            matrix[(src * nodes + dest) as usize] = true;
        }
    }

    matrix
}

pub fn main() -> Result<(), Box<dyn Error>> {
    rustacuda::init(CudaFlags::empty())?;
    let device = Device::get_device(0)?;
    let _context = Context::create_and_push(ContextFlags::MAP_HOST | ContextFlags::SCHED_AUTO, device)?;

    let nodes: i128 = 100000;
    let edges: usize = 100000;

    let graph_edges: Vec<(i128, i128)> = gen_pairs(nodes, edges);
    let adjacency_matrix = create_adjacency_matrix(nodes, &graph_edges);

    let mut device_buffer = DeviceBuffer::from_slice(&adjacency_matrix)?;

    let ptx = std::fs::read_to_string("tc_kernel.ptx")?;
    let ptx_cstr = CString::new(ptx)?;
    let module = Module::load_from_string(&ptx_cstr)?;
    let function_name = CString::new("tc_kernel")?;
    let function = module.get_function(&function_name)?;

    let block_size = 128; // Assuming 1D blocks
    let block_size_1d = BlockSize::from(block_size as u32);

    // Calculate the number of blocks needed
    let grid_size = (nodes as usize + block_size - 1) / block_size;
    let grid_size_1d = GridSize::from(grid_size as u32);

    let stream = Stream::new(StreamFlags::NON_BLOCKING, None)?;
    unsafe {
        launch!(function<<<grid_size_1d, block_size_1d, 0, stream>>>(
            device_buffer.as_device_ptr(),
            nodes as i128
        ))?;
    }

    stream.synchronize()?;

    let mut host_results = vec![false; adjacency_matrix.len()];
    device_buffer.copy_to(&mut host_results)?;

    // println!("Results: {:?}", host_results);
    Ok(())
}
