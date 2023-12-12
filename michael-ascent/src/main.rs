use ascent::ascent;
use rand::Rng;
use std::collections::HashSet;
use std::error::Error;

// https://arxiv.org/pdf/2103.15217

mod tc {
    use ascent::ascent;

    ascent! {
       relation edge(i32, i32);
       relation path(i32, i32);
       // edge(x, x + 1) <-- for x in (0..1000);
       path(*x, *y) <-- edge(x,y);
       path(*x, *z) <-- edge(x,y), path(y, z);
       // path(*x, *z) <-- path(x,y), edge(y, z);

    }
}

fn loop_graph(nodes: usize) -> Vec<(i32, i32)> {
    let mut res = vec![];
    let nodes = nodes as i32;
    for x in 0..nodes {
        res.push((x, (x + 1) % nodes));
    }
    res
}

fn complete_graph(nodes: usize) -> Vec<(i32, i32, u32)> {
    let mut res = vec![];
    let nodes = nodes as i32;
    for x in 0..nodes {
        for y in 0..nodes {
            if x != y {
                res.push((x, y, 1));
            }
        }
    }
    res
}

fn bench_tc_for_graph(graph: Vec<(i32, i32)>, name: &str) {
    /**
     * This function benchmarks given a graph. I will utilize this for random or file read graphs
     */
    let before = Instant::now();
    let mut tc = tc::AscentProgram::default();
    tc.edge = graph;
    tc.run();
    let elapsed = before.elapsed();
    println!("tc for {} took {:?}", name, elapsed);
    // println!("summary: \n{}", tc.scc_times_summary());
    println!("path size: {}", tc.path.len());
}

fn random_graph(nodes: usize) -> Vec<(i32, i32)> {
    /**
     * This will generate a random graph and feed it to bench_tc_for_graph()
     */
}

fn bench_tc_path_join_path(nodes_count: i32) {
    /**
     * This function does linear graph parallel benchmarking
     */
    ascent_m_par! {
       // #![include_rule_times]
       struct TCPathJoinPath;
       relation edge(i32, i32);
       relation path(i32, i32);
       path(x, z) <-- path(x,y), path(y, z);
       path(x, y) <-- edge(x,y);
    }
    let mut tc = TCPathJoinPath::default();
    println!("{}", TCPathJoinPath::summary());

    for i in 0..nodes_count {
        tc.edge.push((i, i + 1));
    }

    let mut stopwatch = Stopwatch::start_new();
    tc.run();
    stopwatch.stop();
    println!("tc path_join_path for {} nodes took {:?}", nodes_count, stopwatch.elapsed());
    // println!("summary: \n{}", tc.scc_times_summary());
    println!("path size: {}", tc.path.len());
}

fn main() {
    bench_tc_path_join_path(100000);
}