use ascent_tests::ascent_m_par;
use std::time::Instant;

ascent_m_par!{
    struct TC;
    relation edge(i32, i32);
    relation path(i32, i32);
 
    path(*x, *y) <-- edge(x,y);
    path(*x, *z) <-- path(x, y), edge(y, z);
}

fn main() {
    let edges = (0..5000).map(|x| (x, x + 1)).collect();
    let mut prog = TC::default();

    prog.edge = edges;

    let before = Instant::now();
    prog.run();
    let took = before.elapsed();
    println!("path len: {}", prog.path.len());
    println!("took {took:?}");
}