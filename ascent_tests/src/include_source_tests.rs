#![allow(unused)]

use ascent::{ascent, ascent_run, ascent_source};
use itertools::Itertools;

use crate::assert_rels_eq;


mod base {
    ascent::ascent_source! { tc:
        relation edge(usize, usize);
        relation path(usize, usize);
        path(x, y) <-- edge(x, y);
        path(x, z) <-- edge(x, y), path(y, z);
    }
}

ascent_source! { edges:
    edge(1, 2);
    edge(2, 3);
    edge(3, 4);
}

#[test]
fn include_ascent_source() {
    let res = ascent_run! {
        include_tokens!(base::tc);

        include_tokens!(edges);        
    };

    assert!(res.path.iter().contains(&(1, 4)));
}