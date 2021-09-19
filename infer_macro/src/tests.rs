use std::collections::HashMap;

use proc_macro2::TokenStream;

use crate::{dl_impl, dl_impl_hir_to_code};


#[test]
fn test_macro(){
    // let inp = quote!{
    //     relation foo(i32);
    //     relation bar(i32, i32);

    //     foo(x) <-- bar(x,y);
    // };

    let inp = quote!{
        relation foo(i32);
        relation bar(i32, i32);
        relation baz(i32, i32);


        foo(x) <-- bar(x,y), baz(y, z) when x < z;
        baz(x - 1, x + 1) <-- foo(x) when x > 42; 
    };

    let inp = quote! {
        relation bar(i32, i32);
        relation foo1(i32, i32, i32);
        relation foo2(i32, Option<i32>);

        relation baz(i32, Vec<i32>);
        relation baz2(i32, Vec<i32>, i32);


        // bar(x, y + z) <-- foo1(x, y, z), foo2(z) when x < y;
        bar(*x, y + z + w.unwrap_or(0)) <-- foo1(x, y, z), foo2(x, w) when *z != 4;

        baz2(*x, exp.clone(), exp.len() as i32) <-- baz(x, exp) when *x < 100;
    };
    
    write_to_scratchpad(inp);
    
}

fn write_to_scratchpad(tokens: TokenStream) -> TokenStream{
    let code = dl_impl(tokens);
    assert!(code.is_ok());

    let code = code.unwrap();
        
    std::fs::write("src/scratchpad.rs", code.to_string());
    std::process::Command::new("rustfmt").args(&["src/scratchpad.rs"]).spawn().unwrap().wait();
    code
}

#[test]
fn test2(){
    let hash: HashMap<i32, i32> = HashMap::new();

    let mut r#mut = 42;
}

#[test]
fn test_macro_tc(){
    let input = quote!{
        relation edge(i32, i32);
        relation path(i32, i32);
  
        path(*x, *y) <-- edge(x,y);
        path(*x, *z) <-- edge(x,y), path(y, z);
    };

    write_to_scratchpad(input);
}