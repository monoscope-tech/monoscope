use hs_bindgen::*;

#[hs_bindgen]
fn hello(name: &str) {
    println!("Hello, {name}!");
}
