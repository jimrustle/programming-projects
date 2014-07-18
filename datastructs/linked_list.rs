/*
 * Linked list implementation
 * (pretty much a rip from the Rust language tutorial)
 * static.rust-lang.org/doc/master/tutorial.html
 * rustbyexample.com/enum.html
 */

//use std::mem::size_of;


// Rust enums are like C unions
// no type generics
enum List {
    Cons(u8, Box<List>), // either a cell of a u8 value and List
    Nil // or empty
}

fn create_list() -> List {
    return Cons(0, box Nil)
}

// Rust linked lists are built backwards
// prepends a 0 in front of the linked list
fn inc_list_len(n:List) -> List {
    Cons(0, box n)
}

fn print_list(n:List){
    match n {
        Cons(car, cdr) =>{
            print!("[{}]->", car);
            print_list(*cdr);
        },
        Nil => {
            print!("Nil\n");
        },
    }
}

fn main() {
    let mut a = create_list();
    a = inc_list_len(a);
    a = inc_list_len(a);
    a = inc_list_len(a);
    a = inc_list_len(a);
    a = inc_list_len(a);
    print_list(a);
}
