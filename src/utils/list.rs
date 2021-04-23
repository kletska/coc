use std::rc::Rc;
use std::ops::Index;
pub enum List<T> {
    Nil,
    Cons(T, Rc<List<T>>),
}

impl<T> List<T> {
    pub fn new(head: T, tail: Rc<List<T>>) -> Rc<List<T>> {
        Rc::new(List::Cons(head, tail))
    }
}

impl<T> Index<usize> for List<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        match (self, index) {
            (List::Nil, _) => unreachable!(),
            (List::Cons(head, _tail), 0) => head,
            (List::Cons(_head, tail), index) => &tail[index - 1],
        }
    }
}
