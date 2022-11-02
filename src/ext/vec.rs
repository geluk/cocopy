pub trait RemoveWhere<T> {
    fn find_remove<P>(&mut self, predicate: P) -> Option<T>
    where
        P: Fn(&T) -> bool;
}

impl<T> RemoveWhere<T> for Vec<T> {
    fn find_remove<P>(&mut self, predicate: P) -> Option<T>
    where
        P: Fn(&T) -> bool,
    {
        self.iter().position(predicate).map(|pos| self.remove(pos))
    }
}
