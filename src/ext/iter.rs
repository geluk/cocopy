pub trait MapFirst<I> {
    fn map_first(self) -> First<I>;
}

#[derive(Debug)]
pub struct First<I> {
    iter: I,
}

impl<I: Iterator<Item = (A, B)>, A, B> MapFirst<I> for I {
    fn map_first(self) -> First<I> {
        First { iter: self }
    }
}

impl<I: Iterator<Item = (A, B)>, A, B> Iterator for First<I> {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(a, _)| a)
    }
}
