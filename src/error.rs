use std::ops::Range;

pub trait PositionalError {
    fn range(&self) -> Range<usize>;
    fn describe(&self) -> String;

    fn length(&self) -> usize {
        self.range().end - self.range().start
    }
}
