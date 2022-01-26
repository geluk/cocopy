//! Useful character extensions.
pub trait CharExt {
    fn is_non_newline_whitespace(&self) -> bool;

    fn is_linebreak(&self) -> bool;

    fn is_not_linebreak(&self) -> bool {
        !self.is_linebreak()
    }
}
impl CharExt for char {
    fn is_non_newline_whitespace(&self) -> bool {
        self.is_whitespace() && *self != '\n' && *self != '\r'
    }

    fn is_linebreak(&self) -> bool {
        *self == '\n' || *self == '\r'
    }
}
