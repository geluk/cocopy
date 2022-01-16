use crate::span::{Bytes, Span};

pub fn find_line(source: &str, target_position: Bytes) -> LineContext {
    if source.is_empty() {
        return LineContext {
            line_no: 1,
            range: Span::new(Bytes::new(0), Bytes::new(0)),
            source: &"",
        };
    }

    let lines = source
        .split_inclusive(|c| c == '\n' || c == '\r')
        .enumerate();

    let mut position = Bytes::new(0);
    for (line_idx, line) in lines {
        let end_position = position + line.len();
        if target_position >= position && target_position <= end_position {
            return LineContext {
                source: line,
                range: Span::new(position, end_position),
                line_no: line_idx + 1,
            };
        }
        position = end_position;
    }

    panic!("Target position was outside the bounds of the source string")
}

/// Contains a slice pointing to a single line in the program,
/// enhanced with contextual information describing its line number and byte range.
pub struct LineContext<'a> {
    source: &'a str,
    range: Span,
    line_no: usize,
}
impl LineContext<'_> {
    pub fn for_display(&self) -> String {
        self.source.replace(&['\r', '\n'], "")
    }

    pub fn source(&self) -> &str {
        self.source
    }

    pub fn range(&self) -> Span {
        self.range
    }

    pub fn line_no(&self) -> usize {
        self.line_no
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn first_character_of_empty_line() {
        let context = find_line("", Bytes::new(0));

        assert_eq!(context.line_no, 1);
        assert_eq!(context.range, Span::new(Bytes::new(0), Bytes::new(0)));
        assert_eq!(context.source, "");
    }

    #[test]
    pub fn first_character_of_single_line() {
        let context = find_line("abc", Bytes::new(0));

        assert_eq!(context.line_no, 1);
        assert_eq!(context.range, Span::new(Bytes::new(0), Bytes::new(3)));
        assert_eq!(context.source, "abc");
    }

    #[test]
    pub fn first_character_of_next_line() {
        let context = find_line("abc\ndef", Bytes::new(5));

        assert_eq!(context.line_no, 2);
        assert_eq!(context.range, Span::new(Bytes::new(4), Bytes::new(7)));
        assert_eq!(context.source, "def");
    }
}
