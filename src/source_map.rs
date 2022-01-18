//! Functionality for looking up character ranges in the source code.
use crate::span::{Bytes, Span};

pub fn find_line(source: &str, target_position: Bytes) -> LineContext {
    if source.is_empty() {
        return LineContext {
            line_no: 1,
            range: Span::new(Bytes::new(0), Bytes::new(0)),
            source: "",
        };
    }

    let lines = inclusive_split_lines(source).into_iter().enumerate();

    let mut position = Bytes::new(0);
    for (line_idx, line) in lines {
        let end_position = position + line.len();
        if target_position >= position && target_position < end_position {
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

fn inclusive_split_lines(source: &str) -> Vec<&str> {
    let mut seen_lines = vec![];
    let mut start = 0;
    let mut position = 0;

    let mut char_enum = source.chars().peekable();

    while let Some(ch) = char_enum.next() {
        position += ch.len_utf8();

        // If we find an \n followed by an \r, delay recognising the line until the next iteration.
        if ch == '\n' || (ch == '\r' && char_enum.peek() != Some(&'\n')) {
            seen_lines.push(&source[start..position]);
            start = position;
        }
    }
    seen_lines.push(&source[start..position]);

    seen_lines
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
    pub fn find_line_first_character_of_empty_line() {
        let context = find_line("", Bytes::new(0));

        assert_eq!(context.line_no, 1);
        assert_eq!(context.range, Span::new(Bytes::new(0), Bytes::new(0)));
        assert_eq!(context.source, "");
    }

    #[test]
    pub fn find_line_first_character_of_single_line() {
        let context = find_line("abc", Bytes::new(0));

        assert_eq!(context.line_no, 1);
        assert_eq!(context.range, Span::new(Bytes::new(0), Bytes::new(3)));
        assert_eq!(context.source, "abc");
    }

    #[test]
    pub fn find_line_first_character_of_next_line() {
        let context = find_line("abc\ndef", Bytes::new(5));

        assert_eq!(context.line_no, 2);
        assert_eq!(context.range, Span::new(Bytes::new(4), Bytes::new(7)));
        assert_eq!(context.source, "def");
    }

    #[test]
    pub fn inclusive_split_lines_empty_string() {
        let lines = inclusive_split_lines(&"");

        assert_eq!(lines, vec![""])
    }

    #[test]
    pub fn inclusive_split_lines_no_line_ending() {
        let lines = inclusive_split_lines(&"abcdef");

        assert_eq!(vec!["abcdef"], lines)
    }

    #[test]
    pub fn inclusive_split_lines_single_lf() {
        let lines = inclusive_split_lines(&"abc\ndef");

        assert_eq!(vec!["abc\n", "def"], lines)
    }

    #[test]
    pub fn inclusive_split_lines_single_cr() {
        let lines = inclusive_split_lines(&"abc\rdef");

        assert_eq!(vec!["abc\r", "def"], lines)
    }

    #[test]
    pub fn inclusive_split_lines_single_crlf() {
        let lines = inclusive_split_lines(&"abc\r\ndef");

        assert_eq!(vec!["abc\r\n", "def"], lines)
    }

    #[test]
    pub fn inclusive_split_lines_mixed_endings() {
        let lines = inclusive_split_lines(&"abc\r\nd\r\n\ne\rf");

        assert_eq!(vec!["abc\r\n", "d\r\n", "\n", "e\r", "f"], lines)
    }
}
