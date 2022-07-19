use std::io::{Result, Write};

pub struct IndentWriter<W: Write> {
    writer: W,
    indent: usize,
    should_indent: bool,
}

impl<W: Write> IndentWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            indent: 0,
            should_indent: false,
        }
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        self.indent -= 1;
    }
}

impl<W: Write> Write for IndentWriter<W> {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let mut start = 0;
        let mut written = 0;

        for cur in 0..buf.len() {
            if buf[cur] == b'\n' {
                self.should_indent = true;
                continue;
            }

            if self.should_indent {
                written += self.writer.write(&buf[start..cur])?;
                start = cur;
                self.should_indent = false;

                for _ in 0..self.indent {
                    self.writer.write(&[b' ', b' ', b' ', b' '])?;
                }
            }
        }

        if start < buf.len() {
            written += self.writer.write(&buf[start..])?;
        }

        Ok(written)
    }

    fn flush(&mut self) -> Result<()> {
        self.writer.flush()
    }
}
