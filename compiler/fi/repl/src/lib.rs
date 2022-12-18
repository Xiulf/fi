#![feature(try_blocks)]

use std::io::{stdout, Stdout};

use crossterm::cursor::{
    position, MoveDown, MoveLeft, MoveRight, MoveTo, MoveToColumn, MoveToNextLine, MoveUp, RestorePosition,
    SavePosition,
};
use crossterm::event::{read, Event, KeyCode, KeyModifiers};
use crossterm::style::Print;
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, size, Clear, ClearType, ScrollUp};
use crossterm::{execute, queue, Result};

pub struct Repl<V, I> {
    stdout: Stdout,
    verifier: V,
    indent: I,
    config: Config,
}

pub struct Config {
    pub indent: usize,
}

#[derive(Debug)]
pub enum ReadLine {
    Line(String),
    Exit,
}

pub trait Verify {
    /// check whether the imput text is complete or not
    fn verify(&mut self, lines: &[String]) -> bool;
}

pub trait Indent {
    /// returns the indentation level for the current line
    fn indent(&mut self, lines: &[String], line: usize, col: usize) -> usize;

    /// whether indentation should triger when a character is inserted
    fn trigger(&mut self, ch: char) -> bool;
}

impl<V, I> Repl<V, I> {
    pub fn new(verifier: V, indent: I) -> Self {
        Repl {
            stdout: stdout(),
            config: Config { indent: 4 },
            verifier,
            indent,
        }
    }

    pub fn with_config(self, config: Config) -> Self {
        Repl { config, ..self }
    }

    pub fn clear(&mut self) -> Result<()> {
        queue!(self.stdout, Clear(ClearType::FromCursorUp))?;
        execute!(self.stdout, MoveTo(0, 0))
    }
}

struct DisableRawMode;

impl Drop for DisableRawMode {
    fn drop(&mut self) {
        disable_raw_mode().unwrap();
    }
}

impl<V: Verify, I: Indent> Repl<V, I> {
    pub fn read_line(&mut self, prompt: impl Into<String>) -> Result<ReadLine> {
        enable_raw_mode()?;

        let _disable_raw = DisableRawMode;
        let prompt = prompt.into();
        let mut lines = vec![String::new()];
        let mut indent = vec![0];
        let mut cursor = (0, 0);

        execute!(self.stdout, Print(prompt.clone()))?;

        loop {
            match read()? {
                | Event::Key(evt) => match evt.code {
                    | KeyCode::Char('c') if evt.modifiers.contains(KeyModifiers::CONTROL) => {
                        queue!(self.stdout, Clear(ClearType::CurrentLine))?;
                        execute!(self.stdout, MoveToColumn(0))?;

                        return Ok(ReadLine::Exit);
                    },
                    | KeyCode::Enter => {
                        if cursor.1 == lines.len() - 1 && self.verifier.verify(&lines) {
                            if position().unwrap().1 == size().unwrap().1 - 1 {
                                queue!(self.stdout, ScrollUp(1))?;
                            }

                            execute!(self.stdout, MoveToNextLine(1))?;

                            return Ok(ReadLine::Line(lines.join("\n")));
                        } else {
                            lines.push(" ".repeat(indent[cursor.1] * self.config.indent));
                            indent.push(indent[cursor.1]);
                            cursor.1 += 1;

                            let lvl = self.indent.indent(&lines, cursor.1, cursor.0);

                            cursor.0 = lvl * self.config.indent;

                            if lvl > indent[cursor.1] {
                                lines[cursor.1]
                                    .insert_str(0, &" ".repeat((lvl - indent[cursor.1]) * self.config.indent));
                            } else if lvl < indent[cursor.1] {
                                lines[cursor.1].replace_range(0..(indent[cursor.1] - lvl) * self.config.indent, "");
                            }

                            indent[cursor.1] = lvl;

                            if position().unwrap().1 == size().unwrap().1 - 1 {
                                queue!(self.stdout, ScrollUp(1))?;
                            }

                            queue!(self.stdout, MoveToNextLine(1))?;
                            queue!(self.stdout, Print(" ".repeat(prompt.len())))?;
                            execute!(self.stdout, Print(lines[cursor.1].clone()))?;
                        }
                    },
                    | KeyCode::Char(c) => {
                        lines[cursor.1].insert(cursor.0, c);

                        if self.indent.trigger(c) {
                            let lvl = self.indent.indent(&lines, cursor.1, cursor.0);

                            if lvl > indent[cursor.1] {
                                lines[cursor.1]
                                    .insert_str(0, &" ".repeat((lvl - indent[cursor.1]) * self.config.indent));

                                cursor.0 += indent[cursor.1] * self.config.indent;
                            } else if lvl < indent[cursor.1] {
                                lines[cursor.1].replace_range(0..(indent[cursor.1] - lvl) * self.config.indent, "");

                                cursor.0 -= (indent[cursor.1] - lvl) * self.config.indent;
                            }

                            indent[cursor.1] = lvl;
                        }

                        cursor.0 += 1;

                        queue!(
                            self.stdout,
                            MoveToColumn(0),
                            Clear(ClearType::CurrentLine),
                            Print(if cursor.1 == 0 {
                                prompt.clone()
                            } else {
                                " ".repeat(prompt.len())
                            }),
                            Print(lines[cursor.1].clone())
                        )?;

                        execute!(self.stdout, MoveToColumn((prompt.len() + cursor.0 + 1) as u16))?;
                    },
                    | KeyCode::Left if cursor.0 > 0 => {
                        cursor.0 -= 1;
                        execute!(self.stdout, MoveLeft(1))?;
                    },
                    | KeyCode::Right if cursor.0 < lines[cursor.1].len() => {
                        cursor.0 += 1;
                        execute!(self.stdout, MoveRight(1))?;
                    },
                    | KeyCode::Home => {
                        cursor.0 = 0;
                        execute!(self.stdout, MoveToColumn(prompt.len() as u16 + 1))?;
                    },
                    | KeyCode::End => {
                        cursor.0 = lines[cursor.1].len();
                        execute!(
                            self.stdout,
                            MoveToColumn((lines[cursor.1].len() + prompt.len()) as u16 + 1)
                        )?;
                    },
                    | KeyCode::Down if cursor.1 < lines.len() - 1 => {
                        cursor.1 += 1;
                        execute!(self.stdout, MoveDown(1))?;

                        if cursor.0 > lines[cursor.1].len() {
                            cursor.0 = lines[cursor.1].len();

                            execute!(
                                self.stdout,
                                MoveToColumn((lines[cursor.1].len() + prompt.len()) as u16 + 1)
                            )?;
                        }
                    },
                    | KeyCode::Up if cursor.1 > 0 => {
                        cursor.1 -= 1;
                        execute!(self.stdout, MoveUp(1))?;

                        if cursor.0 > lines[cursor.1].len() {
                            cursor.0 = lines[cursor.1].len();

                            execute!(
                                self.stdout,
                                MoveToColumn((lines[cursor.1].len() + prompt.len()) as u16 + 1)
                            )?;
                        }
                    },
                    | KeyCode::Backspace if cursor.0 == 0 && lines.len() > 1 && lines[cursor.1].is_empty() => {
                        lines.remove(cursor.1);
                        indent.remove(cursor.1);
                        cursor.1 -= 1;
                        cursor.0 = lines[cursor.1].len();

                        queue!(
                            self.stdout,
                            MoveUp(1),
                            MoveToColumn((lines[cursor.1].len() + prompt.len()) as u16 + 1),
                            SavePosition,
                            Clear(ClearType::FromCursorDown),
                        )?;

                        for i in (cursor.1 + 1)..lines.len() {
                            queue!(
                                self.stdout,
                                MoveToNextLine(1),
                                Print(" ".repeat(prompt.len())),
                                Print(lines[i].clone()),
                            )?;
                        }

                        execute!(self.stdout, RestorePosition)?;
                    },
                    | KeyCode::Backspace if cursor.0 > 0 => {
                        cursor.0 -= 1;
                        lines[cursor.1].remove(cursor.0);

                        queue!(
                            self.stdout,
                            MoveLeft(1),
                            SavePosition,
                            MoveToColumn(0),
                            Clear(ClearType::CurrentLine),
                            Print(if cursor.1 == 0 {
                                prompt.clone()
                            } else {
                                " ".repeat(prompt.len())
                            }),
                            Print(lines[cursor.1].clone())
                        )?;

                        execute!(self.stdout, RestorePosition)?;
                    },
                    | KeyCode::Delete if cursor.0 < lines[cursor.1].len() => {
                        lines[cursor.1].remove(cursor.0);

                        queue!(
                            self.stdout,
                            SavePosition,
                            MoveToColumn(0),
                            Clear(ClearType::CurrentLine),
                            Print(if cursor.1 == 0 {
                                prompt.clone()
                            } else {
                                " ".repeat(prompt.len())
                            }),
                            Print(lines[cursor.1].clone())
                        )?;

                        execute!(self.stdout, RestorePosition)?;
                    },
                    | _ => {},
                },
                | _ => {},
            }
        }
    }
}

impl Verify for () {
    fn verify(&mut self, _text: &[String]) -> bool {
        true
    }
}

impl Indent for () {
    fn indent(&mut self, _text: &[String], _line: usize, _col: usize) -> usize {
        0
    }

    fn trigger(&mut self, _ch: char) -> bool {
        false
    }
}
