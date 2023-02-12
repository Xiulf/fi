use std::fmt;

use crate::id::TypeVarId;

pub trait HirDisplay {
    type Db<'a>: ?Sized + 'a;

    fn hir_fmt(&self, f: &mut HirFormatter<Self::Db<'_>>) -> fmt::Result;

    fn into_displayable<'a>(
        &'a self,
        db: &'a Self::Db<'a>,
        max_size: Option<usize>,
        display_target: DisplayTarget,
    ) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        HirDisplayWrapper {
            t: self,
            db,
            max_size,
            display_target,
        }
    }

    fn display<'a>(&'a self, db: &'a Self::Db<'a>) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        self.into_displayable(db, None, DisplayTarget::Diagnostics)
    }

    fn display_test<'a>(&'a self, db: &'a Self::Db<'a>) -> HirDisplayWrapper<'a, Self>
    where
        Self: Sized,
    {
        self.into_displayable(db, None, DisplayTarget::Test)
    }
}

pub struct HirFormatter<'a, Db>
where
    Db: ?Sized,
{
    pub db: &'a Db,
    fmt: &'a mut dyn fmt::Write,
    buf: String,
    curr_size: usize,
    max_size: Option<usize>,
    pub display_target: DisplayTarget,
}

pub struct HirDisplayWrapper<'a, T>
where
    T: HirDisplay,
{
    t: &'a T,
    db: &'a T::Db<'a>,
    max_size: Option<usize>,
    display_target: DisplayTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayTarget {
    Diagnostics,
    Test,
}

impl<'a, Db> HirFormatter<'a, Db>
where
    Db: ?Sized + 'a,
{
    pub fn with_upcast<T, Db2>(
        &mut self,
        upcast: impl FnOnce(&Db) -> &Db2,
        f: impl FnOnce(&mut HirFormatter<Db2>) -> T,
    ) -> T
    where
        Db2: ?Sized + 'a,
    {
        let buf = std::mem::replace(&mut self.buf, String::new());
        let mut fmt = HirFormatter {
            buf,
            db: upcast(self.db),
            fmt: self.fmt,
            curr_size: self.curr_size,
            max_size: self.max_size,
            display_target: self.display_target,
        };

        let res = f(&mut fmt);
        self.buf = fmt.buf;
        res
    }

    pub fn write_joined<'b, T>(&mut self, iter: impl IntoIterator<Item = T>, sep: &str) -> fmt::Result
    where
        T: HirDisplay<Db<'b> = Db>,
    {
        let mut first = true;

        for e in iter {
            if !first {
                fmt::Write::write_str(self, sep)?;
            }

            first = false;
            e.hir_fmt(self)?;
        }

        Ok(())
    }

    pub fn should_truncate(&self) -> bool {
        if let Some(max_size) = self.max_size {
            self.curr_size >= max_size
        } else {
            false
        }
    }
}

impl<'a, Db> fmt::Write for HirFormatter<'a, Db>
where
    Db: ?Sized,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.curr_size += s.len();
        self.fmt.write_str(s)
    }

    fn write_fmt(mut self: &mut Self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.buf.clear();
        fmt::write(&mut self.buf, args)?;
        self.curr_size += self.buf.len();
        self.fmt.write_str(&self.buf)
    }
}

impl<'a, T> HirDisplay for &'a T
where
    T: HirDisplay,
{
    type Db<'b> = T::Db<'b>;

    fn hir_fmt(&self, f: &mut HirFormatter<Self::Db<'_>>) -> fmt::Result {
        (**self).hir_fmt(f)
    }
}

impl<'a, T> fmt::Display for HirDisplayWrapper<'a, T>
where
    T: HirDisplay,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.t.hir_fmt(&mut HirFormatter {
            db: self.db,
            fmt: f,
            buf: String::with_capacity(20),
            curr_size: 0,
            max_size: self.max_size,
            display_target: self.display_target,
        })
    }
}

pub struct Indent<'a, W: fmt::Write>(&'a mut W, bool, &'a str);

pub fn indent<'a, W: fmt::Write>(f: &'a mut W) -> Indent<'a, W> {
    Indent(f, true, "    ")
}

impl<'a, W: fmt::Write> fmt::Write for Indent<'a, W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            if c == '\n' {
                self.0.write_char(c)?;
                self.1 = true;
                continue;
            }

            if self.1 {
                self.0.write_str(self.2)?;
                self.1 = false;
            }

            self.0.write_char(c)?;
        }

        Ok(())
    }
}

impl HirDisplay for TypeVarId {
    type Db<'a> = dyn crate::Db + 'a;

    fn hir_fmt(&self, f: &mut HirFormatter<Self::Db<'_>>) -> fmt::Result {
        use std::fmt::Write as _;
        let owner = self.owner(f.db);
        let type_map = owner.type_map(f.db).0;
        let name = match self.local_id(f.db) {
            | either::Either::Left(local_id) => type_map[local_id].name,
            | either::Either::Right(name) => name,
        };

        write!(f, "{}", name.display(f.db))
    }
}
