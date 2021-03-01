use std::fmt;

#[derive(Debug)]
pub struct InternalError(String, Option<Box<InternalError>>);

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Err: {}", self.0)?;
        if let Some(err) = &self.1 {
            write!(f, " Caused by: {}", err)?;
        }
        writeln!(f)
    }
}

impl InternalError {
    pub fn new(msg:&str) -> Self {
        InternalError(String::from(msg), None)
    }
    pub fn extend(self, msg:&str) -> Self {
        InternalError(String::from(msg), Some(Box::from(self)))
    }
}
