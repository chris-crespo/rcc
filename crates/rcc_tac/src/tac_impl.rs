use crate::{Label, TempVar};

impl Label {
    pub fn new(label: u32) -> Label {
        Label(label)
    }
}

impl TempVar {
    pub fn new(var: u32) -> TempVar {
        TempVar(var)
    }
}
