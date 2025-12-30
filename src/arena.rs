use std::num::NonZero;

use rustc_hash::FxHashMap;

use crate::vm::Value;

pub type StringRef = NonZero<u32>;
// change to struct and add marked for gc
#[derive(Clone)]
pub struct Arena {
    pub strings: Vec<String>,
    pub globals: FxHashMap<u32, Value>,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            globals: FxHashMap::default(),
            strings: Vec::with_capacity(256),
        }
    }

    pub fn alloc_string(&mut self, obj: String) -> StringRef {
        self.strings.push(obj);
        NonZero::new(self.strings.len() as u32).unwrap()
    }

    pub fn get_string(&self, id: u32) -> &str {
        &self.strings[(id - 1) as usize]
    }
}
