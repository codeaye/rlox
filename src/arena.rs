use rustc_hash::FxHashMap;

use crate::vm::Value;

// change to struct and add marked for gc
#[derive(Clone, Debug)]
pub struct Arena {
    pub strings: Vec<String>,
    pub globals: FxHashMap<u16, Value>,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            globals: FxHashMap::default(),
            strings: Vec::with_capacity(128),
        }
    }

    pub fn alloc_string(&mut self, obj: String) -> u16 {
        self.strings.push(obj);
        (self.strings.len() - 1) as u16
    }

    pub fn get_string(&self, id: u16) -> &str {
        &self.strings[id as usize]
    }
}
