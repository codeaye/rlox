pub type StringRef = usize;
// change to struct and add marked for gc
#[derive(Clone)]
pub struct Arena {
    pub strings: Vec<String>,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            strings: Vec::with_capacity(256),
        }
    }

    pub fn alloc_string(&mut self, obj: String) -> StringRef {
        self.strings.push(obj);
        self.strings.len() - 1
    }

    pub fn get_string(&self, id: StringRef) -> &str {
        &self.strings[id]
    }
}
