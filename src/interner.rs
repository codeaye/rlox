use std::num::NonZero;

use rustc_hash::FxHashMap;

use crate::arena::StringRef;

#[derive(Debug, Clone)]
pub struct Interner {
    ids: Vec<String>,
    map: FxHashMap<String, u32>,
}
impl Interner {
    pub fn new() -> Self {
        Self {
            ids: Vec::new(),
            map: FxHashMap::default(),
        }
    }

    pub fn intern(&mut self, to_store: String) -> StringRef {
        match self.map.get(&to_store) {
            Some(v) => NonZero::new(*v).unwrap(),
            None => {
                self.ids.push(to_store.clone());
                let id = self.ids.len() as u32;
                self.map.insert(to_store, id);
                NonZero::new(id).unwrap()
            }
        }
    }

    pub fn resolve(&self, id: u32) -> &str {
        &self.ids[(id - 1) as usize]
    }
}
