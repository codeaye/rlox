use std::{num::NonZero, ops::Range, sync::Arc};

use rustc_hash::FxHashMap;

use crate::typedef::StringRef;

#[derive(Debug, Clone)]
pub struct Interner {
    source: Arc<str>,
    ids: Vec<Range<usize>>,
    map: FxHashMap<&'static str, u32>,
}
impl Interner {
    pub fn new(source: Arc<str>) -> Self {
        Self {
            source,
            ids: Vec::new(),
            map: FxHashMap::default(),
        }
    }

    pub fn intern(&mut self, range: Range<usize>) -> StringRef {
        let slice = &self.source[range.clone()];
        let slice_static: &'static str = unsafe { std::mem::transmute(slice) };

        if let Some(&id) = self.map.get(slice_static) {
            NonZero::new(id).unwrap()
        } else {
            self.ids.push(range);
            let id = self.ids.len() as u32;
            self.map.insert(slice_static, id);
            NonZero::new(id).unwrap()
        }
    }

    pub fn resolve(&self, id: u32) -> &str {
        let range = &self.ids[(id - 1) as usize];
        &self.source[range.clone()]
    }
}
