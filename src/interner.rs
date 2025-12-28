use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct Interner {
    ids: Vec<String>,
    map: FxHashMap<String, usize>,
}
impl Interner {
    pub fn new() -> Self {
        Self {
            ids: Vec::new(),
            map: FxHashMap::default(),
        }
    }

    pub fn intern(&mut self, to_store: String) -> usize {
        match self.map.get(&to_store) {
            Some(v) => *v,
            None => {
                self.ids.push(to_store.clone());
                let id = self.ids.len() - 1;
                self.map.insert(to_store, id);
                id
            }
        }
    }

    pub fn resolve(&self, id: usize) -> &str {
        &self.ids[id]
    }
}
