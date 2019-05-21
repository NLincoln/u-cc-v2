use std::collections::HashMap;

pub struct SymbolTable<T> {
    inner: Vec<HashMap<String, T>>,
}

impl<T> Default for SymbolTable<T> {
    fn default() -> SymbolTable<T> {
        SymbolTable {
            inner: vec![Default::default()],
        }
    }
}

impl<T> SymbolTable<T> {
    pub fn push_scope(&mut self) {
        self.inner.push(Default::default());
    }
    pub fn pop_scope(&mut self) {
        self.inner.pop();
    }
    pub fn lookup(&self, key: &str) -> Option<&T> {
        for scope in self.inner.iter().rev() {
            match scope.get(key) {
                found @ Some(_) => return found,
                None => {}
            }
        }
        None
    }
    pub fn insert<S: Into<String>>(&mut self, s: S, val: T) {
        self.inner.last_mut().unwrap().insert(s.into(), val);
    }
}
