use std::collections::HashMap;
use std::hash::Hash;

/// Nested hash map (for implementing environments).
pub struct NestedMap<K, V> {
  outer: Option<NestedMapPtr<K, V>>,
  map: HashMap<K, V>,
}

/// Pointer of nested hash maps.
pub type NestedMapPtr<K, V> = Box<NestedMap<K, V>>;

impl<K, V> NestedMap<K, V>
where
  K: Hash + Eq,
{
  /// Creates a new nested map.
  pub fn new() -> NestedMapPtr<K, V> {
    Box::new(NestedMap {
      outer: None,
      map: HashMap::new(),
    })
  }

  /// Creates a new nested map (with outer map).
  pub fn new_with_outer(outer: NestedMapPtr<K, V>) -> NestedMapPtr<K, V> {
    Box::new(NestedMap {
      outer: Some(outer),
      map: HashMap::new(),
    })
  }

  /// Adds item to the current map,
  /// returns true if the operation takes effect.
  pub fn add(&mut self, k: K, v: V) -> bool {
    if !self.map.contains_key(&k) {
      self.map.insert(k, v);
      true
    } else {
      false
    }
  }

  /// Gets item by the specific key,
  /// returns `None` if key not found.
  pub fn get(&self, k: &K, recursive: bool) -> Option<&V> {
    if let Some(v) = self.map.get(k) {
      Some(v)
    } else if self.outer.is_some() && recursive {
      self.outer.as_ref().unwrap().get(k, recursive)
    } else {
      None
    }
  }

  /// Gets item recursively by the specific key,
  /// returns `None` if key not found.
  pub fn get_rec(&self, k: &K) -> Option<&V> {
    self.get(k, true)
  }

  /// Removes item by the specific key,
  /// returns true if the remove operation takes effect.
  pub fn remove(&mut self, k: &K, recursive: bool) -> bool {
    if self.map.remove(k).is_some() {
      true
    } else if self.outer.is_some() && recursive {
      self.outer.as_mut().unwrap().remove(k, recursive)
    } else {
      false
    }
  }

  /// Removes item recursively by the specific key,
  /// returns true if the remove operation takes effect.
  pub fn remove_rec(&mut self, k: &K) -> bool {
    self.remove(k, true)
  }

  /// Updates item by the specific key,
  /// returns true if the update operation takes effect.
  pub fn update(&mut self, k: &K, v: V, recursive: bool) -> bool {
    if let Some(val) = self.map.get_mut(k) {
      *val = v;
      true
    } else if self.outer.is_some() && recursive {
      self.outer.as_mut().unwrap().update(k, v, recursive)
    } else {
      false
    }
  }

  /// Updates item recursively by the specific key,
  /// returns true if the update operation takes effect.
  pub fn update_rec(&mut self, k: &K, v: V) -> bool {
    self.update(k, v, true)
  }

  /// Accesses item in the current map.
  pub fn access(&mut self, k: &K) -> Option<&mut V> {
    self.map.get_mut(k)
  }

  /// Unwrap & move the outer map.
  pub fn outer(&mut self) -> NestedMapPtr<K, V> {
    self.outer.unwrap()
  }

  /// Check if the current map is a root map.
  pub fn is_root(&self) -> bool {
    self.outer.is_none()
  }
}
