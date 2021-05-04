use std::collections::HashMap;
use std::hash::Hash;

/// Nested hash map (for implementing environments).
pub struct NestedMap<K, V> {
  cur: NodePtr<K, V>,
}

/// Nested hash map node.
struct Node<K, V> {
  outer: NodePtr<K, V>,
  map: HashMap<K, V>,
}

/// Nullable pointer of nested hash maps.
type NodePtr<K, V> = Option<Box<Node<K, V>>>;

impl<K, V> NestedMap<K, V>
where
  K: Hash + Eq,
{
  /// Creates a new nested map
  pub fn new() -> Self {
    Self {
      cur: Some(Box::new(Node {
        outer: None,
        map: HashMap::new(),
      })),
    }
  }

  /// Creates and enters a new map.
  pub fn push(&mut self) {
    self.cur = Some(Box::new(Node {
      outer: self.cur.take(),
      map: HashMap::new(),
    }));
  }

  /// Exits from the current map.
  /// Panics when popping from root map.
  pub fn pop(&mut self) {
    self.cur = self.cur.as_mut().unwrap().outer.take();
    if self.cur.is_none() {
      panic!("popping from the root map")
    }
  }

  /// Adds item to the current map,
  /// returns true if the operation takes effect.
  pub fn add(&mut self, k: K, v: V) -> bool {
    self.cur.as_mut().unwrap().add(k, v)
  }

  /// Gets item by the specific key,
  /// returns `None` if key not found.
  pub fn get(&self, k: &K, recursive: bool) -> Option<&V> {
    self.cur.as_ref().unwrap().get(k, recursive)
  }

  /// Gets item recursively by the specific key,
  /// returns `None` if key not found.
  pub fn get_rec(&self, k: &K) -> Option<&V> {
    self.get(k, true)
  }

  /// Removes item by the specific key,
  /// returns true if the remove operation takes effect.
  pub fn remove(&mut self, k: &K, recursive: bool) -> bool {
    self.cur.as_mut().unwrap().remove(k, recursive)
  }

  /// Removes item recursively by the specific key,
  /// returns true if the remove operation takes effect.
  pub fn remove_rec(&mut self, k: &K) -> bool {
    self.remove(k, true)
  }

  /// Updates item by the specific key,
  /// returns true if the update operation takes effect.
  pub fn update(&mut self, k: &K, v: V, recursive: bool) -> bool {
    self.cur.as_mut().unwrap().update(k, v, recursive)
  }

  /// Updates item recursively by the specific key,
  /// returns true if the update operation takes effect.
  pub fn update_rec(&mut self, k: &K, v: V) -> bool {
    self.update(k, v, true)
  }

  /// Updates item recursively by the specific key,
  /// stops updating when the predicate returns false,
  /// returns true if the update operation takes effect.
  pub fn update_until<F>(&mut self, k: &K, v: V, predicate: F) -> bool
  where
    F: Fn(&HashMap<K, V>) -> bool,
  {
    self.cur.as_mut().unwrap().update_until(k, v, predicate)
  }

  /// Accesses item in the current map.
  pub fn access(&mut self, k: &K) -> Option<&mut V> {
    self.cur.as_mut().unwrap().map.get_mut(k)
  }

  /// Check if the current map is a root map
  pub fn is_root(&self) -> bool {
    self.cur.as_ref().unwrap().outer.is_none()
  }
}

impl<K, V> Node<K, V>
where
  K: Hash + Eq,
{
  /// Implementation of `add` method of `NestedMap`.
  fn add(&mut self, k: K, v: V) -> bool {
    if !self.map.contains_key(&k) {
      self.map.insert(k, v);
      true
    } else {
      false
    }
  }

  /// Implementation of `get` method of `NestedMap`.
  fn get(&self, k: &K, recursive: bool) -> Option<&V> {
    if let Some(v) = self.map.get(k) {
      Some(v)
    } else if self.outer.is_some() && recursive {
      self.outer.as_ref().unwrap().get(k, recursive)
    } else {
      None
    }
  }

  /// Implementation of `remove` method of `NestedMap`.
  fn remove(&mut self, k: &K, recursive: bool) -> bool {
    if self.map.remove(k).is_some() {
      true
    } else if self.outer.is_some() && recursive {
      self.outer.as_mut().unwrap().remove(k, recursive)
    } else {
      false
    }
  }

  /// Implementation of `update` method of `NestedMap`.
  fn update(&mut self, k: &K, v: V, recursive: bool) -> bool {
    if let Some(val) = self.map.get_mut(k) {
      *val = v;
      true
    } else if self.outer.is_some() && recursive {
      self.outer.as_mut().unwrap().update(k, v, recursive)
    } else {
      false
    }
  }

  /// Implementation of `update_until` method of `NestedMap`.
  fn update_until<F>(&mut self, k: &K, v: V, predicate: F) -> bool
  where
    F: Fn(&HashMap<K, V>) -> bool,
  {
    if let Some(val) = self.map.get_mut(k) {
      *val = v;
      true
    } else if self.outer.is_some() && !predicate(&self.map) {
      self.outer.as_mut().unwrap().update_until(k, v, predicate)
    } else {
      false
    }
  }
}

#[cfg(test)]
mod test {
  use super::NestedMap;

  #[test]
  fn test_nested() {
    let mut nested = NestedMap::new();
    assert!(nested.add("test1", 1));
    assert!(nested.add("test2", 2));
    assert_eq!(nested.get_rec(&"test1"), Some(&1));
    assert_eq!(nested.get_rec(&"test2"), Some(&2));
    assert_eq!(nested.get_rec(&"test3"), None);
    assert!(nested.is_root());
    nested.push();
    assert!(nested.add("test3", 3));
    assert!(nested.add("test1", 11));
    assert_eq!(nested.get_rec(&"test1"), Some(&11));
    assert_eq!(nested.get_rec(&"test2"), Some(&2));
    assert_eq!(nested.get_rec(&"test3"), Some(&3));
    assert!(!nested.is_root());
    assert!(nested.update(&"test3", 4, false));
    assert_eq!(nested.get_rec(&"test3"), Some(&4));
    assert!(!nested.add(&"test1", 12));
    assert!(!nested.remove(&"test2", false));
    assert!(nested.remove_rec(&"test2"));
    assert_eq!(nested.get_rec(&"test2"), None);
    nested.pop();
    assert_eq!(nested.get_rec(&"test3"), None);
  }
}
