/// Unwraps an enumeration.
#[macro_export]
macro_rules! unwrap_enum {
  ($v:expr, $p:path) => {
    if let $p(val) = $v {
      val
    } else {
      panic!("Failed to unwrap enum!")
    }
  };
}

/// Unwraps a structure.
#[macro_export]
macro_rules! unwrap_struct {
  ($v:expr, $p:path, $($t:tt),*) => {
    if let $p { $($t,)* } = $v {
      ($($t,)*)
    } else {
      panic!("Failed to unwrap struct!")
    }
  };
}

/// Creates a collection with the specific values.
///
/// See: https://stackoverflow.com/a/27582993
#[macro_export]
macro_rules! collection {
  // map-like
  ($($k:expr => $v:expr),* $(,)?) => {
    std::iter::Iterator::collect(std::array::IntoIter::new([$(($k, $v),)*]))
  };
  // set-like
  ($($v:expr),* $(,)?) => {
    std::iter::Iterator::collect(std::array::IntoIter::new([$($v,)*]))
  };
}
