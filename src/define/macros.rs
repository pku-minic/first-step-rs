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

/// Unwraps an result, or returns error.
#[macro_export]
macro_rules! ok_or_return {
  ($e:expr) => {
    match $e {
      Ok(ok) => ok,
      Err(err) => return Err(err),
    }
  };
}
