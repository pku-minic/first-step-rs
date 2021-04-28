/// Unwraps an enumerate.
#[macro_export]
macro_rules! unwrap_enum {
  ($v:expr, $t:path) => {
    if let $t(val) = $v {
      val
    } else {
      panic!("Failed to unwrap enum!")
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
