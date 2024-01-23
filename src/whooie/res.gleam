import gleam/iterator.{ type Iterator, Next, Done }
import gleam/option.{ type Option, Some, None }

/// Return `True` if `res` is `Ok`.
pub fn is_ok(res: Result(a, e)) -> Bool {
  case res {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Return `True` if `res` is `Ok` and the wrapped value satisfies `f`.
pub fn is_ok_and(res: Result(a, e), f: fn(a) -> Bool) -> Bool {
  case res {
    Ok(a) -> f(a)
    Error(_) -> False
  }
}

/// Return `True` if `res` is `Error`.
pub fn is_err(res: Result(a, e)) -> Bool {
  case res {
    Ok(_) -> False
    Error(_) -> True
  }
}

/// Return `True` if `res` is `Error` and the wrapped value satisfies `f`.
pub fn is_err_and(res: Result(a, e), f: fn(e) -> Bool) -> Bool {
  case res {
    Ok(_) -> False
    Error(e) -> f(e)
  }
}

/// Return the wrapped `Ok` value as an `Option`.
pub fn ok(res: Result(a, e)) -> Option(a) {
  case res {
    Ok(a) -> Some(a)
    Error(_) -> None
  }
}

/// Return the wrapped `Error` value as an `Option`.
pub fn err(res: Result(a, e)) -> Option(e) {
  case res {
    Ok(_) -> None
    Error(e) -> Some(e)
  }
}

/// Return `res_b` if `res_a` is `Ok`, otherwise the `Error` value of `res_a`.
pub fn and(res_a: Result(a, e), res_b: Result(b, e)) -> Result(b, e) {
  case res_a {
    Ok(_) -> res_b
    Error(e) -> Error(e)
  }
}

/// If `res` is `Ok`, call `f` on the wrapped value, otherwise return the
/// original `Error`.
pub fn and_then(res: Result(a, e), f: fn(a) -> Result(b, e)) -> Result(b, e) {
  case res {
    Ok(a) -> f(a)
    Error(e) -> Error(e)
  }
}

/// Return `res_b` if `res_a` is `Error`, otherwise the `Ok` value of `res_a`.
pub fn or(res_a: Result(a, e), res_b: Result(a, f)) -> Result(a, f) {
  case res_a {
    Ok(a) -> Ok(a)
    Error(_) -> res_b
  }
}

/// If `res` is `Error`, call `f` on the wrapped value, otherwise return the
/// original `Error`.
pub fn or_else(res: Result(a, e), f: fn(e) -> Result(a, f)) -> Result(a, f) {
  case res {
    Ok(a) -> Ok(a)
    Error(e) -> f(e)
  }
}

/// If `res` is `Ok`, call `f` on the wrapped value and re-wrap the output as
/// `Ok`, otherwise return the original `Error`.
pub fn map(res: Result(a, e), f: fn(a) -> b) -> Result(b, e) {
  case res {
    Ok(a) -> Ok(f(a))
    Error(e) -> Error(e)
  }
}

/// If `res` is `Ok`, call `f` on the wrapped value and return the output,
/// otherwise return `default`.
pub fn map_or(res: Result(a, e), f: fn(a) -> b, default: b) -> b {
  case res {
    Ok(a) -> f(a)
    Error(_) -> default
  }
}

/// If `res` is `Ok`, call `f_ok` on the wrapped value and return the output.
/// If `res` is `Error`, call `f_error` instead.
pub fn map_or_else(
  res: Result(a, e),
  f_ok: fn(a) -> b,
  f_err: fn(e) -> b,
) -> b
{
  case res {
    Ok(a) -> f_ok(a)
    Error(e) -> f_err(e)
  }
}

/// If `res` is `Error`, call `f` on the wrapped value and re-wrap the output
/// as `Error`, otherwise return the original `Ok`.
pub fn map_err(res: Result(a, e), f: fn(e) -> f) -> Result(a, f) {
  case res {
    Ok(a) -> Ok(a)
    Error(e) -> Error(f(e))
  }
}

/// If `res` is `Ok`, return the wrapped value, otherwise return `default`.
pub fn unwrap_or(res: Result(a, e), default: a) -> a {
  case res {
    Ok(a) -> a
    Error(_) -> default
  }
}

/// If `res` is `Ok`, return the wrapped value, otherwise call `f` on the
/// wrapped `Error` value.
pub fn unwrap_or_else(res: Result(a, e), f: fn(e) -> a) -> a {
  case res {
    Ok(a) -> a
    Error(e) -> f(e)
  }
}

/// If `res` is `Ok`, return the wrapped value, otherwise panic.
pub fn unwrap(res: Result(a, e)) -> a {
  case res {
    Ok(a) -> a
    Error(_) -> panic as "called `unwrap` on an `Error` value"
  }
}

/// If `res` is `Ok`, return the wrapped value, otherwise panic with a custom
/// message.
pub fn expect(res: Result(a, e), msg: String) -> a {
  case res {
    Ok(a) -> a
    Error(_) -> panic as msg
  }
}

/// If `res` is `Err`, return the wrapped value, otherwise panic with a custom
/// message.
pub fn expect_err(res: Result(a, e), msg: String) -> e {
  case res {
    Ok(_) -> panic as msg
    Error(e) -> e
  }
}

/// If all items in `items` are `Ok`, return an `Ok` of all wrapped values,
/// otherwise the first `Error` encountered.
pub fn collect_list(items: List(Result(a, e))) -> Result(List(a), e) {
  case items {
    [] -> Ok([])
    [Error(e), ..] -> Error(e)
    [Ok(item), ..tail] -> {
      case collect_list(tail) {
        Error(e) -> Error(e)
        Ok(rec_res) -> Ok([item, ..rec_res])
      }
    }
  }
}

/// If all items in `items` are `Ok`, return an `Ok` of a `List` of all wrapped
/// values, otherwise the first `Error` encountered.
pub fn collect_list_of_iter(items: Iterator(Result(a, e)))
  -> Result(List(a), e)
{
  case iterator.step(items) {
    Done -> Ok([])
    Next(Error(e), _) -> Error(e)
    Next(Ok(item), rest) -> {
      case collect_list_of_iter(rest) {
        Error(e) -> Error(e)
        Ok(rec_res) -> Ok([item, ..rec_res])
      }
    }
  }
}

