pub opaque type Instant {
  Instant(nanos: Int)
}

pub fn now() -> Instant {
  Instant(monotonic_time())
}

pub type TimeUnit {
  Nanosecond
  Microsecond
  Millisecond
  Second
}

pub fn elapsed(instant: Instant, unit: TimeUnit) -> Int {
  let elapsed = now().nanos - instant.nanos

  case unit {
    Nanosecond -> elapsed
    Microsecond -> elapsed / 1000
    Millisecond -> elapsed / 1_000_000
    Second -> elapsed / 1_000_000_000
  }
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time() -> Int
