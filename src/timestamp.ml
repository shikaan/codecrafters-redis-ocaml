open Unix

type t = float

let never = max_float
let now _ = Unix.gettimeofday () *. 1000.0
let add_s s = now () +. (s *. 1000.0)
let add ms = now () +. ms
let is_expired ts = ts <> never && ts < now ()
