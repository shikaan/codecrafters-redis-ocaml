open Unix

type t = Milliseconds of int64 | Seconds of int32

let now_s () = (Int32.of_float (Unix.gettimeofday ()))
let now () = (Int64.of_float (Unix.gettimeofday () *. 1000.0))

let add_s (s: int32) = Seconds (Int32.add (now_s ()) s)
let add (ms: int64) = Milliseconds (Int64.add (now ()) ms)
let is_expired = function
  | Seconds s -> s < now_s ()  
  | Milliseconds ms -> ms < now ()  
