type level = Debug | Info | Error

let current_level = ref Debug

let string_of_level = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Error -> "ERROR"

let level_value = function Debug -> 0 | Info -> 1 | Error -> 2

let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let log level fmt =
  Printf.ksprintf
    (fun s ->
      if level_value level >= level_value !current_level then
        Printf.eprintf "%s [%s] %s\n%!" (timestamp ()) (string_of_level level) s)
    fmt

let debug fmt = log Debug fmt
let info fmt = log Info fmt
let error fmt = log Error fmt
