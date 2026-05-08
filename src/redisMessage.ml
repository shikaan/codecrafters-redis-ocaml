type error = Generic

let show_error = function Generic -> "ERR"

type t =
  | BulkString of string
  | SimpleString of string
  | Array of t list
  | NullBulkString
  | Integer of int
  | SimpleError of error * string

let rec show_dbg = function
  | BulkString s -> "BulkString: " ^ s
  | SimpleString s -> "SimpleString: " ^ s
  | NullBulkString -> "NullBulkString"
  | Integer n -> "Integer: " ^ string_of_int n
  | SimpleError (k, s) -> "SimpleError: " ^ show_error k ^ " " ^ s
  | Array a -> "Array: [" ^ String.concat "; " (List.map show_dbg a) ^ "]"

let rec show = function
  | BulkString s -> s
  | SimpleString s -> s
  | NullBulkString -> "(null)"
  | Integer n -> string_of_int n
  | SimpleError (k, s) -> "(error) " ^ show_error k ^ " " ^ s
  | Array a -> "[" ^ String.concat ", " (List.map show a) ^ "]"

let rec to_buf ?buf v =
  let buf = Option.fold ~none:(Buffer.create 64) ~some:Fun.id buf in
  (match v with
  | BulkString s -> Printf.bprintf buf "$%d\r\n%s\r\n" (String.length s) s
  | NullBulkString -> Printf.bprintf buf "$-1\r\n"
  | SimpleString s -> Printf.bprintf buf "+%s\r\n" s
  | SimpleError (k, s) -> Printf.bprintf buf "-%s %s\r\n" (show_error k) s
  | Integer n -> Printf.bprintf buf ":%d\r\n" n
  | Array a ->
      Printf.bprintf buf "*%d\r\n" (List.length a);
      List.iter (fun x -> ignore (to_buf ~buf x)) a);
  Buffer.to_bytes buf

let of_bytes bytes =
  let len = Bytes.length bytes in
  if len <= 0 then Error "empty message"
  else
    let read_line bytes offset =
      match Bytes.index_from_opt bytes offset '\n' with
      | None -> None
      | Some lf ->
          let line_len = lf - offset - 1 in
          let line = Bytes.sub bytes offset line_len in
          Some (line, line_len, lf + 1)
    in
    let rec parse offset =
      match read_line bytes offset with
      | None -> Error "empty payload"
      | Some (rawline, rawline_len, next) -> (
          let line = String.of_bytes (Bytes.sub rawline 1 (rawline_len - 1)) in
          match Bytes.get rawline 0 with
          | '+' -> Ok (SimpleString line, next)
          | '$' -> (
              match int_of_string_opt line with
              | None -> Error "unable to parse string len"
              | Some 0 -> Ok (BulkString "", next)
              | Some -1 -> Ok (NullBulkString, next)
              | Some len when len < 0 -> Error "unexpected string length"
              | Some len -> (
                  match read_line bytes next with
                  | None -> Error "empty bulk string"
                  | Some (rawline, rawline_len, next) ->
                      if rawline_len <> len then
                        Error "unexpected string length"
                      else
                        let line =
                          String.of_bytes (Bytes.sub rawline 0 rawline_len)
                        in
                        Ok (BulkString line, next)))
          | '*' -> (
              match int_of_string_opt line with
              | None -> Error "unable to parse array length"
              | Some 0 -> Ok (Array [], next)
              | Some len when len < 0 -> Error "array length cannot be negative"
              | Some len ->
                  let rec loop n list offset =
                    if n = 0 then Ok (Array list, offset)
                    else
                      match parse offset with
                      | Error e -> Error e
                      | Ok (msg, next) -> loop (n - 1) (list @ [ msg ]) next
                  in
                  loop len [] next)
          | ':' -> (
              match int_of_string_opt line with
              | None -> Error "unable to parse integer"
              | Some n -> Ok (Integer n, next))
          | _ -> Error "unexpected message type")
    in
    Result.map fst (parse 0)
