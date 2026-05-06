type t = BulkString of string | SimpleString of string | Array of t list |
NullBulkString

let rec show = function
  | BulkString s -> "BulkString: " ^ s
  | SimpleString s -> "SimpleString: " ^ s
  | NullBulkString -> "NullBulkString"
  | Array a -> "Array: [" ^ String.concat "; " (List.map show a) ^ "]"

let rec to_buf ?buf v =
  let buf = Option.fold ~none:(Buffer.create 64) ~some:Fun.id buf in
  (match v with
  | BulkString s -> Printf.bprintf buf "$%d\r\n%s\r\n" (String.length s) s
  | NullBulkString -> Printf.bprintf buf "$-1\r\n"
  | SimpleString s -> Printf.bprintf buf "+%s\r\n" s
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
          let line =
            String.of_bytes (Bytes.sub rawline 1 (rawline_len - 1))
          in
          match Bytes.get rawline 0 with
          | '+' -> Ok (SimpleString line, next)
          | '$' -> (
              match int_of_string line with
              | 0 -> Ok (BulkString "", next)
              | -1 -> Ok (NullBulkString, next)
              | len when len < 0 -> Error "unexpected string length"
              | len -> (
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
              match int_of_string line with
              | 0 -> Ok (Array [], next)
              | len when len < 0 -> Error "unexpected array length"
              | len ->
                  let rec loop n list offset =
                    if n = 0 then Ok (Array list, offset)
                    else
                      match parse offset with
                      | Error e -> Error e
                      | Ok (msg, next) -> loop (n - 1) (list @ [ msg ]) next
                  in
                  loop len [] next)
          | _ -> Error "unexpected message type")
    in
    Result.map fst (parse 0)
