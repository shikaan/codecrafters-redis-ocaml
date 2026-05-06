open Unix

module RedisMessage = struct
  type t = BulkString of string | SimpleString of string | Array of t list

  let rec show = function
    | BulkString s -> "BulkString: " ^ s
    | SimpleString s -> "SimpleString: " ^ s
    | Array a -> "Array: [" ^ (String.concat "; " (List.map show a)) ^ "]"

  let rec to_buf ?buf v =
    let buf = Option.fold ~none:(Buffer.create 64) ~some:Fun.id buf in
    (match v with
    | BulkString s -> Printf.bprintf buf "$%d\r\n%s\r\n" (String.length s) s
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
            let line =
              Bytes.uppercase_ascii (Bytes.sub bytes offset line_len)
            in
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
                        | Ok (msg, next) -> loop (n - 1) (list @ [msg]) next
                    in
                    loop len [] next)
            | _ -> Error "unexpected message type")
      in
      Result.map fst (parse 0)
end

let rec handle client_socket =
  let req = Bytes.create 1024 in
  let bytes = read client_socket req 0 (Bytes.length req) in
  if bytes > 0 then (
    let res =
      RedisMessage.to_buf
        (match RedisMessage.of_bytes req with
        | Error e -> failwith e
        | Ok msg -> (
          Printf.eprintf "msg: %s\n" (RedisMessage.show msg);
            match msg with
            | Array [ BulkString "PING" ] -> SimpleString "PONG"
            | Array [ BulkString "ECHO"; BulkString s ] -> BulkString s
            | _ -> failwith ("error: " ^ String.of_bytes req) ))
    in
    ignore (write client_socket res 0 (Bytes.length res));
    handle client_socket)

let () =
  Printf.eprintf "Logs from your program will appear here!\n";

  (* Create a TCP server socket *)
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;

  while true do
    let client_socket, _ = accept server_socket in
    let _ =
      Thread.create
        (fun () ->
          handle client_socket;
          close client_socket)
        ()
    in
    ()
  done;
  close server_socket
