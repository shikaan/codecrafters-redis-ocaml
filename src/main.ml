open Unix

type record = { value: string; expires_at: float; }
let memory: (string, record) Hashtbl.t = Hashtbl.create 16

let set key value opts =
  let set' k v e =
    ignore (Hashtbl.replace memory k { value = v; expires_at = e });
    RedisMessage.SimpleString "OK"
  in
  RedisMessage.(
    match opts with
    | BulkString opt :: [ BulkString value ] -> (
        match String.uppercase_ascii opt with
        | "EX" -> (
            match float_of_string_opt value with
            | Some ts -> set' key value (ts *. 1000.0)
            | None -> failwith "invalid unix timestamp")
        | "PX" -> (
            match float_of_string_opt value with
            | Some ts -> set' key value ts
            | None -> failwith "invalid unix timestamp")
        | _ -> failwith ("invalid option: " ^ opt))
    | [] -> set' key value max_float
    | _ -> failwith "malformed set command")

let get key =
  match Hashtbl.find_opt memory key with
  | None -> RedisMessage.NullBulkString
  | Some v ->
      if v.expires_at < Unix.time () then (
        Hashtbl.remove memory key;
        RedisMessage.NullBulkString)
      else RedisMessage.BulkString v.value

let rec handle client_socket =
  let req = Bytes.create 1024 in
  let bytes = read client_socket req 0 (Bytes.length req) in
  if bytes > 0 then (
    let res =
      RedisMessage.(to_buf
        (match of_bytes req with
        | Error e -> failwith e
        | Ok msg -> (
            match msg with
            | Array (BulkString cmd :: args) -> (
                match (String.uppercase_ascii cmd, args) with
                | "PING", [] -> SimpleString "PONG"
                | "ECHO", [ BulkString s ] -> BulkString s
                | "SET", [BulkString key; BulkString value] ->
                    set key value [] 
                | "SET", BulkString key :: BulkString value :: opts -> 
                    set key value opts
                | "GET", [BulkString key] -> get key
                | _ -> failwith ("error: " ^ String.of_bytes req))
            | _ -> failwith ("error: " ^ String.of_bytes req))))
    in
    ignore (write client_socket res 0 (Bytes.length res));
    handle client_socket)

let () =
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
