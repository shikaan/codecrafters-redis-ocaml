open Unix

type transaction = {
  mutable started : bool;
  mutable queue : (string * RedisMessage.t list) Queue.t;
}

let set key value opts =
  let set' ?e k v =
    ignore (Store.set k v ?expires_at:e);
    RedisMessage.SimpleString "OK"
  in
  RedisMessage.(
    match opts with
    | BulkString opt :: [ BulkString optval ] -> (
        match String.uppercase_ascii opt with
        | "EX" -> (
            match Int32.of_string_opt optval with
            | Some ts -> set' key value ~e:(Timestamp.add_s ts)
            | None ->
                SimpleError (Generic, "value is not an integer or out of range")
            )
        | "PX" -> (
            match Int64.of_string_opt optval with
            | Some ts -> set' key value ~e:(Timestamp.add ts)
            | None ->
                SimpleError (Generic, "value is not an integer or out of range")
            )
        | _ -> SimpleError (Generic, "syntax error"))
    | [] -> set' key value
    | _ -> SimpleError (Generic, "wrong number of arguments for 'set' command"))

let save (conf : Config.t) =
  match RDB.Encoding.encode () with
  | Ok bytes ->
      if not (Sys.file_exists conf.dir) then Unix.mkdir conf.dir 0o755;
      let c = open_out_bin (Config.path conf) in
      output_bytes c bytes;
      close_out c;
      RedisMessage.SimpleString "OK"
  | Error e -> RedisMessage.SimpleError (Generic, e)

let get key =
  RedisMessage.(
    match Store.get_opt key with
    | None -> NullBulkString
    | Some v -> BulkString v)

let incr key =
  let set' k v =
    ignore (Store.set k (string_of_int v));
    RedisMessage.Integer v
  in
  match Store.get_opt key with
  | None -> set' key 1
  | Some v -> (
      match int_of_string_opt v with
      | None -> SimpleError (Generic, "value is not an integer or out of range")
      | Some n -> set' key (n + 1))

let keys pattern =
  match pattern with
  | "*" ->
      RedisMessage.Array
        (List.map (fun s -> RedisMessage.BulkString s) (Store.keys ()))
  | _ -> RedisMessage.SimpleError (Generic, "not implemented")

let config (conf : Config.t) opts =
  RedisMessage.(
    match opts with
    | [ BulkString "GET"; BulkString key ] -> (
        match String.lowercase_ascii key with
        | "dir" -> Array [ BulkString "dir"; BulkString conf.dir ]
        | "dbfilename" ->
            Array [ BulkString "dbfilename"; BulkString conf.dbfilename ]
        | _ -> SimpleError (Generic, ""))
    | _ -> SimpleError (Generic, ""))

let queue_cmd tx invocation =
  ignore (Queue.push invocation tx.queue);
  RedisMessage.SimpleString "QUEUED"

let handle_cmd conf (cmd, args) =
  RedisMessage.(
    match (cmd, args) with
    | "PING", [] -> SimpleString "PONG"
    | "ECHO", [ BulkString s ] -> BulkString s
    | "SET", [ BulkString key; BulkString value ] -> set key value []
    | "SET", BulkString key :: BulkString value :: opts -> set key value opts
    | "INCR", [ BulkString key ] -> incr key
    | "GET", [ BulkString key ] -> get key
    | "KEYS", [ BulkString pattern ] -> keys pattern
    | "CONFIG", opts -> config conf opts
    | "SAVE", [] -> save conf
    | _ ->
        SimpleError
          ( Generic,
            "unknown command '" ^ cmd ^ "'"
            ^
            match args with
            | head :: rest ->
                " with args beginning with " ^ RedisMessage.show head
            | _ -> "" ))

let multi tx =
  if tx.started then
    RedisMessage.SimpleError (Generic, "MULTI calls cannot be nested")
  else (
    tx.started <- true;
    RedisMessage.SimpleString "OK")

let exec conf tx =
  if tx.started then (
    let results =
      Queue.to_seq tx.queue |> Seq.map (handle_cmd conf) |> List.of_seq
    in
    tx.started <- false;
    tx.queue <- Queue.create ();
    RedisMessage.Array results)
  else RedisMessage.SimpleError (Generic, "EXEC without MULTI")

let discard tx =
  if tx.started then (
    tx.started <- false;
    tx.queue <- Queue.create ();
    RedisMessage.SimpleString "OK")
  else RedisMessage.SimpleError (Generic, "DISCARD without MULTI")

let rec handle conf client_socket tx =
  let req = Bytes.create 1024 in
  let bytes = read client_socket req 0 (Bytes.length req) in
  if bytes > 0 then (
    let res =
      RedisMessage.(
        to_bytes
          (match of_bytes req with
          | Error e -> failwith e
          | Ok msg -> (
              RedisMessage.(
                match msg with
                | Array (BulkString cmd :: args) -> (
                    match (String.uppercase_ascii cmd, args) with
                    | "EXEC", [] -> exec conf tx
                    | "MULTI", [] -> multi tx
                    | "DISCARD", [] -> discard tx
                    | command ->
                        if tx.started then queue_cmd tx command
                        else handle_cmd conf command)
                | _ -> SimpleError (Generic, "syntax error")))))
    in
    ignore (write client_socket res 0 (Bytes.length res));
    handle conf client_socket tx)

let () =
  let conf = Config.of_args () in
  if Sys.file_exists (Config.path conf) then (
    Log.info "Reading database at %s..." (Config.path conf);
    let ic = open_in_bin (Config.path conf) in
    let bytes = In_channel.input_all ic |> Bytes.of_string in
    close_in ic;
    match RDB.Decoding.decode bytes with
    | Ok _ -> Log.info "Reading database at %s... OK" (Config.path conf)
    | Error e ->
        Log.error "%s" e;
        exit 1)
  else Log.info "No database found at %s. Creating new file." (Config.path conf);
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;
  Log.info "Accepting connections at 127.0.0.1:6379";

  while true do
    let client_socket, _ = accept server_socket in
    let _ =
      Thread.create
        (fun () ->
          let tx = { started = false; queue = Queue.create () } in
          handle conf client_socket tx;
          close client_socket)
        ()
    in
    ()
  done;
  close server_socket
