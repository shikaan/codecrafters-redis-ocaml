let ( let* ) = Result.bind
let magic = Bytes.of_string "REDIS"
let metadata_byte = Bytes.of_string "\xfa"
let info_byte = Bytes.of_string "\xfb"
let milliseconds_byte = Bytes.of_string "\xfc"
let seconds_byte = Bytes.of_string "\xfd"
let database_byte = Bytes.of_string "\xfe"
let footer_byte = Bytes.of_string "\xff"
let string_byte = Bytes.of_string "\x00"
let bytes_of_bytes = Bytes.concat Bytes.empty

module Encoding : sig
  val encode : unit -> (bytes, string) result
  (** Encodes the current in-memory store into an RDB byte stream. *)
end = struct
  let len n =
    match n with
    | n when n < 64 ->
        let buf = Bytes.create 1 in
        Bytes.set_uint8 buf 0 n;
        Ok buf
    | n when n < 16384 ->
        let buf = Bytes.create 2 in
        Bytes.set_uint16_le buf 0 (0b01000000_00000000 lor n);
        Ok buf
    | n when n < 4294967296 ->
        let buf = Bytes.create 5 in
        Bytes.set_uint8 buf 0 0b10_000000;
        Bytes.set_int32_le buf 1 (Int32.of_int n);
        Ok buf
    | _ -> Error ("unsupported size: " ^ string_of_int n)

  let str s =
    match len (String.length s) with
    | Ok encoded -> Ok Bytes.(of_string s |> cat encoded)
    | Error e -> Error e

  let header _ = Ok (bytes_of_bytes [ magic; Bytes.of_string "0011" ])

  let metadata fields =
    let subsection (k, v) =
      let* key = str k in
      let* value = str v in
      Ok (bytes_of_bytes [ metadata_byte; key; value ])
    in
    List.fold_left
      (fun acc f ->
        match acc with
        | Error e -> Error e
        | Ok bytes -> (
            match subsection f with
            | Ok field -> Ok (Bytes.cat bytes field)
            | Error e -> Error e))
      (Ok Bytes.empty) fields

  let database idx keys =
    let nexpirations = ref 0 in
    let field' k =
      let field'' k v =
        let* key = str k in
        let* value = str v in
        Ok (bytes_of_bytes [ string_byte; key; value ])
      in
      let exp'' = function
        | Timestamp.Seconds s ->
            let buf = Bytes.create 5 in
            Bytes.set buf 0 (Bytes.get seconds_byte 0);
            Bytes.set_int32_le buf 1 s;
            Ok buf
        | Timestamp.Milliseconds ms ->
            let buf = Bytes.create 9 in
            Bytes.set buf 0 (Bytes.get milliseconds_byte 0);
            Bytes.set_int64_le buf 1 ms;
            Ok buf
      in
      match Store.get_with_ttl_opt k with
      | Some v, None -> field'' k v
      | Some v, Some exp ->
          incr nexpirations;
          let* e = exp'' exp in
          let* f = field'' k v in
          Ok (Bytes.cat e f)
      | _ -> Error "cannot encode field"
    in
    let records' =
      List.fold_left
        (fun acc' k' ->
          let* acc = acc' in
          let* field = field' k' in
          Ok (Bytes.cat acc field))
        (Ok Bytes.empty) keys
    in
    let* dbidx = len idx in
    let* nvalues = len (List.length keys) in
    let* nexpirations = len !nexpirations in
    let* records = records' in
    Ok
      (bytes_of_bytes
         [ database_byte; dbidx; info_byte; nvalues; nexpirations; records ])

  let footer _ =
    let z = Bytes.of_string "\x00" in
    Ok (bytes_of_bytes [ footer_byte; z; z; z; z; z; z; z; z ])

  let encode () =
    List.fold_left
      (fun acc r ->
        match (acc, r) with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok a, Ok b -> Ok (Bytes.cat a b))
      (Ok Bytes.empty)
      [
        header ();
        metadata [ ("redis-ver", "8.6.3") ];
        database 0 (Store.keys ());
        footer ();
      ]
end

module Decoding : sig
  val decode : bytes -> (unit, string) result
  (** Parses an RDB byte stream and loads its key/value pairs in memory. *)
end = struct
  type cursor = { mutable pos : int; mutable stream : Bytes.t }
  type length = Length of int | Value of int

  (** reads n bytes from the stream, without advancing the position *)
  let peek c = function
    | n when c.pos + n <= Bytes.length c.stream ->
        Ok (Bytes.sub c.stream c.pos n)
    | _ -> Error "unexpected EOF"

  (** consumes n characters from the stream *)
  let read c n =
    let* buf = peek c n in
    c.pos <- c.pos + n;
    Ok buf

  (** decodes bytes as length *)
  let len c =
    let* byte = peek c 1 in
    match Bytes.get_uint8 byte 0 land 0b11_000000 with
    | 0 ->
        let n = Bytes.get_uint8 byte 0 land 0b00_111111 in
        c.pos <- c.pos + 1;
        Ok (Length n)
    | 0b01_000000 ->
        let n = Bytes.get_uint16_be c.stream c.pos land 0x3FFF in
        c.pos <- c.pos + 2;
        Ok (Length n)
    | 0b10_000000 ->
        let n = Bytes.get_int32_le c.stream (c.pos + 1) in
        c.pos <- c.pos + 5;
        Ok (Length (Int32.to_int n))
    | 0b11_000000 -> (
        c.pos <- c.pos + 1;
        match Bytes.get_uint8 byte 0 land 0b00_111111 with
        | 0 ->
            let n = Bytes.get_uint8 c.stream c.pos in
            c.pos <- c.pos + 1;
            Ok (Value n)
        | 1 ->
            let n = Bytes.get_uint16_le c.stream c.pos in
            c.pos <- c.pos + 2;
            Ok (Value n)
        | 2 ->
            let n = Bytes.get_int32_le c.stream c.pos in
            c.pos <- c.pos + 4;
            Ok (Value (Int32.to_int n))
        | _ -> Error "unexpected special encoding")
    | _ -> Error "unexpected length"

  let unwrap = function Length i -> i | Value i -> i

  (** decodes bytes as string *)
  let str c =
    match len c with
    | Ok (Length nint) ->
        if c.pos + nint > Bytes.length c.stream then
          Error
            (Printf.sprintf "out of bounds: max %d, got %d"
               (Bytes.length c.stream) (c.pos + nint))
        else
          let s = Bytes.sub_string c.stream c.pos nint in
          c.pos <- c.pos + nint;
          Ok s
    | Ok (Value i) -> Ok (string_of_int i)
    | Error e -> Error e

  let decode bytes =
    let c = { pos = 0; stream = bytes } in

    let validate_header' () =
      let lhdr = Bytes.length magic in
      let* magic' = read c lhdr in
      let* version' =
        let* version = read c 4 in
        let sversion = String.of_bytes version in
        Ok (int_of_string sversion)
      in
      if Bytes.equal magic' magic && version' >= 11 then (
        Log.info "Version: %d" version';
        Ok ())
      else Error "unexpected header"
    in

    let validate_metadata' () =
      let rec loop () =
        let* next = peek c 1 in
        if Bytes.equal next metadata_byte then (
          c.pos <- c.pos + 1;
          let* key = str c in
          let* value = str c in
          Log.info "  - %s: %s" key value;
          loop ())
        else Ok ()
      in
      Log.info "Metadata:";
      loop ()
    in

    let read_data' () =
      let* database = read c 1 in
      if Bytes.equal database database_byte then (
        let* index = read c 1 in
        let* info = read c 1 in
        if not (Bytes.equal info info_byte) then Error "missing info byte"
        else
          let* values_size = len c in
          let* expirations_size = len c in
          Log.info "Found %d records of which %d have expirations."
            (unwrap values_size) (unwrap expirations_size);
          let rec loop _ =
            let str' ttl =
              let* key = str c in
              let* value = str c in
              Store.set key value ?expires_at:ttl;
              loop ()
            in
            let* next = peek c 1 in
            match next with
            | b when Bytes.equal b string_byte ->
                c.pos <- c.pos + 1;
                str' None
            | b when Bytes.equal b milliseconds_byte ->
                let ms = Bytes.get_int64_le c.stream (c.pos + 1) in
                c.pos <- c.pos + 10;
                str' (Some (Timestamp.Milliseconds ms))
            | b when Bytes.equal b seconds_byte ->
                let s = Bytes.get_int32_le c.stream (c.pos + 1) in
                c.pos <- c.pos + 6;
                str' (Some (Timestamp.Seconds s))
            | b when Bytes.equal b footer_byte -> Ok ()
            | _ -> Error "unexpected database record"
          in
          loop ())
      else Error "expected database byte"
    in
    Log.info "Decoding database...";
    let* _ = validate_header' () in
    let* _ = validate_metadata' () in
    let* _ = read_data' () in
    Log.info "Decoding database... OK";
    Ok ()
end
