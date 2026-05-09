let _values : (string, string) Hashtbl.t = Hashtbl.create 16
let _expirations : (string, Timestamp.t) Hashtbl.t = Hashtbl.create 16

let __get_exp_opt k =
  match (Hashtbl.find_opt _values k, Hashtbl.find_opt _expirations k) with
  | None, _ -> (None, None)
  | Some v, None -> (Some v, None)
  | Some v, Some exp ->
      if Timestamp.is_expired exp then (
        Hashtbl.remove _values k;
        Hashtbl.remove _expirations k;
        (None, None))
      else (Some v, Some exp)

let set ?expires_at k v =
  Hashtbl.replace _values k v;
  Option.iter (Hashtbl.replace _expirations k) expires_at

let get_opt k = match __get_exp_opt k with None, _ -> None | v, _ -> v

let keys () =
  Hashtbl.fold
    (fun k v acc ->
      match Hashtbl.find_opt _expirations k with
      | None -> k :: acc
      | Some expires_at ->
          if Timestamp.is_expired expires_at then (
            Hashtbl.remove _values k;
            Hashtbl.remove _expirations k;
            acc)
          else k :: acc)
    _values []

module Encoding = struct
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
        Bytes.set_uint8 buf 0 0b11;
        Bytes.set_int32_le buf 1 (Int32.of_int n);
        Ok buf
    | _ -> Error ("unsupported size: " ^ string_of_int n)

  let str s =
    match len (String.length s) with
    | Ok encoded -> Ok (Bytes.cat encoded (Bytes.of_string s))
    | Error e -> Error e

  let header _ = Ok (Bytes.of_string "REDIS0011")

  let metadata fields =
    let subsection (k, v) =
      match str k with
      | Ok key -> (
          match str v with
          | Ok value -> Ok (Bytes.cat key value)
          | Error e -> Error e)
      | Error e -> Error e
    in
    List.fold_left
      (fun acc f ->
        match acc with
        | Error e -> Error e
        | Ok bytes -> (
            match subsection f with
            | Ok field -> Ok (Bytes.cat bytes field)
            | Error e -> Error e))
      (Ok (Bytes.of_string "\xfa"))
      fields

  let database idx values expirations =
    let info =
      match (len (Hashtbl.length values), len (Hashtbl.length expirations)) with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok nvalues, Ok nexpirations ->
          Ok
            (Bytes.cat nvalues nexpirations
            |> Bytes.cat (Bytes.of_string "\xfb"))
    in
    let start =
      match len idx with
      | Error e -> Error e
      | Ok index -> Ok (Bytes.cat (Bytes.of_string "\xfe") index)
    in
    let field k =
      let field' k v =
        match (str k, str v) with
        | Ok key, Ok value ->
            Ok (Bytes.cat key value |> Bytes.cat (Bytes.of_string "\x00"))
        | Error e, _ -> Error e
        | _, Error e -> Error e
      in
      let exp' = function
        | Timestamp.Seconds s ->
            let buf = Bytes.create 5 in
            Bytes.set buf 0 '\xfd';
            Bytes.set_int32_le buf 1 s;
            Ok buf
        | Timestamp.Milliseconds ms ->
            let buf = Bytes.create 9 in
            Bytes.set buf 0 '\xfc';
            Bytes.set_int64_le buf 1 ms;
            Ok buf
      in
      match __get_exp_opt k with
      | Some v, None -> field' k v
      | Some v, Some exp -> (
          match (exp' exp, field' k v) with
          | Ok exp', Ok field' -> Ok (Bytes.cat exp' field')
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      | _ -> Error "cannot encode field"
    in
    match (start, info) with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok start', Ok info' ->
        Hashtbl.fold
          (fun k _ acc ->
            match (acc, field k) with
            | Error e, _ -> Error e
            | _, Error e -> Error e
            | Ok acc', Ok field' -> Ok (Bytes.cat acc' field'))
          values
          (Ok (Bytes.cat start' info'))

  let footer _ = Ok (Bytes.of_string "\xff\x00\x00\x00\x00\x00\x00\x00\x00")

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
        database 0 _values _expirations;
        footer ();
      ]
end
