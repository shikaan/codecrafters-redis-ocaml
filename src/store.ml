let _values : (string, string) Hashtbl.t = Hashtbl.create 16
let _ttls : (string, Timestamp.t) Hashtbl.t = Hashtbl.create 16

let get_with_ttl_opt k =
  match (Hashtbl.find_opt _values k, Hashtbl.find_opt _ttls k) with
  | None, _ -> (None, None)
  | Some v, None -> (Some v, None)
  | Some v, Some exp ->
      if Timestamp.is_expired exp then (
        Hashtbl.remove _values k;
        Hashtbl.remove _ttls k;
        (None, None))
      else (Some v, Some exp)

let set ?expires_at k v =
  Hashtbl.replace _values k v;
  Option.iter (Hashtbl.replace _ttls k) expires_at

let get_opt k = match get_with_ttl_opt k with None, _ -> None | v, _ -> v
let ttl_opt k = match get_with_ttl_opt k with None, _ -> None | _, exp -> exp

let keys () =
  Hashtbl.fold
    (fun k v acc ->
      match Hashtbl.find_opt _ttls k with
      | None -> k :: acc
      | Some expires_at ->
          if Timestamp.is_expired expires_at then (
            Hashtbl.remove _values k;
            Hashtbl.remove _ttls k;
            acc)
          else k :: acc)
    _values []
