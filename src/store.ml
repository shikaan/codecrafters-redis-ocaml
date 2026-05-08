type record = { value : string; expires_at : Timestamp.t }

let memory : (string, record) Hashtbl.t = Hashtbl.create 16

let set ?expires_at k v  = 
  let e = Option.value ~default:Timestamp.never expires_at in
  ignore (Hashtbl.replace memory k { value = v; expires_at = e })

let get k =
  match Hashtbl.find_opt memory k with
  | None -> None
  | Some v ->
      if Timestamp.is_expired v.expires_at then (
        Hashtbl.remove memory k;
        None)
      else Some v.value

let keys () =
  Hashtbl.fold
    (fun k v acc ->
      if Timestamp.is_expired v.expires_at then (
        Hashtbl.remove memory k;
        acc)
      else k :: acc)
    memory []
