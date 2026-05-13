module T = Timestamp.Make (struct
  let gettimeofday () = 1000.0
end)

let test_is_expired () =
  let now = T.now () in
  let nows = T.now_s () in
  assert (T.is_expired (Milliseconds 0L));
  assert (T.is_expired (Seconds 0l));
  assert (not (T.is_expired (Milliseconds Int64.max_int)));
  assert (not (T.is_expired (Seconds Int32.max_int)));
  assert (not (T.is_expired (Milliseconds now)));
  assert (not (T.is_expired (Seconds nows)));
  assert (T.is_expired (Milliseconds (Int64.sub now 1L)));
  assert (T.is_expired (Seconds (Int32.sub nows 1l)))

let test_add () =
  let now = T.now () in
  assert (T.add 0L = Milliseconds now);
  assert (T.add 100L = Milliseconds (Int64.add now 100L))

let test_add_s () =
  let now = T.now_s () in
  assert (T.add_s 0l = Seconds now);
  assert (T.add_s 100l = Seconds (Int32.add now 100l))

let () =
  test_is_expired ();
  test_add ();
  test_add_s ()
