open RedisMessage

let equals b s = Bytes.equal b (Bytes.of_string s)

let test_to_bytes () =
  assert (equals (to_bytes (SimpleString "OK")) "+OK\r\n");
  assert (equals (to_bytes (SimpleString "")) "+\r\n");
  assert (equals (to_bytes (BulkString "Hello")) "$5\r\nHello\r\n");
  assert (equals (to_bytes (BulkString "")) "$0\r\n\r\n");
  assert (equals (to_bytes NullBulkString) "$-1\r\n");
  assert (equals (to_bytes (Integer 42)) ":42\r\n");
  assert (equals (to_bytes (Integer 0)) ":0\r\n");
  assert (equals (to_bytes (Integer (-1))) ":-1\r\n");
  assert (equals (to_bytes (SimpleError (Generic, "msg"))) "-ERR msg\r\n");
  assert (equals (to_bytes (Array [])) "*0\r\n");
  assert (equals (to_bytes (Array [ SimpleString "1" ])) "*1\r\n+1\r\n");
  assert (
    equals
      (to_bytes (Array [ BulkString "foo"; Integer 1 ]))
      "*2\r\n$3\r\nfoo\r\n:1\r\n");
  assert (
    equals
      (to_bytes (Array [ Array [ SimpleString "a" ] ]))
      "*1\r\n*1\r\n+a\r\n")

let test_of_bytes_ok () =
  assert (of_bytes (Bytes.of_string "+OK\r\n") = Ok (SimpleString "OK"));
  assert (of_bytes (Bytes.of_string "+\r\n") = Ok (SimpleString ""));
  assert (of_bytes (Bytes.of_string "$5\r\nHello\r\n") = Ok (BulkString "Hello"));
  assert (of_bytes (Bytes.of_string "$0\r\n\r\n") = Ok (BulkString ""));
  assert (of_bytes (Bytes.of_string "$-1\r\n") = Ok NullBulkString);
  assert (of_bytes (Bytes.of_string ":42\r\n") = Ok (Integer 42));
  assert (of_bytes (Bytes.of_string ":0\r\n") = Ok (Integer 0));
  assert (of_bytes (Bytes.of_string ":-1\r\n") = Ok (Integer (-1)));
  assert (of_bytes (Bytes.of_string "*0\r\n") = Ok (Array []));
  assert (
    of_bytes (Bytes.of_string "*1\r\n+OK\r\n")
    = Ok (Array [ SimpleString "OK" ]));
  assert (
    of_bytes (Bytes.of_string "*2\r\n$3\r\nfoo\r\n:1\r\n")
    = Ok (Array [ BulkString "foo"; Integer 1 ]));
  assert (
    of_bytes (Bytes.of_string "*1\r\n*1\r\n+a\r\n")
    = Ok (Array [ Array [ SimpleString "a" ] ]))

let test_of_bytes_error () =
  assert (of_bytes Bytes.empty = Error "empty message");
  assert (
    of_bytes (Bytes.of_string "?unknown\r\n") = Error "unexpected message type");
  assert (
    of_bytes (Bytes.of_string "$5\r\nHi\r\n") = Error "unexpected string length");
  assert (of_bytes (Bytes.of_string "$5\r\n") = Error "empty bulk string");
  assert (
    of_bytes (Bytes.of_string "$abc\r\n") = Error "unable to parse string len");
  assert (
    of_bytes (Bytes.of_string "*abc\r\n") = Error "unable to parse array length");
  assert (
    of_bytes (Bytes.of_string "*-1\r\n")
    = Error "array length cannot be negative");
  assert (
    of_bytes (Bytes.of_string ":abc\r\n") = Error "unable to parse integer")

let test_roundtrip () =
  let roundtrip x = of_bytes (to_bytes x) = Ok x in
  assert (roundtrip (SimpleString "hello"));
  assert (roundtrip (BulkString "world"));
  assert (roundtrip (BulkString ""));
  assert (roundtrip NullBulkString);
  assert (roundtrip (Integer 99));
  assert (roundtrip (Integer (-1)));
  assert (roundtrip (Array []));
  assert (roundtrip (Array [ SimpleString "a"; BulkString "b"; Integer 1 ]));
  assert (roundtrip (Array [ Array [ SimpleString "nested" ] ]))

let () =
  test_to_bytes ();
  test_of_bytes_ok ();
  test_of_bytes_error ();
  test_roundtrip ()
