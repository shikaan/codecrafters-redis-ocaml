open Unix;;

let rec handle client_socket =
  let req = Bytes.create 1024 in
  let bytes = read client_socket req 0 (Bytes.length req) in
  if bytes > 0 then (
    let res = Bytes.of_string "+PONG\r\n" in
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
    let _ = Thread.create(fun () -> 
      handle client_socket;
      close client_socket;
    ) () in ()
  done;
  close server_socket
