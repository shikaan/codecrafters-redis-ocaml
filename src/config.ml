type t = { dir : string; dbfilename : string }
let dir = ref ""
let dbfilename = ref ""
let spec = [
  ("--dir", Arg.Set_string dir, " Directory");
  ("--dbfilename", Arg.Set_string dbfilename, " Database File Name");
]

let of_args (): t = 
  Arg.parse spec (fun _ -> ()) "usage: redis [options]";
  { dir = !dir; dbfilename = !dbfilename } 
