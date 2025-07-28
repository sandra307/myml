
open Myml.Types
open Myml.Env
open Myml.Interpreter
let in_env =
 to_map [
    ("+", Simple (TFun (TInt, TFun (TInt, TInt))));
    ("-", Simple (TFun (TInt, TFun (TInt, TInt))));
    ("*",Simple (TFun (TInt, TFun (TInt, TInt))));
    ("=",Simple (TFun (TInt, TFun (TInt, TBool))));
    ("true", Simple TBool);
    ("false",Simple TBool);
  ]

let rec outloop () =
  print_string ">>> ";
  flush stdout;
  try
    match read_line () with
    | ":q" | ":Q" | ":quit" | "Quit" | "_q" | "_Q" ->
        print_endline "Exiting..."
    | ip ->
        (try
          let op = infer_and_eval in_env ip in
          print_endline op
        with Failure msg ->
          Printf.printf "Error: %s\n" msg);
        outloop ()
  with End_of_file ->
    print_endline "\nOk Bye!!"

let () = outloop ()
