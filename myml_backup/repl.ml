open Interpreter
open Env
open Types

let initial_env =
  of_list [
    ("+", Simple (TFun (TInt, TFun (TInt, TInt))));
    ("-", Simple (TFun (TInt, TFun (TInt, TInt))));
    ("*", Simple (TFun (TInt, TFun (TInt, TInt))));
    ("=", Simple (TFun (TInt, TFun (TInt, TBool))));
    ("true", Simple TBool);
    ("false", Simple TBool);
  ]

let rec loop () =
  print_string ">>> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> print_endline "\nGoodbye!"
  | input ->
      if input = ":quit" || input = ":q" then
        print_endline "Exiting..."
      else
        (try
           let output = infer_and_eval initial_env  input in
           print_endline output
         with Failure msg ->
           Printf.printf "Error: %s\n" msg);
      loop ()

let main () = loop ()

