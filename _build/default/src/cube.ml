(* let cube_rep_r =
  [| [| [| "R"; "R"; "R" |]; [| "R"; "R"; "R" |]; [| "R"; "R"; "R" |] |] |]

let () = cube_rep_r |> Array.iter (Array.iter print_endline) *)
#use "topfind";;
#require "graphics";;
open Graphics;;

open_graph " 700x700";;
set_window_title "Rubik's Cube Simulator";;
auto_synchronize false;;