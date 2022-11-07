open Cube_rep
open Actions
open Graphics

let read_key () =
  while
    match read_key () with
    | 'q' ->
        close_graph ();
        false
    | c ->
        (match c with
        | 'u' ->
            u_turn cube;
            draw cube
        | 'i' ->
            u'_turn cube;
            draw cube
        | _ -> ());
        true
  do
    ()
  done

let () = read_key ()