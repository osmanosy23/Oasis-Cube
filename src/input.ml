open Cube_rep
open Actions
open Graphics

let read () =
  while
    match read_key () with
    | 'q' ->
        close_graph ();
        false
    | c ->
        (match c with
        | 'r' ->
            turn_clock cube.(0);
            turn_clock_outer cube.(5) cube.(1) cube.(2) cube.(3);
            draw_2dcube cube.(0) 350 350;
            draw_2dcube cube.(2) 50 350;
            draw_2dcube cube.(5) 650 350;
            draw_2dcube cube.(4) 950 350;
            draw_2dcube cube.(1) 350 650;
            draw_2dcube cube.(3) 350 50
        | 'c' ->
            turn_counter cube.(0);
            draw_2dcube cube.(0) 350 350;
            turn_counter_outer cube.(5) cube.(1) cube.(2) cube.(3);
            draw_2dcube cube.(0) 350 350;
            draw_2dcube cube.(2) 50 350;
            draw_2dcube cube.(5) 650 350;
            draw_2dcube cube.(4) 950 350;
            draw_2dcube cube.(1) 350 650;
            draw_2dcube cube.(3) 350 50
        | _ -> ());
        true
  do
    ()
  done

let () = read ()