open Cube_rep
open Actions
open Graphics

let cube = ref cube_rep
let counter = ref ~-1
(* let next =
       fun () ->
           counter := !counter + 1;
           !counter
   let count_text () =
   set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
   Graphics.draw_string("Counter: " ^ string_of_int(next())) *)

let next () =
  counter := !counter + 1;
  !counter

let ref_c = ref "Counter: "

let draw_count count =
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string !count

(* let count = ref draw_count *)
let () = draw_count ref_c
let hap () = ref (Graphics.draw_string (string_of_int (next ()) ^ " "))

let read_key () =
  while
    !(hap ());
    match read_key () with
    | 'q' ->
        close_graph ();
        false
    | c ->
        (match c with
        | 'u' ->
            u_turn !cube;
            draw !cube
        | 'U' ->
            u'_turn !cube;
            draw !cube
        | 'd' ->
            d_turn !cube;
            draw !cube
        | 'D' ->
            d'_turn !cube;
            draw !cube
        | 'b' ->
            b_turn !cube;
            draw !cube
        | 'B' ->
            b'_turn !cube;
            draw !cube
        | 'f' ->
            f_turn !cube;
            draw !cube
        | 'F' ->
            f'_turn !cube;
            draw !cube
        | 'r' ->
            r_turn !cube;
            draw !cube
        | 'R' ->
            r'_turn !cube;
            draw !cube
        | 'l' ->
            l_turn !cube;
            draw !cube
        | 'L' ->
            l'_turn !cube;
            draw !cube
        | 'm' ->
            m_turn !cube;
            draw !cube
        | 'M' ->
            m'_turn !cube;
            draw !cube
        | 'e' ->
            e_turn !cube;
            draw !cube
        | 'E' ->
            e'_turn !cube;
            draw !cube
        | 's' ->
            s_turn !cube;
            draw !cube
        | 'S' ->
            s'_turn !cube;
            draw !cube
        | 'y' ->
            y_rotate !cube;
            draw !cube
        | 'Y' ->
            y'_rotate !cube;
            draw !cube
        | 'x' ->
            x_rotate !cube;
            draw !cube
        | 'X' ->
            x'_rotate !cube;
            draw !cube
        | 'z' ->
            z_rotate !cube;
            draw !cube
        | 'Z' ->
            z'_rotate !cube;
            draw !cube
        | '.' ->
            cube := solve ();
            draw !cube
        | '\\' ->
            randomize !cube 100;
            draw !cube
        | '1' ->
            randomize !cube 1;
            draw !cube
        | '2' ->
            randomize !cube 2;
            draw !cube
        | '3' ->
            randomize !cube 3;
            draw !cube
        | '4' ->
            randomize !cube 4;
            draw !cube
        | '5' ->
            randomize !cube 5;
            draw !cube
        | '6' ->
            randomize !cube 6;
            draw !cube
        | '7' ->
            randomize !cube 7;
            draw !cube
        | '8' ->
            randomize !cube 8;
            draw !cube
        | '9' ->
            randomize !cube 9;
            draw !cube
        | _ -> ());

        true
  do
    ()
  done

let () = read_key ()
