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

(* let next =
    fun () ->
        counter := !counter + 1;
        !counter *)

let ref_c = ref "Counter: "
let nextref = ref counter

let draw_count count n =
  set_color white;
  counter := !counter + 1;
  draw_rect 0 0 100 100;
  set_color white;
  fill_rect 0 0 350 70;
  set_color black;
  Graphics.moveto 0 0;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string (!count ^ string_of_int !n);
  Graphics.moveto 0 0

(* let count = ref draw_count *)
(* let () = draw_count ref_c !(nextref) *)

(* let hap() = ref (Graphics.draw_string(string_of_int(!nextref()) ^ " ")) *)

let read_key () =
  while
    draw_count ref_c !nextref;
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
        | '1' ->
            randomize !cube 1;
            draw !cube
        | '\\' ->
            randomize !cube 100;
            draw !cube
        | ',' ->
            Graphics.set_color black;
            fill_rect 0 0 10000 10000;
            draw !cube
        | '<' ->
            Graphics.set_color white;
            fill_rect 0 0 10000 10000;
            draw !cube
        | '2' -> 
            Graphics.set_color white;
            fill_rect 0 0 10000 10000;
            draw2 !cube
        | '3' -> 
         Graphics.set_color white;
          fill_rect 0 0 10000 10000;
         draw !cube
        | _ -> ());

        true
  do
    ()
  done

let () = read_key ()
