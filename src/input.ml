open Cube_rep
open Actions
open Graphics
open Actions2

let cube = ref cube_rep
let cube2 = ref cube_rep2
let background_color = ref white
let is_3x3 = ref true
let purple = rgb 240 150 240
let lblue = rgb 157 190 250
let lred = rgb 255 150 150
let lgreen = rgb 146 250 146
let counter = ref 0
let ref_c = "Counter: "
let nextref = ref counter
let click = ref false

let draw_prime x y =
  set_color black;
  fill_rect x y 3 9

let draw_buttons color =
  set_color color;
  fill_rect 521 259 60 38;
  fill_rect 595 259 60 38;
  fill_rect 669 259 60 38;
  fill_rect 743 259 60 38;
  fill_rect 817 259 60 38;
  fill_rect 891 259 60 38;
  set_color lblue;
  fill_rect 521 207 60 38;
  fill_rect 595 207 60 38;
  fill_rect 669 207 60 38;
  fill_rect 743 207 60 38;
  fill_rect 817 207 60 38;
  fill_rect 891 207 60 38;
  set_color lgreen;
  fill_rect 669 103 134 38;
  set_color lred;
  fill_rect 620 155 232 38;
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--45-*-*-*-*-*-iso8859-1";
  moveto 543 255;
  draw_string "F";
  moveto 616 255;
  draw_string "R";
  moveto 690 255;
  draw_string "U";
  moveto 763 255;
  draw_string "B";
  moveto 839 255;
  draw_string "L";
  moveto 911 255;
  draw_string "D";
  moveto 537 203;
  draw_string "F";
  moveto 611 203;
  draw_string "R";
  moveto 685 203;
  draw_string "U";
  moveto 759 203;
  draw_string "B";
  moveto 834 203;
  draw_string "L";
  moveto 907 203;
  draw_string "D";
  moveto 675 99;
  draw_string "SOLVE";
  moveto 626 151;
  draw_string "RANDOMIZE";
  draw_prime 566 231;
  draw_prime 640 231;
  draw_prime 714 231;
  draw_prime 788 231;
  draw_prime 861 231;
  draw_prime 936 231

let draw_count count n =
  if !background_color = black then set_color white else set_color black;
  Graphics.moveto 0 0;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string (count ^ string_of_int !n);
  Graphics.moveto 0 0

let eval_turn turn dim =
  match dim with
  | true ->
      turn !cube;
      draw_3d !cube;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := !counter + 1
  | false ->
      turn !cube2;
      draw2 !cube2;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := !counter + 1

let eval_solve = function
  | true ->
      cube := solve ();
      draw_3d !cube;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := 0
  | false ->
      cube2 := solve2 ();
      draw2 !cube2;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := 0

let eval_random n dim =
  let _ =
    match dim with
    | true ->
        randomize !cube n;
        draw_3d !cube
    | false ->
        randomize2 !cube2 n;
        draw2 !cube2
  in
  set_color !background_color;
  fill_rect 0 0 1000 100;
  counter := 0

let check_click x y xup xlo yup ylo turn = if xup >= x && x >= xlo && yup >= y && y >= ylo then eval_turn turn !is_3x3

let change_view (view : cube_type -> unit) dim =
  set_color !background_color;
  fill_rect 0 0 1000000 1000000;
  match dim with
  | true ->
      is_3x3 := true;
      eval_solve true;
      view !cube;
      draw_buttons purple
  | false ->
      is_3x3 := false;
      eval_solve false;
      view !cube2;
      draw_buttons purple

let read_key =
  draw_buttons purple;

  while true do
    draw_count ref_c !nextref;
    if button_down () then click := true
    else
      try
        let s = Graphics.wait_next_event [ Button_up; Graphics.Poll ] in
        if s.Graphics.keypressed then
          match read_key () with
          | 'q' -> close_graph ()
          | c -> (
              match c with
              | 'u' -> eval_turn (if !is_3x3 then u_turn else u_turn2) !is_3x3
              | 'U' -> eval_turn (if !is_3x3 then u'_turn else u'_turn2) !is_3x3
              | 'd' -> eval_turn (if !is_3x3 then d_turn else d_turn2) !is_3x3
              | 'D' -> eval_turn (if !is_3x3 then d'_turn else d'_turn2) !is_3x3
              | 'b' -> eval_turn (if !is_3x3 then b_turn else b_turn2) !is_3x3
              | 'B' -> eval_turn (if !is_3x3 then b'_turn else b'_turn2) !is_3x3
              | 'f' -> eval_turn (if !is_3x3 then f_turn else f_turn2) !is_3x3
              | 'F' -> eval_turn (if !is_3x3 then f'_turn else f'_turn2) !is_3x3
              | 'r' -> eval_turn (if !is_3x3 then r_turn else r_turn2) !is_3x3
              | 'R' -> eval_turn (if !is_3x3 then r'_turn else r'_turn2) !is_3x3
              | 'l' -> eval_turn (if !is_3x3 then l_turn else l_turn2) !is_3x3
              | 'L' -> eval_turn (if !is_3x3 then l'_turn else l'_turn2) !is_3x3
              | 'm' -> eval_turn (if !is_3x3 then m_turn else fun _ -> ()) !is_3x3
              | 'M' -> eval_turn (if !is_3x3 then m'_turn else fun _ -> ()) !is_3x3
              | 'e' -> eval_turn (if !is_3x3 then e_turn else fun _ -> ()) !is_3x3
              | 'E' -> eval_turn (if !is_3x3 then e'_turn else fun _ -> ()) !is_3x3
              | 's' -> eval_turn (if !is_3x3 then s_turn else fun _ -> ()) !is_3x3
              | 'S' -> eval_turn (if !is_3x3 then s'_turn else fun _ -> ()) !is_3x3
              | 'x' -> eval_turn (if !is_3x3 then x_rotate else x_rotate2) !is_3x3
              | 'X' -> eval_turn (if !is_3x3 then x'_rotate else x'_rotate2) !is_3x3
              | 'y' -> eval_turn (if !is_3x3 then y_rotate else y_rotate2) !is_3x3
              | 'Y' -> eval_turn (if !is_3x3 then y'_rotate else y'_rotate2) !is_3x3
              | 'z' -> eval_turn (if !is_3x3 then z_rotate else z_rotate2) !is_3x3
              | 'Z' -> eval_turn (if !is_3x3 then z'_rotate else z'_rotate) !is_3x3
              | '.' -> eval_solve !is_3x3
              | '1' -> eval_random 1 !is_3x3
              | '\\' -> eval_random 100 !is_3x3
              | ',' ->
                  background_color := black;
                  set_color !background_color;
                  fill_rect 0 0 1000000 1000000;
                  if !is_3x3 then draw !cube else draw2 !cube;
                  draw_buttons purple
              | '<' ->
                  background_color := white;
                  set_color !background_color;
                  fill_rect 0 0 1000000 1000000;
                  if !is_3x3 then draw !cube else draw2 !cube;
                  draw_buttons purple
              | '2' -> change_view draw2 false
              | '3' -> change_view draw true
              | '4' -> change_view draw_3d true
              | _ -> ())
        else if (not s.button) && !click then
          match (s.mouse_x, s.mouse_y) with
          | a, b ->
              check_click a b 581 521 297 259 (if !is_3x3 then f_turn else f_turn2);
              check_click a b 655 595 297 259 (if !is_3x3 then r_turn else r_turn2);
              check_click a b 729 669 297 259 (if !is_3x3 then u_turn else u_turn2);
              check_click a b 803 743 297 259 (if !is_3x3 then b_turn else b_turn2);
              check_click a b 877 817 297 259 (if !is_3x3 then l_turn else l_turn2);
              check_click a b 951 891 297 259 (if !is_3x3 then d_turn else d_turn2);
              check_click a b 581 521 245 207 (if !is_3x3 then f'_turn else f'_turn2);
              check_click a b 655 595 245 207 (if !is_3x3 then r'_turn else r'_turn2);
              check_click a b 729 669 245 207 (if !is_3x3 then u'_turn else u'_turn2);
              check_click a b 803 743 245 207 (if !is_3x3 then b'_turn else b'_turn2);
              check_click a b 877 817 245 207 (if !is_3x3 then l'_turn else l'_turn2);
              check_click a b 951 891 245 207 (if !is_3x3 then d'_turn else d'_turn2);
              if 803 >= a && a >= 669 && 141 >= b && b >= 103 then eval_solve !is_3x3;
              if 852 >= a && a >= 620 && 193 >= b && b >= 155 then eval_random 100 !is_3x3;
              click := false
      with _ -> failwith "Window closed"
  done
