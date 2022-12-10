open Cube_rep
open Actions
open Graphics
open Actions2

let cube = ref cube_rep
let cube2 = ref cube_rep2
let background_color = ref white
let is_3x3 = ref true
let is_3d = ref true

let purple = rgb 240 150 240
let lblue = rgb 157 190 250
let lred = rgb 255 150 150
let lgreen = rgb 146 250 146
let lorange = rgb 255 202 149
let lyellow = rgb 255 255 148
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
  set_color lorange;
  fill_rect 74 259 78 38;
  fill_rect 166 259 78 38;
  set_color lyellow;
  fill_rect 92 207 60 38;
  fill_rect 166 207 60 38;
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
  moveto 77 255;
  draw_string "3x3";
  moveto 169 255;
  draw_string "2x2";
  moveto 99 203;
  draw_string "3D";
  moveto 173 203;
  draw_string "2D";
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

let eval_turn turn3 turn2 valid =
  match !is_3x3 with
  | true ->
      turn3 !cube;
      if !is_3d then draw_3d !cube else draw !cube;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := !counter + 1
  | false ->
      turn2 !cube2;
      if !is_3d then draw2_3d !cube2 else draw2 !cube2;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      if valid then counter := !counter + 1 

let eval_solve = function
  | true ->
      cube := solve ();
      if !is_3d then draw_3d !cube else draw !cube;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := 0
  | false ->
      cube2 := solve2 ();
      if !is_3d then draw2_3d !cube2 else draw2 !cube2;
      set_color !background_color;
      fill_rect 0 0 1000 100;
      counter := 0

let eval_random n dim =
  let _ =
    match dim with
    | true ->
        randomize !cube n;
        if !is_3d then draw_3d !cube else draw !cube
    | false ->
        randomize2 !cube2 n;
        if !is_3d then draw2_3d !cube2 else draw2 !cube2
  in
  set_color !background_color;
  fill_rect 0 0 1000 100;
  counter := 0

let check_click x y xup xlo yup ylo turn3 turn2 = 
  if xup >= x && x >= xlo && yup >= y && y >= ylo then eval_turn turn3 turn2 true

let change_view is3x3 is3d=
  is_3x3 := is3x3;
  is_3d := is3d;
  set_color !background_color;
  fill_rect 0 0 1000000 1000000;
  is_3d := is3d;
  match is3x3, is3d with
  | true, true ->
      eval_solve true;
      draw_3d !cube;
      draw_buttons purple
  | false, true ->
      eval_solve false;
      draw2_3d !cube2;
      draw_buttons purple
  | true, false ->
      eval_solve true;
      draw !cube;
      draw_buttons purple
  | false, false ->
      eval_solve false;
      draw2 !cube2;
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
              | 'u' -> eval_turn u_turn u_turn2 true
              | 'U' -> eval_turn u'_turn u'_turn2 true
              | 'd' -> eval_turn d_turn d_turn2 true
              | 'D' -> eval_turn d'_turn d'_turn2 true
              | 'b' -> eval_turn b_turn b_turn2 true
              | 'B' -> eval_turn b'_turn  b'_turn2 true
              | 'f' -> eval_turn f_turn  f_turn2 true
              | 'F' -> eval_turn f'_turn  f'_turn2 true
              | 'r' -> eval_turn r_turn  r_turn2 true
              | 'R' -> eval_turn r'_turn  r'_turn2 true
              | 'l' -> eval_turn l_turn  l_turn2 true 
              | 'L' -> eval_turn l'_turn  l'_turn2 true 
              | 'm' -> eval_turn  m_turn (fun _ -> ()) false 
              | 'M' -> eval_turn m'_turn (fun _ -> ()) false
              | 'e' -> eval_turn e_turn  (fun _ -> ()) false 
              | 'E' -> eval_turn e'_turn  (fun _ -> ()) false 
              | 's' -> eval_turn s_turn  (fun _ -> ()) false 
              | 'S' -> eval_turn s'_turn (fun _ -> ()) false
              | 'x' -> eval_turn x_rotate x_rotate2 true
              | 'X' -> eval_turn x'_rotate x'_rotate2 true
              | 'y' -> eval_turn y_rotate y_rotate2 true
              | 'Y' -> eval_turn y'_rotate y'_rotate2 true 
              | 'z' -> eval_turn z_rotate z_rotate2 true
              | 'Z' -> eval_turn z'_rotate z'_rotate true 
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
              | '2' -> change_view  false false
              | '3' -> change_view  true false
              | '4' -> change_view  false true
              | '5' -> change_view  true true
              | _ -> ())
        else if (not s.button) && !click then
          match (s.mouse_x, s.mouse_y) with
          | x, y ->
              check_click x y 581 521 297 259 f_turn  f_turn2;
              check_click x y 655 595 297 259 r_turn r_turn2;
              check_click x y 729 669 297 259 u_turn u_turn2;
              check_click x y 803 743 297 259 b_turn b_turn2;
              check_click x y 877 817 297 259 l_turn l_turn2;
              check_click x y 951 891 297 259 d_turn d_turn2;
              check_click x y 581 521 245 207 f'_turn f'_turn2;
              check_click x y 655 595 245 207 r'_turn r'_turn2;
              check_click x y 729 669 245 207 u'_turn u'_turn2;
              check_click x y 803 743 245 207 b'_turn b'_turn2;
              check_click x y 877 817 245 207 l'_turn l'_turn2;
              check_click x y 951 891 245 207 d'_turn d'_turn2;
              if 803 >= x && x >= 669 && 141 >= y && y >= 103 then eval_solve !is_3x3;
              if 852 >= x && x >= 620 && 193 >= y && y >= 155 then eval_random 100 !is_3x3;
              if 152 >= x && x >= 74 && 297 >= y && y >= 259 then change_view true !is_3d;
              if 244 >= x && x >= 166 && 297 >= y && y >= 259 then change_view false !is_3d;
              if 152 >= x && x >= 92 && 245 >= y && y >= 207 then change_view !is_3x3 true;
              if 226 >= x && x >= 166 && 245 >= y && y >= 207 then change_view !is_3x3 false;
              click := false
      with _ -> failwith "Window closed"
  done
