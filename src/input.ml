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
let gray = rgb 204 204 204
let counter = ref 0
let ref_c = "Counter: "
let nextref = ref counter
let click = ref false

let darkmode bkgrnd =
  set_font "-*-fixed-medium-r-semicondensed--45-*-*-*-*-*-iso8859-1";
  match bkgrnd with
  | 0xFFFFFF ->
      fill_rect 108 155 102 38;
      moveto 111 151;
      set_color black;
      draw_string "DARK"
  | 0x323232 ->
      fill_rect 94 155 130 38;
      moveto 98 151;
      set_color black;
      draw_string "LIGHT"
  | _ -> ()

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
  set_color gray;
  darkmode !background_color;
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
  if !background_color = 0x323232 then set_color white else set_color black;
  Graphics.moveto 4 2;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string (count ^ string_of_int !n);
  Graphics.moveto 0 0

let draw_background color n m =
  set_color color;
  fill_rect 0 0 n m

let eval_turn turn3 turn2 valid not_rot =
  let _ =
    match !is_3x3 with
    | true ->
        turn3 !cube;
        if !is_3d then draw_3d !cube else draw !cube
    | false ->
        turn2 !cube2;
        if !is_3d then draw2_3d !cube2 else draw2 !cube2
  in
  draw_background !background_color 1000 100;
  if valid && not_rot then incr counter

let eval_solve dim =
  let _ =
    match dim with
    | true ->
        cube := solve ();
        if !is_3d then draw_3d !cube else draw !cube
    | false ->
        cube2 := solve2 ();
        if !is_3d then draw2_3d !cube2 else draw2 !cube2
  in
  draw_background !background_color 1000 100;
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
  draw_background !background_color 1000 100;
  counter := 0

let check_click x y xup xlo yup ylo turn3 turn2 =
  if xup >= x && x >= xlo && yup >= y && y >= ylo then eval_turn turn3 turn2 !is_3x3 true

let change_view is3x3 is3d =
  is_3x3 := is3x3;
  is_3d := is3d;
  draw_background !background_color 1000000 1000000;

  is_3d := is3d;
  match (is3x3, is3d) with
  | true, true ->
      draw_3d !cube;
      draw_buttons purple
  | false, true ->
      draw2_3d !cube2;
      draw_buttons purple
  | true, false ->
      draw !cube;
      draw_buttons purple
  | false, false ->
      draw2 !cube2;
      draw_buttons purple

let change_background color =
  background_color := color;
  draw_background !background_color 1000000 1000000;
  match (!is_3x3, !is_3d) with
  | true, true ->
      draw_3d !cube;
      draw_buttons purple
  | false, true ->
      draw2_3d !cube2;
      draw_buttons purple
  | true, false ->
      draw !cube;
      draw_buttons purple
  | false, false ->
      draw2 !cube2;
      draw_buttons purple

let check_dark_click x y bkgrnd =
  match bkgrnd with
  | 0xFFFFFF -> if 210 >= x && x >= 108 && 193 >= y && y >= 155 then change_background 0x323232
  | 0x323232 -> if 224 >= x && x >= 94 && 193 >= y && y >= 155 then change_background white
  | _ -> ()

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
              | 'u' -> eval_turn u_turn u_turn2 true true
              | 'U' -> eval_turn u'_turn u'_turn2 true true
              | 'd' -> eval_turn d_turn d_turn2 true true
              | 'D' -> eval_turn d'_turn d'_turn2 true true
              | 'b' -> eval_turn b_turn b_turn2 true true
              | 'B' -> eval_turn b'_turn b'_turn2 true true
              | 'f' -> eval_turn f_turn f_turn2 true true
              | 'F' -> eval_turn f'_turn f'_turn2 true true
              | 'r' -> eval_turn r_turn r_turn2 true true
              | 'R' -> eval_turn r'_turn r'_turn2 true true
              | 'l' -> eval_turn l_turn l_turn2 true true
              | 'L' -> eval_turn l'_turn l'_turn2 true true
              | 'm' -> eval_turn m_turn (fun _ -> ()) !is_3x3 true
              | 'M' -> eval_turn m'_turn (fun _ -> ()) !is_3x3 true
              | 'e' -> eval_turn e_turn (fun _ -> ()) !is_3x3 true
              | 'E' -> eval_turn e'_turn (fun _ -> ()) !is_3x3 true
              | 's' -> eval_turn s_turn (fun _ -> ()) !is_3x3 true
              | 'S' -> eval_turn s'_turn (fun _ -> ()) !is_3x3 true
              | 'x' -> eval_turn x_rotate x_rotate2 true false
              | 'X' -> eval_turn x'_rotate x'_rotate2 true false
              | 'y' -> eval_turn y_rotate y_rotate2 true false
              | 'Y' -> eval_turn y'_rotate y'_rotate2 true false
              | 'z' -> eval_turn z_rotate z_rotate2 true false
              | 'Z' -> eval_turn z'_rotate z'_rotate true false
              | '.' -> eval_solve !is_3x3
              | '1' -> eval_random 1 !is_3x3
              | '\\' -> eval_random 100 !is_3x3
              | ',' -> change_background 0x323232
              | '<' -> change_background white
              | '2' -> change_view false false
              | '3' -> change_view true false
              | '4' -> change_view false true
              | '5' -> change_view true true
              | _ -> ())
        else if (not s.button) && !click then
          match (s.mouse_x, s.mouse_y) with
          | x, y ->
              check_click x y 581 521 297 259 f_turn f_turn2;
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
              check_dark_click x y !background_color;
              click := false
      with _ -> failwith "Window closed"
  done
