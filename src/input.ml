open Cube_rep
open Actions
open Graphics
let cube = ref cube_rep
let background_color = ref white
let is_3x3 = ref true
let purple = rgb 240 150 240 
let lblue = rgb 157 190 250
let counter = ref 0

let ref_c = "Counter: "
let nextref = ref counter
let draw_prime x y  = 
if !background_color = black then set_color white else set_color black;
    fill_rect x y 3 9
let draw_buttons color = set_color color;
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
  if !background_color = black then set_color white else set_color black;
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

let eval_turn turn = 
    turn !cube;
    draw !cube;
    set_color !background_color;
    fill_rect 0 0 1000 100;
    counter := !counter + 1

let read_key =
    draw_buttons purple;
         
while true do 
    draw_count ref_c !nextref;
    try 
      let s = Graphics.wait_next_event [Graphics.Button_up; Graphics.Poll] 
      in if s.Graphics.keypressed then match read_key () with
        | 'q' ->
            close_graph ();
        | c ->
            (match c with
            | 'u' -> eval_turn u_turn
            | 'U' -> eval_turn u'_turn
            | 'd' -> eval_turn d_turn
            | 'D' -> eval_turn d'_turn
            | 'b' -> eval_turn b_turn
            | 'B' -> eval_turn b'_turn
            | 'f' -> eval_turn f_turn 
            | 'F' -> eval_turn f'_turn
            | 'r' -> eval_turn r_turn 
            | 'R' -> eval_turn r'_turn
            | 'l' -> eval_turn l_turn
            | 'L' -> eval_turn l'_turn
            | 'm' -> eval_turn m_turn
            | 'M' -> eval_turn m'_turn
            | 'e' -> eval_turn e_turn
            | 'E' -> eval_turn e'_turn
            | 's' -> eval_turn s_turn
            | 'S' -> eval_turn s'_turn
            | 'x' -> eval_turn x_rotate
            | 'X' -> eval_turn x'_rotate
            | 'y' -> eval_turn y_rotate
            | 'Y' -> eval_turn y'_rotate
            | 'z' -> eval_turn z_rotate
            | 'Z' -> eval_turn z'_rotate
            | '.' ->
                cube := solve ();
                draw !cube;
                set_color !background_color;
                fill_rect 0 0 1000 100;
                counter := 0
            | '1' ->
                randomize !cube 1;
                draw !cube
            | '\\' ->
                randomize !cube 100;
                draw !cube
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
            | '2' -> 
                set_color !background_color;
                fill_rect 0 0 1000000 1000000;
                is_3x3 := false;
                draw2 !cube;
                draw_buttons purple
            
            | '3' -> 
                set_color !background_color;
                fill_rect 0 0 1000000 1000000;
                is_3x3 := true;
                draw !cube;
                draw_buttons purple
            | _ -> ()); 
         else if s.button 
              then eval_turn f'_turn; 
    with 
       _ -> failwith "invalid"
  done
