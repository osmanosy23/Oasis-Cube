open Graphics

let () = open_graph " 1000x900"

type color = White | Red | Blue | Orange | Yellow | Green
type face = color array
type cube_type = face array

let white_face = [| White; White; White; White; White; White; White; White; White |]
let red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]
let blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |]
let orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]
let yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]
let white_face2 = [| White; White; White; White |]
let red_face2 = [| Red; Red; Red; Red |]
let blue_face2 = [| Blue; Blue; Blue; Blue |]
let orange_face2 = [| Orange; Orange; Orange; Orange |]
let yellow_face2 = [| Yellow; Yellow; Yellow; Yellow |]
let green_face2 = [| Green; Green; Green; Green |]

let cube_rep =
  let open Array in
  [| copy orange_face; copy yellow_face; copy green_face; copy white_face; copy red_face; copy blue_face |]

let solve () =
  let open Array in
  [| copy orange_face; copy yellow_face; copy green_face; copy white_face; copy red_face; copy blue_face |]

let cube_rep2 =
  let open Array in
  [| copy orange_face2; copy yellow_face2; copy green_face2; copy white_face2; copy red_face2; copy blue_face2 |]

(* let solve2 () =
   let open Array in
   [| copy orange_face2; copy yellow_face2; copy green_face2; copy white_face2; copy red_face2; copy blue_face2 |] *)

let set_cube_color = function
  | White -> set_color white
  | Red -> set_color red
  | Green -> set_color green
  | Orange -> set_color (rgb 255 165 0)
  | Yellow -> set_color yellow
  | Blue -> set_color blue

let draw_2dcube f x y =
  for j = 0 to 8 do
    set_cube_color f.(j);
    match j mod 3 with
    | 0 -> fill_rect x ((j / 3 * 75) + y) 75 75
    | 1 -> fill_rect (x + 75) ((j / 3 * 75) + y) 75 75
    | 2 -> fill_rect (x + 150) ((j / 3 * 75) + y) 75 75
    | _ -> failwith "invalid"
  done;
  (*the rest of the function draws the black lines separating the colors*)
  Graphics.set_color black;
  fill_rect 275 125 7 675;
  fill_rect 350 125 7 675;
  fill_rect 425 125 7 675;
  fill_rect 500 125 7 675;

  fill_rect 50 350 900 7;
  fill_rect 50 425 900 7;
  fill_rect 50 500 900 7;
  fill_rect 50 575 900 7;

  fill_rect 50 350 7 225;
  fill_rect 125 350 7 225;
  fill_rect 200 350 7 225;

  fill_rect 575 350 7 225;
  fill_rect 650 350 7 225;
  fill_rect 725 350 7 225;
  fill_rect 800 350 7 225;
  fill_rect 875 350 7 225;
  fill_rect 950 350 7 232;

  fill_rect 275 125 225 7;
  fill_rect 275 200 225 7;
  fill_rect 275 275 225 7;
  fill_rect 275 650 225 7;
  fill_rect 275 725 225 7;
  fill_rect 275 800 232 7

let draw_2dcube2 f x y =
  for j = 0 to 3 do
    set_cube_color f.(j);
    match j mod 2 with
    | 0 -> fill_rect x ((j / 2 * 75) + y) 75 75
    | 1 -> fill_rect (x + 75) ((j / 2 * 75) + y) 75 75
    | _ -> failwith "invalid"
  done;
  (*the rest of the function draws the black lines separating the colors*)
  Graphics.set_color black;
  fill_rect 275 200 7 450;
  fill_rect 350 200 7 450;
  fill_rect 425 200 7 450;
  fill_rect 500 350 7 150; 

  fill_rect 125 350 600 7;
  fill_rect 125 425 600 7;
  fill_rect 125 500 600 7;
  fill_rect 275 575 150 7;

  fill_rect 125 350 7 150;
  fill_rect 200 350 7 150;

  fill_rect 575 350 7 150;
  fill_rect 650 350 7 150;
  fill_rect 725 350 7 157;

 
  fill_rect 275 200 150 7;
  fill_rect 275 275 150 7;
  fill_rect 275 650 157 7

let draw cube =
  draw_2dcube cube.(4) 275 350;
  draw_2dcube cube.(0) 725 350;
  draw_2dcube cube.(5) 50 350;
  draw_2dcube cube.(2) 500 350;
  draw_2dcube cube.(1) 275 575;
  draw_2dcube cube.(3) 275 125

let draw2 cube =
  draw_2dcube2 cube.(4) 275 350;
  draw_2dcube2 cube.(0) 575 350;
  draw_2dcube2 cube.(5) 125 350;
  draw_2dcube2 cube.(2) 425 350;
  draw_2dcube2 cube.(1) 275 500;
  draw_2dcube2 cube.(3) 275 200
(* set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
   Graphics.draw_string("Counter: 0") *)

let () = draw2 cube_rep2 

(*
   Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" *)

(* let char = draw_string "WHY";;

   set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" *)

(* let time f x =
   let start = Unix.gettimeofday ()
   in let res = f x
   in let stop = Unix.gettimeofday ()
   in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
   in res;
   draw_char ('C');; set_text_size 400;;*)
