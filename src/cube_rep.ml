open Graphics
(* positions 0-8 of a face array represent the following
   * 0 = bottom left of face
     1 = bottom middle of face
    2 = bottom right of face
    3 = middle left of face
    4 = center of face
    5 = middle right of face
    6 = top left of face
    7 = top middle of face
    8 = top right of face
*)

let () = open_graph " 1500x1500"

type color = White | Red | Blue | Orange | Yellow | Green
type face = color array

let white_face = [| White; White; White; White; White; White; White; White; White |]
let red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]
let blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |]
let orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]
let yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]
let cube = [| white_face; red_face; blue_face; orange_face; yellow_face; green_face |]

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
    | 0 -> fill_rect x ((j / 3 * 100) + y) 100 100
    | 1 -> fill_rect (x + 100) ((j / 3 * 100) + y) 100 100
    | 2 -> fill_rect (x + 200) ((j / 3 * 100) + y) 100 100
    | _ -> failwith "invalid"
  done;
  Graphics.set_color black;
  fill_rect 450 50 10 900;
  fill_rect 650 50 10 900;
  fill_rect 550 50 10 900;
  fill_rect 50 550 1200 10;
  fill_rect 50 450 1200 10;
  fill_rect 50 650 1200 10;

  fill_rect 350 50 10 900;
  fill_rect 50 350 1200 10;
  fill_rect 50 350 10 300;
  fill_rect 150 350 10 300;
  fill_rect 250 350 10 300;
  fill_rect 750 350 10 300;
  fill_rect 850 350 10 300;
  fill_rect 950 350 10 300;
  fill_rect 1050 350 10 300;
  fill_rect 1150 350 10 300;
  fill_rect 1250 350 10 310;
  fill_rect 350 50 300 10;
  fill_rect 350 150 300 10;
  fill_rect 350 250 300 10;
  fill_rect 350 750 300 10;
  fill_rect 350 850 300 10;
  fill_rect 350 950 310 10
;;

draw_2dcube cube.(0) 350 350;
draw_2dcube cube.(2) 50 350;
draw_2dcube cube.(5) 650 350;
draw_2dcube cube.(4) 950 350;
draw_2dcube cube.(1) 350 650;
draw_2dcube cube.(3) 350 50

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
