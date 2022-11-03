(*Use [dune utop] in terminal and type [#use "cube.ml";;] to run code*)
open Graphics;;

open_graph " 1500x1500"
(**positions 0-8 of a face array represent the following
   0 = bottom left of face
   1 = bottom middle of face 
   2 = bottom right of face
   3 = middle left of face
   4 = center of face
   5 = middle right of face
   6 = top left of face
   7 = top middle of face
   8 = top right of face
*)
type color = White | Red | Blue | Orange | Yellow | Green 
let white_face = [|White;Red;Blue;Blue;White;Green;Red;Blue;Green|]
let red_face = [|Red;Red;Red;Red;Red;Red;Red;Red;Red|]
let blue_face = [|Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue|]
let orange_face = [|Orange;Orange;Orange;Orange;Orange;Orange;Orange;Orange;Orange|]
let yellow_face = [|Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow;Yellow|]
let green_face = [|Green;Green;Green;Green;Green;Green;Green;Green;Green|]

let cube = [|white_face; red_face; blue_face; orange_face; yellow_face; green_face|]
let set_color = function
| White -> set_color white
| Red -> set_color red 
| Green -> set_color green 
| Orange -> set_color (rgb 255 165 0) 
| Yellow -> set_color yellow 
| Blue -> set_color blue 

let turn_clock face = 
  match face with
[|a;b;c;d;e;f;g;h;i |] -> 
  face.(0) <- c;
  face.(1) <- f;
  face.(2) <- i;
  face.(3) <- b;
  face.(4) <- e;
  face.(5) <- h;
  face.(6) <- a;
  face.(7) <- d;
  face.(8) <- g;
|_ -> failwith "invalid"

let turn_counter face =
  match face with
[|a;b;c;d;e;f;g;h;i |] -> 
  face.(0) <- g;
  face.(1) <- d;
  face.(2) <- a;
  face.(3) <- h;
  face.(4) <- e;
  face.(5) <- b;
  face.(6) <- i;
  face.(7) <- f;
  face.(8) <- c;
|_ -> failwith "invalid"

let draw_2dcube f x y= 
    for j=0 to 8 do 
      set_color f.(j);
      match j mod 3 with
      0 ->  fill_rect (x) (j/3 * 100 + y) 100 100
      | 1 -> fill_rect (x + 100) (j/3 * 100 + y) 100 100
      | 2 -> fill_rect (x + 200) (j/3 * 100 + y) 100 100
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
fill_rect 350 950 310 10;;

draw_2dcube cube.(0) 350 350;
draw_2dcube cube.(2) 50 350;
draw_2dcube cube.(5) 650 350;
draw_2dcube cube.(4) 950 350;
draw_2dcube cube.(1) 350 650;
draw_2dcube cube.(3) 350 50;

let read () =
  while
  (
    match (read_key ()) with 
    | 'q' -> close_graph () ;false
    |c   -> 
begin
  match c with
  | 'r' -> turn_clock cube.(0); draw_2dcube cube.(0) 350 350;
  | 'c' -> turn_counter cube.(0); draw_2dcube cube.(0) 350 350;
  |_    -> ()
end;
true) do ()
done 
in read ();


