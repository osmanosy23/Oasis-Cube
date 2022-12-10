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

let solve2 () =
  let open Array in
  [| copy orange_face2; copy yellow_face2; copy green_face2; copy white_face2; copy red_face2; copy blue_face2 |]

let set_cube_color = function
  | White -> set_color white
  | Red -> set_color red
  | Green -> set_color green
  | Orange -> set_color (rgb 255 165 0)
  | Yellow -> set_color yellow
  | Blue -> set_color blue

let draw_square x y =
  set_line_width 7;
  fill_rect x y 75 75;
  set_color black;
  draw_rect x y 75 75

let draw_2dcube f x y =
  for j = 0 to 8 do
    set_cube_color f.(j);
    match j mod 3 with
    | 0 -> draw_square x ((j / 3 * 75) + y)
    | 1 -> draw_square (x + 75) ((j / 3 * 75) + y)
    | 2 -> draw_square (x + 150) ((j / 3 * 75) + y)
    | _ -> failwith "invalid"
  done

let draw_2dcube2 f x y =
  for j = 0 to 3 do
    set_cube_color f.(j);
    match j mod 2 with
    | 0 -> draw_square x ((j / 2 * 75) + y)
    | 1 -> draw_square (x + 75) ((j / 2 * 75) + y)
    | _ -> failwith "invalid"
  done

let make_poly_array x y = [| (x, y); (x + 53, y + 53); (x + 128, y + 53); (x + 75, y) |]
let make_poly_array2 x y = [| (x, y); (x, y + 75); (x + 53, y + 128); (x + 53, y + 53) |]

let match_j x = function
  | 0 -> x
  | 1 -> x + 75
  | 2 -> x + 150
  | 3 -> x + 53
  | 4 -> x + 128
  | 5 -> x + 203
  | 6 -> x + 106
  | 7 -> x + 181
  | 8 -> x + 256
  | _ -> x

let match_i y = function
  | 0 -> y
  | 1 -> y + 53
  | 2 -> y + 106
  | 3 -> y + 75
  | 4 -> y + 128
  | 5 -> y + 181
  | 6 -> y + 150
  | 7 -> y + 203
  | 8 -> y + 256
  | _ -> y

let draw_angled_top_face f x y =
  set_line_width 7;
  let xpos = ref x in
  for j = 0 to 8 do
    set_cube_color f.(j);
    xpos := match_j x j;
    let poly_array = make_poly_array !xpos ((j / 3 * 53) + y) in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 275 575;
  lineto 500 575

let draw_angled_side_face f x y =
  let ypos = ref x in
  for j = 0 to 8 do
    set_cube_color f.(j);
    ypos := match_i y j;
    let poly_array = make_poly_array2 ((j mod 3 * 53) + x) !ypos in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 500 350;
  lineto 659 509

let draw_3d cube =
  draw_2dcube cube.(3) 275 125;
  draw_2dcube cube.(4) 275 350;
  draw_2dcube cube.(5) 50 350;
  draw_2dcube cube.(0) 659 509;
  draw_angled_top_face cube.(1) 275 575;
  draw_angled_side_face cube.(2) 500 350

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

let () = draw_3d cube_rep
