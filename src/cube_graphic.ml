(* open GdkPixbuf *)
open Graphics
(* open Lwt *)

(* open Bigarray
   open Char
   open String
   open Bigarray.Array1 *)

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

let match_x x = function
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

let match_y y = function
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
  for j = 0 to 8 do
    set_cube_color f.(j);
    let poly_array = make_poly_array (match_x x j) ((j / 3 * 53) + y) in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 275 575;
  lineto 500 575

let draw_angled_side_face f x y =
  for j = 0 to 8 do
    set_cube_color f.(j);
    let poly_array = make_poly_array2 ((j mod 3 * 53) + x) (match_y y j) in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 500 350;
  lineto 659 509

let match_x2 x = function 0 -> x | 1 -> x + 75 | 2 -> x + 53 | 3 -> x + 128 | _ -> x

let draw_angled_top_face2 f x y =
  set_line_width 7;
  for j = 0 to 3 do
    set_cube_color f.(j);
    let poly_array = make_poly_array (match_x2 x j) ((j / 2 * 53) + y) in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 275 500;
  lineto 425 500

let match_y2 y = function 0 -> y | 1 -> y + 53 | 2 -> y + 75 | 3 -> y + 128 | _ -> y

let draw_angled_side_face2 f x y =
  set_line_width 7;
  for j = 0 to 3 do
    set_cube_color f.(j);
    let poly_array = make_poly_array2 ((j mod 2 * 53) + x) (match_y2 y j) in
    fill_poly poly_array;
    set_color black;
    draw_poly_line poly_array
  done;
  moveto 425 350;
  lineto 531 456

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

let draw2_3d cube =
  draw_2dcube2 cube.(4) 275 350;
  draw_2dcube2 cube.(5) 125 350;
  draw_2dcube2 cube.(3) 275 200;
  draw_2dcube2 cube.(0) 531 456;
  draw_angled_top_face2 cube.(1) 275 500;
  draw_angled_side_face2 cube.(2) 425 350

(* let () =
   let im = create_image 100 100 in
   draw_image im 300 100 *)

(* let pixbuf = GdkPixbuf.from_file *)

(* let image_filename = "immages/cs3110logo improved.PNG" (* Replace with the path to your image file *)
   let pixbuf = GdkPixbuf.from_file image_filename
   let pixels = GdkPixbuf.get_pixels pixbuf
   let rowstride = GdkPixbuf.get_rowstride pixbuf *)

(* Convert the pixel data into a format that can be drawn on the graphics window using Graphics.make_image *)
(* let image_data =
   (* Define the pixels and rowstride variables and initialize them with the appropriate values *)
   let pixels = GdkPixbuf.get_pixels pixbuf and rowstride = GdkPixbuf.get_rowstride pixbuf in
   let height = GdkPixbuf.get_height pixbuf and width = GdkPixbuf.get_width pixbuf in
   let channels = GdkPixbuf.get_n_channels pixbuf in
   (* Switch the order of the height and width arguments passed to the Array.make_matrix function *)
   let image_data = Array.make_matrix width height 0 in
   (* Use the Bigarray.Array1.to_string function to convert the pixels array to a string *)
   let pixels_str = Bigarray.Array1.to_string pixels in
   for y = 0 to height - 1 do
     (* Use the String.sub function to extract the row of pixel data for the current row in the image *)
     let row = String.sub pixels_str (y * rowstride) (rowstride - 1) in
     for x = 0 to width - 1 do
       let offset = x * channels in
       (* Use the Char.code function to convert the raw byte values to integer values *)
       let r = Char.code row.[offset] and g = Char.code row.[offset + 1] and b = Char.code row.[offset + 2] in
       let pixel = (r lsl 16) + (g lsl 8) + b in
       image_data.(y).(x) <- pixel
     done
   done;
   image_data *)

(* Open the image file and read its contents into a buffer *)
(* let image_file = open_in image_filename
   let image_buffer = Buffer.create 1024
   let _ = Buffer.add_channel image_buffer image_file (in_channel_length image_file)
   let image_data = Buffer.contents image_buffer
   let () = draw_3d cube_rep
   let image_data_scanner = Scanf.Scanning.from_string image_data
   let image_data_array = ref []

   let _ =
     try
       while true do
         let row = Scanf.bscanf image_data_scanner "[|%d" (fun x -> [ x ]) in
         let row = ref row in
         while !row <> [] do
           row := Scanf.bscanf image_data_scanner ";%d" (fun x -> x :: !row)
         done;
         image_data_array := !row :: !image_data_array
       done
     with End_of_file -> ()

   let image_data_array = Array.of_list (List.rev !image_data_array)
   let image_data_array = Array.map (fun row -> Array.of_list row) image_data_array *)
(* let () = draw_image (make_image image_data_array) 0 0 *)
let () = draw_3d cube_rep
(* let () =
   let im = create_image 300 300 in
   draw_image im 300 300 *)
