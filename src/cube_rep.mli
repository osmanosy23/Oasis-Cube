(** Representation of the cube itself.
    Type [dune utop] in terminal and type [#use "cube.ml";;] to run code*)

type color
type face = color array

val white_face : face
val red_face : face
val blue_face : face
val orange_face : face
val yellow_face : face
val green_face : face
val cube : face array
val set_cube_color : color -> unit
val draw_2dcube : color array -> int -> int -> unit
