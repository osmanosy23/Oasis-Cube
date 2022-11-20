(** Representation of the cube itself.
    Type [dune utop] in terminal to run code*)

type color
(** [color] is the type of each of the colors of a Rubik's Cube. *)

type face = color array
(** [face] is type of a face of of a Rubik's Cube.
    Indices 0-8 of a face array represent the following
    0 = bottom left of face
    1 = bottom middle of face
    2 = bottom right of face
    3 = middle left of face
    4 = center of face
    5 = middle right of face
    6 = top left of face
    7 = top middle of face
    8 = top right of face *)

type cube_type = face array
(** [cube_type] is type of the Rubik's Cube. *)

val cube_rep : face array
(** [cube_rep] is the Rubik's Cube in the simulation. *)

val solve : unit -> color array array
(** [solve ()] returns a solved cube. *)

val draw_2dcube : face -> int -> int -> unit
(** [draw_2dcube face x y] draws a [face] of the Rubik's Cube center at [x, y] *)

val draw : cube_type -> unit
(** [draw_2dcube cube] the Rubik's Cube*)