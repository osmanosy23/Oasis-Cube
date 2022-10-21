type color
(** The type of colors representing the cube. *)

type cube
(** The abstract type of values representing the cube. *)

type command
(** The abstract type of representing the keyboard press *)

val make_cube : cube -> unit
(** [make_cube a] opens up a Ocaml Graphics window and draw the cube a. *)

val turn: command -> unit
(** [turn a] turns the cube with Ocaml Graphics window opened *)