(** Representation of cube movements *)

val turn_clock : 'a array -> unit
(** [turn_clock face] turns the side that [face] represents clockwise. *)

val turn_clock_outer : 'a array -> 'a array -> 'a array -> 'a array -> unit

val turn_counter : 'a array -> unit
(** [turn_counter face] turns the side that [face] represents counterclockwise. *)

val turn_counter_outer : 'a array -> 'a array -> 'a array -> 'a array -> unit
