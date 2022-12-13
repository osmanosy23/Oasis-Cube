(** Representation of the 2x2 Rubik's Cube movements.*)

val u_turn2 : 'a array array -> unit
(** [u_turn2 2x2_cube] makes a U turn on [2x2_cube].*)

val u'_turn2 : 'a array array -> unit
(** [u'_turn2 2x2_cube] makes a U' turn on [2x2_cube].*)

val f_turn2 : 'a array array -> unit
(** [f_turn2 2x2_cube] makes a F turn on [2x2_cube].*)

val f'_turn2 : 'a array array -> unit
(** [f'_turn2 2x2_cube] makes a F' turn on [2x2_cube].*)

val d_turn2 : 'a array array -> unit
(** [d_turn2 2x2_cube] makes a D turn on [2x2_cube].*)

val d'_turn2 : 'a array array -> unit
(** [d'_turn2 2x2_cube] makes a D' turn on [2x2_cube].*)

val b_turn2 : 'a array array -> unit
(** [f_turn2 2x2_cube] makes a F turn on [2x2_cube].*)

val b'_turn2 : 'a array array -> unit
(** [f'_turn2 2x2_cube] makes a F' turn on [2x2_cube].*)

val r_turn2 : 'a array array -> unit
(** [r_turn2 2x2_cube] makes a R turn on [2x2_cube].*)

val r'_turn2 : 'a array array -> unit
(** [r'_turn2 2x2_cube] makes a R' turn on [2x2_cube].*)

val l_turn2 : 'a array array -> unit
(** [l_turn2 2x2_cube] makes a L turn on [2x2_cube].*)

val l'_turn2 : 'a array array -> unit
(** [l'_turn2 2x2_cube] makes a L' turn on [2x2_cube].*)

val y_rotate2 : 'a array array -> unit
(** [y_rotate2 2x2_cube] makes an y rotation on [2x2_cube].*)

val y'_rotate2 : 'a array array -> unit
(** [y'_rotate2 2x2_cube] makes an y' rotation on [2x2_cube].*)

val x_rotate2 : 'a array array -> unit
(** [x_rotate2 2x2_cube] makes an x rotation on [2x2_cube].*)

val x'_rotate2 : 'a array array -> unit
(** [x'_rotate2 2x2_cube] makes an x' rotation on [2x2_cube].*)

val z_rotate2 : 'a array array -> unit
(** [z_rotate2 2x2_cube] makes a z rotation on [2x2_cube].*)

val z'_rotate2 : 'a array array -> unit
(** [z'_rotate2 2x2_cube] makes a z' rotation on [2x2_cube].*)

val randomize2 : 'a array array -> int -> unit
(** [randomize2 2x2_cube n] do [n] number of random moves on the [2x2_cube].*)
