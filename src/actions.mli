(** Representation of 3x3_cube movements *)

val u_turn : 'a array array -> unit
(** [u_turn 3x3_cube] makes a U turn on [3x3_cube]. *)

val u'_turn : 'a array array -> unit
(** [u'_turn 3x3_cube] makes a U' turn on [3x3_cube]. *)

val f_turn : 'a array array -> unit
(** [f_turn 3x3_cube] makes a F turn on [3x3_cube]. *)

val f'_turn : 'a array array -> unit
(** [f'_turn 3x3_cube] makes a F' turn on [3x3_cube]. *)

val d_turn : 'a array array -> unit
(** [d_turn 3x3_cube] makes a D turn on [3x3_cube]. *)

val d'_turn : 'a array array -> unit
(** [d'_turn 3x3_cube] makes a D' turn on [3x3_cube]. *)

val b_turn : 'a array array -> unit
(** [f_turn 3x3_cube] makes a F turn on [3x3_cube]. *)

val b'_turn : 'a array array -> unit
(** [f'_turn 3x3_cube] makes a F' turn on [3x3_cube]. *)

val r_turn : 'a array array -> unit
(** [r_turn 3x3_cube] makes a R turn on [3x3_cube]. *)

val r'_turn : 'a array array -> unit
(** [r'_turn 3x3_cube] makes a R' turn on [3x3_cube]. *)

val l_turn : 'a array array -> unit
(** [l_turn 3x3_cube] makes a L turn on [3x3_cube]. *)

val l'_turn : 'a array array -> unit
(** [l'_turn 3x3_cube] makes a L' turn on [3x3_cube]. *)

val m_turn : 'a array array -> unit
(** [m_turn 3x3_cube] makes a M turn on [3x3_cube]. *)

val m'_turn : 'a array array -> unit
(** [m'_turn 3x3_cube] makes a M' turn on [3x3_cube]. *)

val e_turn : 'a array array -> unit
(** [e_turn 3x3_cube] makes an E turn on [3x3_cube]. *)

val e'_turn : 'a array array -> unit
(** [e'_turn 3x3_cube] makes an E' turn on [3x3_cube]. *)

val s_turn : 'a array array -> unit
(** [s_turn 3x3_cube] makes a S turn on [3x3_cube]. *)

val s'_turn : 'a array array -> unit
(** [s'_turn 3x3_cube] makes a S' turn on [3x3_cube]. *)

val y_rotate : 'a array array -> unit
(** [y_rotate 3x3_cube] makes an y rotation on [3x3_cube]. *)

val y'_rotate : 'a array array -> unit
(** [y'_rotate 3x3_cube] makes an y' rotation on [3x3_cube]. *)

val x_rotate : 'a array array -> unit
(** [x_rotate 3x3_cube] makes an x rotation on [3x3_cube]. *)

val x'_rotate : 'a array array -> unit
(** [x'_rotate 3x3_cube] makes an x' rotation on [3x3_cube]. *)

val z_rotate : 'a array array -> unit
(** [z_rotate 3x3_cube] makes a z rotation on [3x3_cube]. *)

val z'_rotate : 'a array array -> unit
(** [z'_rotate 3x3_cube] makes a z' rotation on [3x3_cube]. *)

val random_int_bound : int -> int -> int
(** [random_int_bound a b] returns a random integer in the range from [a] to [b] (a inclusive and b not inclusive). *)

val randomize : 'a array array -> int -> unit
(** [randomize 3x3_cube] do [n] number of random moves on the [3x3_cube].*)
