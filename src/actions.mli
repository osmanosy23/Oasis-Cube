(** Representation of the 3x3 Rubik's Cube movements.*)

val u_turn : 'a array array -> unit
(** [u_turn 3x3_cube] makes a U turn on [3x3_cube]. That is, [u_turn] takes in 
    an 'a array array [3x3_cube] and does a 90-degree clockwise rotation of the 
    upper face.*)

val u'_turn : 'a array array -> unit
(** [u'_turn 3x3_cube] makes a U' turn on [3x3_cube]. That is, [u'_turn] takes 
in a [3x3_cube] and does a 90-degree counterclockwise rotation of the U face. *)

val f_turn : 'a array array -> unit
(** [f_turn 3x3_cube] makes a F turn on [3x3_cube]. That is, the frontside of 
    the cube, which faces the solver, is rotated in a counterlockwise manner. *)

val f'_turn : 'a array array -> unit
(** [f'_turn 3x3_cube] makes a F' turn on [3x3_cube]. [f'_turn 3x3_cube] does a 
    90-degree counterclockwise rotation of the F face. *)

val d_turn : 'a array array -> unit
(** [d_turn 3x3_cube] makes a D turn on [3x3_cube]. That is, [d_turn 3x3_cube] 
    makes a 90-degree clockwise rotation of the bottom/down face--the 
    face opposite to the upper face. *)

val d'_turn : 'a array array -> unit
(** [d'_turn 3x3_cube] makes a D' turn on [3x3_cube]. That is, [d'_turn 3x3_cube] 
    makes a 90-degree counterclockwise rotation of the bottom/down face--the 
    face opposite to the upper face. *)

val b_turn : 'a array array -> unit
(** [b_turn 3x3_cube] makes a B turn on [3x3_cube]. That is,[b_turn 3x3_cube] 
    makes a 90 degree clockwise rotation of the back face. *)

val b'_turn : 'a array array -> unit
(** [b'_turn 3x3_cube] makes a B' turn on [3x3_cube]. That is, [b'_turn 3x3_cube] 
    makes a 90 degree counterclockwise rotation of the back face. *)

val r_turn : 'a array array -> unit
(** [r_turn 3x3_cube] makes a R turn on [3x3_cube]. That is, [r_turn 3x3_cube] 
    makes a 90 degree clockwise rotation of the right face. *)

val r'_turn : 'a array array -> unit
(** [r'_turn 3x3_cube] makes a R' turn on [3x3_cube]. That is, [r'_turn 3x3_cube] 
    makes a 90 degree counterclockwise rotation of the right face. *)

val l_turn : 'a array array -> unit
(** [l_turn 3x3_cube] makes a L turn on [3x3_cube]. That is, [l_turn 3x3_cube] 
    makes a 90 degree clockwise rotation of the left face. *)

val l'_turn : 'a array array -> unit
(** [l'_turn 3x3_cube] makes a L' turn on [3x3_cube]. That is, [l'_turn 3x3_cube] 
    makes a 90 degree counterclockwise rotation of the left face. *)

val m_turn : 'a array array -> unit
(** [m_turn 3x3_cube] makes a M turn on [3x3_cube]. That is, [m_turn 3x3_cube] 
    makes a rotation upon the middle layer of the cube parallel to the R & L faces 
    in a downward motion. *)

val m'_turn : 'a array array -> unit
(** [m'_turn 3x3_cube] makes a M' turn on [3x3_cube]. That is, [m'_turn 3x3_cube] 
    makes a rotation upon the middle layer of the cube parallel to the R & L faces 
    in an upward motion. *)

val e_turn : 'a array array -> unit
(** [e_turn 3x3_cube] makes an E turn on [3x3_cube]. That is, [e_turn 3x3_cube] 
    makes a rotation upon the equatorial layer of the cube parallel to the U & D 
    faces in a rightwards motion. *)

val e'_turn : 'a array array -> unit
(** [e'_turn 3x3_cube] makes an E' turn on [3x3_cube]. That is, [e'_turn 3x3_cube] 
    makes a rotation upon the equatorial layer of the cube parallel to the U & D 
    faces in a leftwards motion. *)

val s_turn : 'a array array -> unit
(** [s_turn 3x3_cube] makes a S turn on [3x3_cube]. That is, [s_turn 3x3_cube] 
    makes a clockwise rotation upon the middle layer parallel to the F & B faces. *)

val s'_turn : 'a array array -> unit
(** [s'_turn 3x3_cube] makes a S' turn on [3x3_cube]. That is, [s'_turn 3x3_cube] 
    makes a counterclockwise rotation upon the middle layer parallel to the F & B faces. *)

val y_rotate : 'a array array -> unit
(** [y_rotate 3x3_cube] makes an y rotation on [3x3_cube]. That is, [y_rotate 3x3_cube] 
    rotates the cube on the Y axis such that the U & D faces remain intact. *)

val y'_rotate : 'a array array -> unit
(** [y'_rotate 3x3_cube] makes an y' rotation on [3x3_cube]. *)

val x_rotate : 'a array array -> unit
(** [x_rotate 3x3_cube] makes an x rotation on [3x3_cube]. That is, [x_rotate 3x3_cube]
    rotates the cube on the X axis such that the R & L faces remain unchanged. *)

val x'_rotate : 'a array array -> unit
(** [x'_rotate 3x3_cube] makes an x' rotation on [3x3_cube].*)

val z_rotate : 'a array array -> unit
(** [z_rotate 3x3_cube] makes a z rotation on [3x3_cube]. This function rotates 
    the cube on the Z axis such that the F & B faces remain intact. *)

val z'_rotate : 'a array array -> unit
(** [z'_rotate 3x3_cube] makes a z' rotation on [3x3_cube].*)

val random_int_bound : int -> int -> int
(** [random_int_bound a b] returns a random integer in the range from [a] to [b] (a inclusive and b not inclusive).*)

val randomize : 'a array array -> int -> unit
(** [randomize 3x3_cube n] do [n] number of random moves on the [3x3_cube].*)
