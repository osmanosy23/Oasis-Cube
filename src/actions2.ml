let turn_counter_face pface =
  match pface with
  | [| a; b; c; d |] ->
      pface.(0) <- c;
      pface.(1) <- a;
      pface.(2) <- d;
      pface.(3) <- b
  | _ -> failwith "invalid"

let turn_F rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; _; b; _; c; d; _; _; _; e; _; f; _; _; g; h |] ->
      lface.(1) <- h;
      lface.(3) <- g;
      tface.(0) <- e;
      tface.(1) <- f;
      rface.(0) <- d;
      rface.(2) <- c;
      bface.(2) <- a;
      bface.(3) <- b
  | _ -> failwith "invalid"

let turn_clock_face pface =
  match pface with
  | [| a; b; c; d |] ->
      pface.(0) <- b;
      pface.(1) <- d;
      pface.(2) <- a;
      pface.(3) <- c
  | _ -> failwith "invalid"

let turn_up_layer_R rface tface lface bface =
  let open Array in
  let temparrayone = append lface tface in
  let temparraytwo = append temparrayone rface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; a; _; b; _; c; _; d; e; _; f; _; _; g; _; h |] ->
      lface.(1) <- g;
      lface.(3) <- h;
      tface.(1) <- a;
      tface.(3) <- b;
      rface.(2) <- c;
      rface.(0) <- d;
      bface.(1) <- f;
      bface.(3) <- e
  | _ -> failwith "invalid"

let turn_up_layer_L rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; _; b; _; c; _; d; _; _; e; _; f; g; _; h; _ |] ->
      lface.(1) <- d;
      lface.(3) <- c;
      tface.(0) <- a;
      tface.(2) <- b;
      rface.(0) <- g;
      rface.(2) <- h;
      bface.(0) <- f;
      bface.(2) <- e
  | _ -> failwith "invalid"

let turn_clock_layer_U rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; _; a; b; _; _; c; d; _; _; e; f; _; _; g; h |] ->
      lface.(2) <- g;
      lface.(3) <- h;
      tface.(2) <- e;
      tface.(3) <- f;
      rface.(2) <- c;
      rface.(3) <- d;
      bface.(2) <- a;
      bface.(3) <- b
  | _ -> failwith "invalid"

let turn_clock_layer_D rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; b; _; _; c; d; _; _; e; f; _; _; g; h; _; _ |] ->
      lface.(0) <- g;
      lface.(1) <- h;
      tface.(0) <- e;
      tface.(1) <- f;
      rface.(0) <- c;
      rface.(1) <- d;
      bface.(0) <- a;
      bface.(1) <- b
  | _ -> failwith "invalid"

(*let cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |]*)

let f_turn2 cube =
  turn_clock_face cube.(4);
  turn_F cube.(2) cube.(1) cube.(5) cube.(3)

let f'_turn2 cube =
  f_turn2 cube;
  f_turn2 cube;
  f_turn2 cube

let b'_turn2 cube =
  turn_counter_face cube.(0);
  turn_F cube.(5) cube.(3) cube.(2) cube.(1)

let b_turn2 cube =
  b'_turn2 cube;
  b'_turn2 cube;
  b'_turn2 cube

let r_turn2 cube =
  turn_clock_face cube.(2);
  turn_up_layer_R cube.(0) cube.(1) cube.(4) cube.(3)

let r'_turn2 cube =
  turn_counter_face cube.(2);
  turn_up_layer_R cube.(0) cube.(3) cube.(4) cube.(1)

let l_turn2 cube =
  turn_clock_face cube.(5);
  turn_up_layer_L cube.(4) cube.(3) cube.(0) cube.(1)

let l'_turn2 cube =
  turn_counter_face cube.(5);
  turn_up_layer_L cube.(4) cube.(1) cube.(0) cube.(3)

let u_turn2 cube =
  turn_clock_face cube.(1);
  turn_clock_layer_U cube.(2) cube.(0) cube.(5) cube.(4)

let u'_turn2 cube =
  turn_counter_face cube.(1);
  turn_clock_layer_U cube.(2) cube.(4) cube.(5) cube.(0)

let d'_turn2 cube =
  turn_counter_face cube.(3);
  turn_clock_layer_D cube.(5) cube.(4) cube.(2) cube.(0)

let d_turn2 cube =
  turn_clock_face cube.(3);
  turn_clock_layer_D cube.(5) cube.(0) cube.(2) cube.(4)

let y_rotate2 cube =
  u_turn2 cube;
  d'_turn2 cube

let y'_rotate2 cube =
  u'_turn2 cube;
  d_turn2 cube

let x_rotate2 cube =
  r_turn2 cube;
  l'_turn2 cube

let x'_rotate2 cube =
  r'_turn2 cube;
  l_turn2 cube

let z_rotate2 cube =
  f_turn2 cube;
  b'_turn2 cube

let z'_rotate2 cube =
  f'_turn2 cube;
  b_turn2 cube

let randomize2 cube n =
  let open Actions in
  let num_rotations = ref (random_int_bound n (n + 1)) in
  while !num_rotations > 0 do
    num_rotations := !num_rotations - 1;
    match Random.int 12 with
    | 0 -> r_turn2 cube
    | 1 -> r'_turn2 cube
    | 2 -> f_turn2 cube
    | 3 -> f'_turn2 cube
    | 4 -> u_turn2 cube
    | 5 -> u'_turn2 cube
    | 6 -> l_turn2 cube
    | 7 -> l'_turn2 cube
    | 8 -> d_turn2 cube
    | 9 -> d'_turn2 cube
    | 10 -> b_turn2 cube
    | 11 -> b'_turn2 cube
    | _ -> failwith "invalid"
  done
