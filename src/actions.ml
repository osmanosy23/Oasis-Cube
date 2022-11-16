let turn_clock_face pface =
  match pface with
  | [| a; b; c; d; e; f; g; h; i |] ->
      pface.(0) <- c;
      pface.(1) <- f;
      pface.(2) <- i;
      pface.(3) <- b;
      pface.(4) <- e;
      pface.(5) <- h;
      pface.(6) <- a;
      pface.(7) <- d;
      pface.(8) <- g
  | _ -> failwith "invalid"

let turn_clock_layer rface tface lface bface =
  let open Array in
  let temparrayone = append lface tface in
  let temparraytwo = append temparrayone rface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; _; a; _; _; b; _; _; c; d; e; f; _; _; _; _; _; _; g; _; _; h; _; _; i; _; _; _; _; _; _; _; _; j; k; l |] ->
      lface.(2) <- l;
      lface.(5) <- k;
      lface.(8) <- j;
      tface.(0) <- a;
      tface.(1) <- b;
      tface.(2) <- c;
      rface.(6) <- d;
      rface.(3) <- e;
      rface.(0) <- f;
      bface.(6) <- g;
      bface.(7) <- h;
      bface.(8) <- i
  | _ -> failwith "invalid"

let turn_counter_face face =
  match face with
  | [| a; b; c; d; e; f; g; h; i |] ->
      face.(0) <- g;
      face.(1) <- d;
      face.(2) <- a;
      face.(3) <- h;
      face.(4) <- e;
      face.(5) <- b;
      face.(6) <- i;
      face.(7) <- f;
      face.(8) <- c
  | _ -> failwith "invalid"

let turn_counter_layer rface tface lface bface =
  let open Array in
  let temparrayone = append lface tface in
  let temparraytwo = append temparrayone rface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; _; a; _; _; b; _; _; c; d; e; f; _; _; _; _; _; _; g; _; _; h; _; _; i; _; _; _; _; _; _; _; _; j; k; l |] ->
      lface.(2) <- d;
      lface.(5) <- e;
      lface.(8) <- f;
      tface.(0) <- i;
      tface.(1) <- h;
      tface.(2) <- g;
      rface.(6) <- l;
      rface.(3) <- k;
      rface.(0) <- j;
      bface.(6) <- c;
      bface.(7) <- b;
      bface.(8) <- a
  | _ -> failwith "invalid"

let turn_up_layer_R rface tface lface bface =
  let open Array in
  let temparrayone = append lface tface in
  let temparraytwo = append temparrayone rface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; _; a; _; _; b; _; _; c; _; _; d; _; _; e; _; _; f; g; _; _; h; _; _; i; _; _; _; _; j; _; _; k; _; _; l |] ->
      lface.(2) <- j;
      lface.(5) <- k;
      lface.(8) <- l;
      tface.(2) <- a;
      tface.(5) <- b;
      tface.(8) <- c;
      rface.(6) <- d;
      rface.(3) <- e;
      rface.(0) <- f;
      bface.(2) <- i;
      bface.(5) <- h;
      bface.(8) <- g
  | _ -> failwith "invalid"

let turn_up_layer_L rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; _; _; b; _; _; c; _; _; d; _; _; e; _; _; f; _; _; _; _; g; _; _; h; _; _; i; j; _; _; k; _; _; l; _; _ |] ->
      lface.(2) <- f;
      lface.(5) <- e;
      lface.(8) <- d;
      tface.(6) <- c;
      tface.(3) <- b;
      tface.(0) <- a;
      rface.(6) <- l;
      rface.(3) <- k;
      rface.(0) <- j;
      bface.(6) <- i;
      bface.(3) <- h;
      bface.(0) <- g
  | _ -> failwith "invalid"

let turn_clock_layer_U rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; _; _; _; _; _; a; b; c; _; _; _; _; _; _; d; e; f; _; _; _; _; _; _; g; h; i; _; _; _; _; _; _; j; k; l |] ->
      lface.(6) <- j;
      lface.(7) <- k;
      lface.(8) <- l;
      tface.(6) <- g;
      tface.(7) <- h;
      tface.(8) <- i;
      rface.(6) <- d;
      rface.(7) <- e;
      rface.(8) <- f;
      bface.(6) <- a;
      bface.(7) <- b;
      bface.(8) <- c
  | _ -> failwith "invalid"

let turn_clock_layer_D rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; b; c; _; _; _; _; _; _; d; e; f; _; _; _; _; _; _; g; h; i; _; _; _; _; _; _; j; k; l; _; _; _; _; _; _ |] ->
      lface.(0) <- j;
      lface.(1) <- k;
      lface.(2) <- l;
      tface.(0) <- g;
      tface.(1) <- h;
      tface.(2) <- i;
      rface.(0) <- d;
      rface.(1) <- e;
      rface.(2) <- f;
      bface.(0) <- a;
      bface.(1) <- b;
      bface.(2) <- c
  | _ -> failwith "invalid"

let turn_down_layer_L rface tface lface bface =
  let open Array in
  let temparrayone = append rface tface in
  let temparraytwo = append temparrayone lface in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| a; _; _; b; _; _; c; _; _; d; _; _; e; _; _; f; _; _; _; _; g; _; _; h; _; _; i; j; _; _; k; _; _; l; _; _ |] ->
      lface.(2) <- l;
      lface.(5) <- k;
      lface.(8) <- j;
      tface.(6) <- i;
      tface.(3) <- h;
      tface.(0) <- g;
      rface.(6) <- f;
      rface.(3) <- e;
      rface.(0) <- d;
      bface.(6) <- c;
      bface.(3) <- b;
      bface.(0) <- a
  | _ -> failwith "invalid"

let turn_M front_face tface back_face bface =
  let open Array in
  let temparrayone = append front_face tface in
  let temparraytwo = append temparrayone back_face in
  let changearray = append temparraytwo bface in
  match changearray with
  | [| _; a; _; _; b; _; _; c; _; _; d; _; _; e; _; _; f; _; _; g; _; _; h; _; _; i; _; _; j; _; _; k; _; _; l; _ |] ->
      front_face.(1) <- j;
      front_face.(4) <- k;
      front_face.(7) <- l;
      tface.(1) <- a;
      tface.(4) <- b;
      tface.(7) <- c;
      back_face.(7) <- d;
      back_face.(4) <- e;
      back_face.(1) <- f;
      bface.(7) <- g;
      bface.(4) <- h;
      bface.(1) <- i
  | _ -> failwith "invalid"

(*let cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |]*)
let f_turn cube =
  turn_clock_face cube.(4);
  turn_clock_layer cube.(2) cube.(1) cube.(5) cube.(3)

let f'_turn cube =
  turn_counter_face cube.(4);
  turn_counter_layer cube.(2) cube.(1) cube.(5) cube.(3)

let b'_turn cube =
  turn_counter_face cube.(0);
  turn_clock_layer cube.(5) cube.(3) cube.(2) cube.(1)

let b_turn cube =
  turn_clock_face cube.(0);
  turn_counter_layer cube.(5) cube.(3) cube.(2) cube.(1)

let r_turn cube =
  turn_clock_face cube.(2);
  turn_up_layer_R cube.(0) cube.(1) cube.(4) cube.(3)

let r'_turn cube =
  turn_counter_face cube.(2);
  turn_up_layer_R cube.(0) cube.(3) cube.(4) cube.(1)

let l_turn cube =
  turn_counter_face cube.(5);
  turn_down_layer_L cube.(4) cube.(1) cube.(0) cube.(3)

let l'_turn cube =
  turn_clock_face cube.(5);
  turn_up_layer_L cube.(4) cube.(1) cube.(0) cube.(3)

let u_turn cube =
  turn_clock_face cube.(1);
  turn_clock_layer_U cube.(2) cube.(0) cube.(5) cube.(4)

let u'_turn cube =
  turn_counter_face cube.(1);
  turn_clock_layer_U cube.(2) cube.(4) cube.(5) cube.(0)

let d'_turn cube =
  turn_counter_face cube.(3);
  turn_clock_layer_D cube.(5) cube.(4) cube.(2) cube.(0)

let d_turn cube =
  turn_clock_face cube.(3);
  turn_clock_layer_D cube.(5) cube.(0) cube.(2) cube.(4)

let m_turn cube = turn_M cube.(4) cube.(1) cube.(0) cube.(3)
let m'_turn cube = turn_M cube.(4) cube.(3) cube.(0) cube.(1)
