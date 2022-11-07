let turn_clock pface =
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

let turn_clock_outer rface tface lface bface =
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

let u_turn cube =
  turn_clock cube.(4);
  turn_clock_outer cube.(2) cube.(1) cube.(5) cube.(3)

let turn_counter face =
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

let turn_counter_outer rface tface lface bface =
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

let u'_turn cube =
  turn_counter cube.(4);
  turn_counter_outer cube.(2) cube.(1) cube.(5) cube.(3)
