open Cube_rep
open Actions
open Graphics

let read_key () =
  while
    match read_key () with
    | 'q' ->
        close_graph ();
        false
    | c ->
        (match c with
        | 'u' ->
            u_turn cube;
            draw cube
        | 'U' ->
            u'_turn cube;
            draw cube
        | 'd' ->
            d_turn cube;
            draw cube
        | 'D' ->
            d'_turn cube;
            draw cube
        | 'b' ->
            b_turn cube;
            draw cube
        | 'B' ->
            b'_turn cube;
            draw cube
        | 'f' ->
            f_turn cube;
            draw cube
        | 'F' ->
            f'_turn cube;
            draw cube
        | 'r' ->
            r_turn cube;
            draw cube
        | 'R' ->
            r'_turn cube;
            draw cube
        | 'l' ->
            l_turn cube;
            draw cube
        | 'L' ->
            l'_turn cube;
            draw cube
        | 'm' ->
            m_turn cube;
            draw cube
        | 'M' ->
            m'_turn cube;
            draw cube
        | _ -> ());

        true
  do
    ()
  done

let () = read_key ()