let cube_rep_r = [| [| "R R R"; "R R R"; "R R R" |] |]
let () = cube_rep_r |> Array.iter (Array.iter print_endline)