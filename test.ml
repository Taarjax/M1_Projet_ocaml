(* Définition des types et constantes *)
type edge = int
type color = int
type piece = {top: edge; right: edge; bottom: edge; left: edge}
type piece_option = piece option

let colors = [1; 2; 3; 4]
let edge_types = [1; 2; 3; 4;5;6;7;8;9]
let n, m = 3, 3

let print_piece piece =
  Printf.printf "{%d %d %d %d}" piece.top piece.right piece.bottom piece.left

  let print_puzzle puzzle =
    Array.iter
      (fun row ->
        Array.iter
          (fun piece_option ->
            match piece_option with
            | Some piece -> print_piece piece
            | None -> Printf.printf "      ")
          row;
        print_endline "")
      puzzle

(* Fonction pour générer des pièces aléatoires *)
let random_piece () =
  let random_edge () = List.nth edge_types (Random.int (List.length edge_types)) in
  {top = random_edge (); right = random_edge (); bottom = random_edge (); left = random_edge ()}

(* Fonction pour générer un puzzle aléatoire *)
let generate_puzzle n m =
  let rec generate_puzzle_aux puzzle n m =
    if n = 0 then puzzle
    else
      let row = List.init m (fun _ -> random_piece ()) in
      generate_puzzle_aux (row :: puzzle) (n - 1) m
  in
  generate_puzzle_aux [] n m

(* Fonction pour vérifier si une pièce peut être placée à une position spécifique *)
let piece_fits puzzle piece i j =
  let top_fits = i = 0 || (match puzzle.(i - 1).(j) with
                          | Some p -> p.bottom = piece.top
                          | None -> false) in
  let left_fits = j = 0 || (match puzzle.(i).(j - 1) with
                          | Some p -> p.right = piece.left
                          | None -> false) in
  top_fits && left_fits

  (* Fonction pour générer une bordure aléatoire *)
let random_edge () = List.nth edge_types (Random.int (List.length edge_types))
(* Fonction pour résoudre le puzzle en utilisant le backtracking *)
let rec solve puzzle pieces i j =
  if i = n then true
  else if j = m then solve puzzle pieces (i + 1) 0
  else if puzzle.(i).(j) <> None then solve puzzle pieces i (j + 1)
  else
    let rec try_pieces pieces =
      match pieces with
      | [] -> false
      | piece :: rest ->
        if piece_fits puzzle piece i j then (
          puzzle.(i).(j) <- Some piece;
          if solve puzzle rest i (j + 1) then true
          else (
            puzzle.(i).(j) <- None;
            try_pieces rest
          )
        )
        else try_pieces rest
    in
    try_pieces pieces

(* Fonction pour générer un puzzle solvable *)
let generate_solvable_puzzle n m =
  let puzzle = Array.make_matrix n m None in
  let gray = 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let top = if i = 0 then gray else (Option.get puzzle.(i - 1).(j)).bottom in
      let left = if j = 0 then gray else (Option.get puzzle.(i).(j - 1)).right in
      let bottom = if i = n - 1 then gray else random_edge () in
      let right = if j = m - 1 then gray else random_edge () in
      puzzle.(i).(j) <- Some {top; right; bottom; left}
    done
  done;
  puzzle

    (* Programme principal *)
    let () =
    Random.self_init ();
    let puzzle = generate_solvable_puzzle n m in
    let pieces = List.flatten (Array.to_list (Array.map Array.to_list puzzle)) in
    let solved = solve puzzle (List.map Option.get pieces) 0 0 in
    if solved then (
      Printf.printf "Le puzzle a été résolu!\n";
      print_puzzle puzzle
    ) else Printf.printf "Le puzzle n'a pas pu être résolu!\n"