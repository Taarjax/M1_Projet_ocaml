(* Définition des types et constantes *)
type edge = int
type color = int
type piece = {top: edge; right: edge; bottom: edge; left: edge}
type piece_option = piece option

let colors = [1; 2; 3; 4]
let edge_types = [1; 2; 3; 4;5;6;7;8;9]
let n, m = 12, 12

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

  let edge_to_color edge =
    match edge with
    | 1 -> "red"
    | 2 -> "blue"
    | 3 -> "green"
    | 4 -> "yellow"
    | 5 -> "orange"
    | 6 -> "purple"
    | 7 -> "cyan"
    | 8 -> "magenta"
    | 9 -> "lime"
    | _ -> "black"
  
  let piece_to_svg piece =
    let side_to_polygon side color =
      match side with
      | "top" -> Printf.sprintf "<polygon points=\"0,0 50,50 0,100\" fill=\"%s\" />" color
      | "right" -> Printf.sprintf "<polygon points=\"0,0 50,50 100,0\" fill=\"%s\" />" color
      | "bottom" -> Printf.sprintf "<polygon points=\"100,0 50,50 100,100\" fill=\"%s\" />" color
      | "left" -> Printf.sprintf "<polygon points=\"0,100 50,50 100,100\" fill=\"%s\" />" color
      | _ -> failwith "Invalid side"
    in
  
    let top = side_to_polygon "top" (edge_to_color piece.left) in
    let right = side_to_polygon "right" (edge_to_color piece.top) in
    let bottom = side_to_polygon "bottom" (edge_to_color piece.right) in
    let left = side_to_polygon "left" (edge_to_color piece.bottom) in
  
    Printf.sprintf "<g>\n%s\n%s\n%s\n%s\n</g>" top right bottom left

    let pieces_to_svg puzzle filename =
      let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" in
      let piece_width = 100 in
      let piece_height = 100 in
      let width = Array.length puzzle.(0) * piece_width in
      let height = Array.length puzzle * piece_height in
      let header_svg = Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"%d\" height=\"%d\">" width height in
      let footer = "</svg>" in
    
      let body = Array.mapi (fun i row ->
        Array.mapi (fun j piece_option ->
          match piece_option with
          | Some piece ->
            let x = j * piece_width in
            let y = i * piece_height in
            Printf.sprintf "<g transform=\"translate(%d, %d)\">%s</g>" x y (piece_to_svg piece)
          | None -> ""
        ) row
      ) puzzle in
    
      let svg = header ^ header_svg ^ (String.concat "\n" (Array.to_list (Array.map (String.concat "\n") (Array.map (Array.to_list) body)))) ^ footer in
      let oc = open_out filename in
      output_string oc svg;
      close_out oc

      let print_svg puzzle filename =
        pieces_to_svg puzzle filename
    
  



  let shuffle_puzzle puzzle =
    let shuffled_puzzle = Array.map Array.copy puzzle in
    let n, m = Array.length puzzle, Array.length puzzle.(0) in
    for i = n - 1 downto 1 do
      for j = m - 1 downto 1 do
        let i' = Random.int (i + 1) in
        let j' = Random.int (j + 1) in
        let tmp = shuffled_puzzle.(i).(j) in
        shuffled_puzzle.(i).(j) <- shuffled_puzzle.(i').(j');
        shuffled_puzzle.(i').(j') <- tmp
      done
    done;
    shuffled_puzzle

    (* Programme principal *)
    let () =
    Random.self_init ();
    let puzzle = generate_solvable_puzzle n m in
    let puzzle_shuffle = shuffle_puzzle puzzle in
    print_endline "Puzzle mélangé :";
    print_puzzle puzzle_shuffle;
    print_svg puzzle_shuffle "puzzle_melange.svg";
    let pieces = List.flatten (Array.to_list (Array.map Array.to_list puzzle)) in
    let solved = solve puzzle (List.map Option.get pieces) 0 0 in
    if solved then (
      Printf.printf "Le puzzle a été résolu!\n";
      print_endline "Puzzle résolu :";
      print_puzzle puzzle;
      print_svg puzzle "puzzle_resolu.svg";
    ) else Printf.printf "Le puzzle n'a pas pu être résolu!\n"
  