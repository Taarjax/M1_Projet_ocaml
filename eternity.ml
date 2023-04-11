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
(* let generate_puzzle n m =
  let rec generate_puzzle_aux puzzle n m =
    if n = 0 then puzzle
    else
      let row = List.init m (fun _ -> random_piece ()) in
      generate_puzzle_aux (row :: puzzle) (n - 1) m
  in
  generate_puzzle_aux [] n m *)


(* Fonction pour vérifier si une pièce peut être placée à une position spécifique *)
(* La couleur du bord haut de la piece doit etre égale à la couleur du bord bas de la piece du dessus (si elle existe) *)
(* La couleur du bord gauche de la piece doit etre égale à la couleur du bord droit de la piece de gauche (si elle existe) *)
(* La couleur du bord bas de la piece doit etre égale à la couleur du bord haut de la piece du dessous (si elle existe) *)
(* La couleur du bord droit de la piece doit etre égale à la couleur du bord gauche de la piece de droite (si elle existe) *)
let piece_fits puzzle piece i j =
  let gray = 0 in
  let top = if i = 0 then gray else (Option.get puzzle.(i - 1).(j)).bottom in
  let left = if j = 0 then gray else (Option.get puzzle.(i).(j - 1)).right in
  piece.top = top && piece.left = left



  (* Fonction pour générer une bordure aléatoire *)
let random_edge () = List.nth edge_types (Random.int (List.length edge_types)
)

(* Fonction pour faire pivoter une pièce *)

let rotate (piece: piece) : piece =
  { top = piece.left; right = piece.top; bottom = piece.right; left = piece.bottom }
 
(* Fonction pour résoudre le puzzle en utilisant le backtracking *)
let rec solve (puzzle: piece option array array) (pieces: piece list) (i: int) (j: int) : piece option array array option =
  if i = n && j = 0 then
    Some puzzle
  else
    let next_i, next_j = if j = m - 1 then i + 1, 0 else i, j + 1 in
    let try_piece piece =
      let piece_fits_in_current_position = piece_fits puzzle piece i j in
      if piece_fits_in_current_position then (
        puzzle.(i).(j) <- Some piece;
        match solve puzzle (List.filter ((<>) piece) pieces) next_i next_j with
        | Some _ as solved_puzzle -> solved_puzzle
        | None ->
          puzzle.(i).(j) <- None;
          None
      ) else
        None
    in
    let rec try_pieces pieces =
      match pieces with
      | [] -> None
      | piece :: remaining_pieces ->
        (match try_piece piece with
        | Some _ as solved_puzzle -> solved_puzzle
        | None -> try_pieces remaining_pieces)
    in
    try_pieces pieces

    let rec solve_heuristic (puzzle: piece option array array) (pieces: piece list) (i: int) (j: int) : piece option array array option =
      if i = n && j = 0 then
        Some puzzle
      else
        let next_i, next_j = if j = m - 1 then i + 1, 0 else i, j + 1 in
    
        let matching_edges_count piece =
          let count = ref 0 in
          if i > 0 && puzzle.(i - 1).(j) <> None && (Option.get puzzle.(i - 1).(j)).bottom = piece.top then incr count;
          if j > 0 && puzzle.(i).(j - 1) <> None && (Option.get puzzle.(i).(j - 1)).right = piece.left then incr count;
          if i < n - 1 && puzzle.(i + 1).(j) <> None && (Option.get puzzle.(i + 1).(j)).top = piece.bottom then incr count;
          if j < m - 1 && puzzle.(i).(j + 1) <> None && (Option.get puzzle.(i).(j + 1)).left = piece.right then incr count;
          !count
        in
    
        let sorted_pieces = List.sort (fun p1 p2 -> compare (matching_edges_count p2) (matching_edges_count p1)) pieces in
    
        let try_piece piece =
          let piece_fits_in_current_position = piece_fits puzzle piece i j in
          if piece_fits_in_current_position then (
            puzzle.(i).(j) <- Some piece;
            match solve_heuristic puzzle (List.filter ((<>) piece) pieces) next_i next_j with
            | Some _ as solved_puzzle -> solved_puzzle
            | None ->
              puzzle.(i).(j) <- None;
              None
          ) else
            None
        in
    
        let rec try_pieces pieces =
          match pieces with
          | [] -> None
          | piece :: remaining_pieces ->
            (match try_piece piece with
            | Some _ as solved_puzzle -> solved_puzzle
            | None -> try_pieces remaining_pieces)
        in
        try_pieces sorted_pieces
    
        let count_compatible_sides puzzle piece i j =
          let gray = 0 in
          let top = if i = 0 then gray else (Option.get puzzle.(i - 1).(j)).bottom in
          let left = if j = 0 then gray else (Option.get puzzle.(i).(j - 1)).right in
          let count = ref 0 in
          if piece.top = top then incr count;
          if piece.left = left then incr count;
          !count
        
          let rec solve_fast (puzzle: piece option array array) (pieces: piece list) (i: int) (j: int) : piece option array array option =
            if i = n && j = 0 then
              Some puzzle
            else
              let next_i, next_j = if j = m - 1 then i + 1, 0 else i, j + 1 in
              let sorted_pieces = List.sort (fun p1 p2 ->
                let count1 = count_compatible_sides puzzle p1 i j in
                let count2 = count_compatible_sides puzzle p2 i j in
                compare count2 count1
              ) pieces in
              let rec try_pieces remaining_pieces =
                match remaining_pieces with
                | [] -> None
                | piece :: other_pieces ->
                  let piece_fits_in_current_position = piece_fits puzzle piece i j in
                  if piece_fits_in_current_position then (
                    puzzle.(i).(j) <- Some piece;
                    match solve_fast puzzle (List.filter ((<>) piece) pieces) next_i next_j with
                    | Some _ as solved_puzzle -> solved_puzzle
                    | None ->
                      puzzle.(i).(j) <- None;
                      try_pieces other_pieces
                  ) else
                    try_pieces other_pieces
              in
              try_pieces sorted_pieces
          

    (* Fonction pour générer un puzzle solvable *)
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
    let shuffled_puzzle = Array.copy puzzle in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        let i' = Random.int n in
        let j' = Random.int m in
        let tmp = shuffled_puzzle.(i).(j) in
        shuffled_puzzle.(i).(j) <- shuffled_puzzle.(i').(j');
        shuffled_puzzle.(i').(j') <- tmp
      done
    done;
    shuffled_puzzle

    (* Programme principal *)

    let () =
    Random.self_init ();
    let soluce = generate_solvable_puzzle n m in
    print_endline "SOLUTION DU PUZZLE";
    print_puzzle soluce;
    print_svg soluce "Solution_que_lalgo_doit_avoir.svg";
    let shuffled_puzzle = shuffle_puzzle soluce in
    print_endline "PUZZLE MELANGE";
    print_puzzle shuffled_puzzle;
    print_svg shuffled_puzzle "puzzle_melanger_a_resoudre.svg";
    let pieces = List.flatten (Array.to_list (Array.map Array.to_list shuffled_puzzle)) in
    let pieces_filtered = List.filter_map Fun.id pieces in
    
    let start_time = Sys.time () in
  
    let rec solve_until_found pieces =
      (* match solve_heuristic shuffled_puzzle pieces_filtered 0 0 with *)
      match solve shuffled_puzzle pieces 0 0 with
      | Some solved_puzzle ->
        print_endline "Puzzle résolu!";
        print_puzzle solved_puzzle;
        print_svg solved_puzzle "Solution_de_lalgo.svg"
      | None ->
        (* print_endline "Puzzle non résolu. Réessayer..."; *)
        solve_until_found pieces_filtered
    in
    solve_until_found pieces_filtered;
  
    let end_time = Sys.time () in
    let elapsed_time = end_time -. start_time in
    Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;
  


   (* let () =
   Random.self_init ();
   let soluce = generate_solvable_puzzle n m in
   print_endline "SOLUTION DU PUZZLE";
   print_puzzle soluce;
   print_svg soluce "Solution_que_lalgo_doit_avoir.svg";
   let shuffled_puzzle = shuffle_puzzle soluce in
   print_endline "PUZZLE MELANGE";
   print_puzzle shuffled_puzzle;
   print_svg shuffled_puzzle "puzzle_melanger_a_resoudre.svg";
   let pieces = List.flatten (Array.to_list (Array.map Array.to_list shuffled_puzzle)) in
   let pieces_filtered = List.filter_map Fun.id pieces in

   let rec solve_until_found pieces =
     match solve_fast shuffled_puzzle pieces 0 0 with
     | Some solved_puzzle ->
       print_endline "Puzzle résolu!";
       print_puzzle solved_puzzle;
       print_svg solved_puzzle "Solution_de_lalgo.svg"
     | None ->
       (* print_endline "Puzzle non résolu. Réessayer..."; *)
       solve_until_found pieces_filtered
   in
   solve_until_found pieces_filtered; *)

