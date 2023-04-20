(* Définition des types et constantes *)
type edge = int
type piece = {top: edge; right: edge; bottom: edge; left: edge}
type puzzle = piece option array array

(* Dimensions du puzzle *) 
let n, m = 8, 8

(* Puzzle *)
let puzzle = Array.make_matrix n m None 

(* Liste des couleurs *)
let colors = [1; 2 ; 3; 4; 5; 6; 7; 8; 9]

(* Fonction pour afficher une pièce *)
let print_piece (piece: piece) : unit =
  Printf.printf "%d %d %d %d ; " piece.top piece.right piece.bottom piece.left

(* Fonction pour afficher le puzzle *)
let print_puzzle (puzzle: puzzle) : unit =
  List.iter
    (fun row ->
      List.iter
        (fun piece_option ->
          match piece_option with
          | Some piece -> print_piece piece
          | None -> Printf.printf "x")
        (Array.to_list row);
      print_endline "")
    (Array.to_list puzzle)

(* Fonction pour générer des pièces aléatoires *)
let random_piece () : piece =
  let random_edge () : edge = List.nth colors (Random.int (List.length colors)) in
  {top = random_edge (); right = random_edge (); bottom = random_edge (); left = random_edge ()}

(* Fonction pour générer une bordure aléatoire *)
let random_edge () : edge = List.nth colors (Random.int (List.length colors))

(* Fonction pour générer un puzzle solvable *)
let generate_solvable_puzzle (n : int) (m : int) : puzzle =
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


(* Fonction pour vérifier si une pièce correspond en haut *)
let piece_fits_top (puzzle: puzzle) (piece: piece) (i: int) (j: int) : bool =
  let gray = 0 in
  match i with
  | 0 -> piece.top = gray
  | _ -> piece.top = (Option.get puzzle.(i - 1).(j)).bottom

(* Fonction pour vérifier si une pièce correspond à gauche *)
let piece_fits_left (puzzle: puzzle) (piece: piece) (i: int) (j: int) : bool =
  let gray = 0 in
  match j with
  | 0 -> piece.left = gray
  | _ -> piece.left = (Option.get puzzle.(i).(j - 1)).right

(* Fonction pour vérifier si une pièce peut être placée à une position spécifique *)
(* On vérifie uniquement que les bords supérieur et gauche de la piece correspondent aux bords inférieur et droit des pièces adjacentes 
  CAR nous parcouront le puzzle de gauche a droite et de haut en bas*)
let piece_fits (puzzle: puzzle) (piece: piece) (i: int) (j: int) : bool =
    piece_fits_top puzzle piece i j && piece_fits_left puzzle piece i j

(* Fonction pour faire pivoter une pièce *)
let rotate (piece: piece) : piece =
  { top = piece.left; right = piece.top; bottom = piece.right; left = piece.bottom }


(* Fonction pour résoudre le puzzle en utilisant le backtracking *)
let rec solve (puzzle: puzzle) (pieces: piece list) (i: int) (j: int) : puzzle option =
  if i = n && j = 0 then Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let try_placing_piece (piece: piece) : puzzle option =
      if piece_fits puzzle piece i j then
        let updated_puzzle = Array.copy puzzle in
        updated_puzzle.(i).(j) <- Some piece;
        solve updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j
      else None
    in
    let rec try_remaining_pieces = function
      | [] -> None
      | piece :: remaining_pieces ->
          match try_placing_piece piece with
          | Some _ as solved_puzzle -> solved_puzzle
          | None -> try_remaining_pieces remaining_pieces
    in
    try_remaining_pieces pieces

    (* Méthode de résolution un peu plus performante que solve *)
  let rec solve_2 (puzzle: puzzle) (remaining_pieces: piece list) (i: int) (j: int) : puzzle option =
    if i = n && j = 0 then Some puzzle
    else
      let next_i, next_j =
        if j = m - 1 then i + 1, 0
        else i, j + 1
      in
      let rec try_remaining_pieces = function
        | [] -> None
        | piece :: r_piece ->
            if piece_fits puzzle piece i j then
              begin
                let updated_puzzle = Array.copy puzzle in
                updated_puzzle.(i).(j) <- Some piece;
                match solve_2 updated_puzzle (List.filter ((<>) piece) remaining_pieces) next_i next_j with
                | Some _ as solved_puzzle -> solved_puzzle
                | None -> try_remaining_pieces r_piece
              end
            else
              try_remaining_pieces r_piece
      in
      try_remaining_pieces remaining_pieces
  
  (* Fonction pour séparer les pièces en coins, bords et pièces internes *)
  let separate_pieces (pieces: piece list) : (piece list) * (piece list) * (piece list) =
    let rec aux (coins, borders, inner) = function
      | [] -> (coins, borders, inner)
      | p :: r_piece ->
          let count_grays = List.fold_left (fun acc x -> if x = 0 then acc + 1 else acc) 0 [p.top; p.right; p.bottom; p.left] in
          if count_grays = 2 then
            aux (p :: coins, borders, inner) r_piece
          else if count_grays = 1 then
            aux (coins, p :: borders, inner) r_piece
          else
            aux (coins, borders, p :: inner) r_piece
    in
    aux ([], [], []) pieces

(*  Fonction pour résoudre le puzzle en utilisant le backtracking 
   Plus performante car utilise les candidats pour chaque case *)
let solve_all (puzzle: puzzle) (pieces: piece list) : puzzle option =
  let corners, borders, inner = separate_pieces pieces in
  let rec solve_aux (remaining_corners: piece list) (remaining_borders: piece list) (remaining_inner: piece list) (i: int) (j: int) : puzzle option =
    if i = n && j = 0 then Some puzzle
    else
      let next_i, next_j =
        if j = m - 1 then i + 1, 0
        else i, j + 1
      in
      let candidates =
        if i = 0 || i = n - 1 || j = 0 || j = m - 1 then
          if i = 0 && j = 0 || i = 0 && j = m - 1 || i = n - 1 && j = 0 || i = n - 1 && j = m - 1 then
            remaining_corners
          else
            remaining_borders
        else
          remaining_inner
      in
      (* On essaye de placer chaque pièce candidate *)
      let rec try_candidates = function
        | [] -> None
        | candidate :: candidates ->
            if piece_fits puzzle candidate i j then
              begin
                let updated_puzzle = Array.copy puzzle in
                updated_puzzle.(i).(j) <- Some candidate;
                let new_remaining_corners, new_remaining_borders, new_remaining_inner =
                  if i = 0 || i = n - 1 || j = 0 || j = m - 1 then
                    if i = 0 && j = 0 || i = 0 && j = m - 1 || i = n - 1 && j = 0 || i = n - 1 && j = m - 1 then
                      List.filter ((<>) candidate) remaining_corners, remaining_borders, remaining_inner
                    else
                      remaining_corners, List.filter ((<>) candidate) remaining_borders, remaining_inner
                  else
                    remaining_corners, remaining_borders, List.filter ((<>) candidate) remaining_inner
                in
                match solve_aux new_remaining_corners new_remaining_borders new_remaining_inner next_i next_j with
                | Some _ as solved_puzzle -> solved_puzzle
                | None -> try_candidates candidates
              end
            else
              try_candidates candidates
      in
      try_candidates candidates
  in
  solve_aux corners borders inner 0 0


  
       
(* Fonction pour mélanger le puzzle *)
 let shuffle_puzzle (puzzle: puzzle) : puzzle =
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




    (* --------------------------AFFICHAGE-------------------------- *)





    
(* Tableau de correspondance des couleurs *)
let color_table = [|"gray"; "red"; "blue"; "green"; "yellow"; "orange"; "purple"; "cyan"; "magenta"; "lime"|]

(* Fonction pour générer le SVG d'une pièce *)
let piece_to_svg piece : string =
  let side_to_polygon side color =
    let color_str = color_table.(color) in
    match side with
    | "top" -> Printf.sprintf "<polygon points=\"0,0 50,50 0,100\" fill=\"%s\" />" color_str
    | "right" -> Printf.sprintf "<polygon points=\"0,0 50,50 100,0\" fill=\"%s\" />" color_str
    | "bottom" -> Printf.sprintf "<polygon points=\"100,0 50,50 100,100\" fill=\"%s\" />" color_str
    | "left" -> Printf.sprintf "<polygon points=\"0,100 50,50 100,100\" fill=\"%s\" />" color_str
    | _ -> failwith "Invalid side"
  in

  let top = side_to_polygon "top" piece.left in
  let right = side_to_polygon "right" piece.top in
  let bottom = side_to_polygon "bottom" piece.right in
  let left = side_to_polygon "left" piece.bottom in

  Printf.sprintf "<g>\n%s\n%s\n%s\n%s\n</g>" top right bottom left


(* Fonction pour générer le SVG d'un puzzle *)
let pieces_to_svg puzzle filename : unit =
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

(* Fonction pour afficher le SVG d'un puzzle *)
let print_svg puzzle filename : unit =
  pieces_to_svg puzzle filename


  (*  --------------------------UTILS--------------------------  *)

  (* Fonction pour importer un puzzle depuis un fichier texte *)
  let import_puzzle filename : puzzle =
    let lines = ref [] in
    let ic = open_in filename in
    try
      while true; do
        lines := input_line ic :: !lines
      done;
      failwith "Unreachable"
    with End_of_file ->
      close_in ic;
      let lines = List.rev !lines in
      let puzzle = Array.of_list (List.map (fun line ->
        let parts = String.split_on_char ';' line in
        let pieces = List.map (fun part ->
            let nums = List.filter_map (fun s -> try Some (int_of_string s) with Failure _ -> None) 
                          (String.split_on_char ' ' (String.trim part)) in
            match nums with
            | [top; right; bottom; left] -> Some {top; right; bottom; left}
            | _ ->
                print_endline ("Could not convert string to int: " ^ part);
                None
        ) parts in
        
        Array.of_list pieces
      ) lines) in
      puzzle

  (* Fonction pour extraire les pièces d'un puzzle *)
  let get_pieces_from_puzzle (puzzle: puzzle) : piece list =
    Array.fold_left (fun acc row ->
      acc @ (Array.fold_left (fun acc_piece piece_option ->
        match piece_option with
        | Some piece -> acc_piece @ [piece]
        | None -> acc_piece
      ) [] row)
    ) [] puzzle

  let print_pieces corners borders inner =
    let print_piece_list label pieces =
      print_endline label;
      List.iter (fun piece ->
        print_piece piece;
        print_newline ()) pieces in

    print_piece_list "Corners:" corners;
    print_piece_list "Borders:" borders;
    print_piece_list "Inner pieces:" inner;;

  (* Programme principal *)
  let p1() =
    Random.self_init ();
    let soluce = generate_solvable_puzzle n m in
    print_endline "SOLUTION DU PUZZLE";
    print_puzzle soluce;
    print_svg soluce "Solution_que_lalgo_doit_avoir.svg";
    let shuffled_puzzle = shuffle_puzzle soluce in

    print_endline "PUZZLE MELANGE";
    print_puzzle shuffled_puzzle;
    print_svg shuffled_puzzle "puzzle_melanger_a_resoudre.svg";

    let empty_puzzle =
      Array.init n (fun _ -> Array.init m (fun _ -> None)) in

    let pieces = List.flatten (Array.to_list (Array.map Array.to_list shuffled_puzzle)) in
    let pieces_filtered = List.filter_map Fun.id pieces in

    let start_time = Sys.time () in

    (* let corners, borders, inner = separate_pieces pieces_filtered in *)
    (* let solution = solve_2 empty_puzzle (corners @ borders @ inner) 0 0 in *)
    let solution = solve empty_puzzle pieces_filtered 0 0 in
    match solution with
    | Some solved_puzzle ->
      print_endline "PUZZLE RESOLU";
      print_puzzle solved_puzzle;
      print_svg solved_puzzle "puzzle_resolu.svg";
      let end_time = Sys.time () in
      let elapsed_time = end_time -. start_time in
      Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;
    | None -> print_endline "Pas de solution";;



  let p2() = 
    let puzzle_115s = import_puzzle "puzzle8s.txt" in
    print_endline "PUZZLE DEPUIS FICHIER";
    print_puzzle puzzle_115s;

    let empty_puzzle =
      Array.init n (fun _ -> Array.init m (fun _ -> None)) in
    let start_time = Sys.time () in

    let pieces = get_pieces_from_puzzle puzzle_115s in
    let solution = solve_all empty_puzzle pieces in
    
     (* let corners, borders, inner = separate_pieces pieces in *)
    (* let solution = solve_2 empty_puzzle (corners @ borders @ inner) 0 0 in   *)

    match solution with
    | Some solved_puzzle ->
      print_endline "Puzzle importé résolu!";
      print_puzzle solved_puzzle;
      print_svg solved_puzzle "Solution_de_lalgo_importer.svg";
      let end_time = Sys.time () in
      let elapsed_time = end_time -. start_time in
      Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;
    | None ->
      print_endline "Puzzle importé non résolu. Réessayer...";;

  let main () =
    p1()
    (* p2();; *)

  let () = main ()


    (* let rec solve_with_rotate (puzzle: puzzle) (pieces: piece list) (i: int) (j: int) : puzzle option =
    if i = n && j = 0 then Some puzzle
    else
      let next_i, next_j =
        if j = m - 1 then i + 1, 0
        else i, j + 1
      in
      let try_placing_piece (piece: piece) : puzzle option =
        let rec try_piece_with_rotations rotations_left =
          if rotations_left = 0 then None
          else
            let updated_piece = if rotations_left = 4 then piece else rotate piece in
            if piece_fits puzzle updated_piece i j then
              let updated_puzzle = Array.copy puzzle in
              updated_puzzle.(i).(j) <- Some updated_piece;
              match solve_with_rotate updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j with
              | Some _ as solved_puzzle -> solved_puzzle
              | None -> try_piece_with_rotations (rotations_left - 1)
            else
              try_piece_with_rotations (rotations_left - 1)
        in
        try_piece_with_rotations 4
      in
      let rec try_remaining_pieces = function
        | [] -> None
        | p :: ps ->
            match try_placing_piece p with
            | Some _ as solved_puzzle -> solved_puzzle
            | None -> try_remaining_pieces ps
      in
      try_remaining_pieces pieces *)




(* Fonction mélange d'un puzzle qui tourne également les pieces *)
(* let shuffle_puzzle (puzzle: puzzle) : puzzle =
  let shuffled_puzzle = Array.copy puzzle in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let i' = Random.int n in
      let j' = Random.int m in
      let tmp = shuffled_puzzle.(i).(j) in
      shuffled_puzzle.(i).(j) <- shuffled_puzzle.(i').(j');
      shuffled_puzzle.(i').(j') <- tmp;
      match shuffled_puzzle.(i).(j) with
      | None -> ()
      | Some p ->
          let r = Random.int 4 in
          let rotated_piece = ref p in
          for _ = 1 to r do
            rotated_piece := rotate !rotated_piece
          done;
          shuffled_puzzle.(i).(j) <- Some !rotated_piece
    done
  done;
  shuffled_puzzle *)



  (*
  (* MRV = meme temps de résolution *)
(* Fonction pour vérifier si le puzzle est résolu *)
let is_solved puzzle =
  Array.for_all (fun row ->
    Array.for_all (fun cell -> cell <> None) row
  ) puzzle

(* Fonction pour obtenir les valeurs restantes pour une position spécifique *)
let get_remaining_values puzzle pieces i j =
  List.filter (fun piece -> piece_fits puzzle piece i j) pieces


(* Fonction pour essayer de placer une pièce dans le puzzle *)
let try_placing_piece puzzle piece i j =
  if piece_fits puzzle piece i j then
    let updated_puzzle = Array.copy puzzle in
    updated_puzzle.(i).(j) <- Some piece;
    Some updated_puzzle
  else
    None

(* Fonction pour résoudre le puzzle en utilisant le backtracking et la heuristique MRV *)
let rec solve_mrv puzzle pieces i j =
  if i = n && j = 0 then Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let remaining_values = get_remaining_values puzzle pieces i j in
    let rec try_remaining_values = function
      | [] -> None
      | piece :: remaining_pieces ->
        match try_placing_piece puzzle piece i j with
        | Some updated_puzzle ->
          (match solve_mrv updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j with
          | Some _ as solved_puzzle -> solved_puzzle
          | None -> try_remaining_values remaining_pieces)
        | None -> try_remaining_values remaining_pieces
    in
    try_remaining_values remaining_values
  


(* FORWARD CHECKING = PAS PLUS PERFORMANT
Fonction pour vérifier si le puzzle est résolu *)
let is_solved puzzle =
  Array.for_all (fun row ->
    Array.for_all (fun cell -> cell <> None) row
  ) puzzle

(* Fonction pour obtenir les valeurs restantes pour une position spécifique *)
let get_remaining_values puzzle pieces i j =
  List.filter (fun piece -> piece_fits puzzle piece i j) pieces

(* Fonction pour vérifier si les pièces restantes peuvent être placées dans les positions adjacentes *)
let forward_checking puzzle pieces i j =
  let positions = [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] in
  List.for_all (fun (x, y) ->
    if x >= 0 && x < n && y >= 0 && y < m && puzzle.(x).(y) = None then
      let remaining_values = get_remaining_values puzzle pieces x y in
      remaining_values <> []
    else
      true
  ) positions

(* Fonction pour essayer de placer une pièce dans le puzzle *)
let try_placing_piece puzzle piece i j =
  if piece_fits puzzle piece i j then
    let updated_puzzle = Array.copy puzzle in
    updated_puzzle.(i).(j) <- Some piece;
    Some updated_puzzle
  else
    None

(* Fonction pour résoudre le puzzle en utilisant le backtracking et l'heuristique forward-checking *)
let rec solve_forward_checking puzzle pieces i j =
  if i = n && j = 0 then Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let remaining_values = get_remaining_values puzzle pieces i j in
    let rec try_remaining_values = function
      | [] -> None
      | piece :: remaining_pieces ->
        match try_placing_piece puzzle piece i j with
        | Some updated_puzzle ->
          if forward_checking updated_puzzle (List.filter ((<>) piece) pieces) i j then
            match solve_forward_checking updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j with
            | Some _ as solved_puzzle -> solved_puzzle
            | None -> try_remaining_values remaining_pieces
          else
            try_remaining_values remaining_pieces
        | None -> try_remaining_values remaining_pieces
    in
    try_remaining_values remaining_values 


 (* LCV = environ 5 seconde plus long
Fonction pour vérifier si le puzzle est résolu *)
let is_solved puzzle =
  Array.for_all (fun row ->
    Array.for_all (fun cell -> cell <> None) row
  ) puzzle

(* Fonction pour obtenir les valeurs restantes pour une position spécifique *)
let get_remaining_values puzzle pieces i j =
  List.filter (fun piece -> piece_fits puzzle piece i j) pieces

(* Fonction pour compter les contraintes imposées par une pièce *)
let count_constraints puzzle pieces piece i j =
  let positions = [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)] in
  List.fold_left (fun count (x, y) ->
    if x >= 0 && x < n && y >= 0 && y < m && puzzle.(x).(y) = None then
      count + (List.length (get_remaining_values puzzle pieces x y))
    else
      count
  ) 0 positions


(* Fonction pour trier les pièces par contraintes imposées (LCV) *)
let sort_by_lcv puzzle pieces i j =
  List.sort (fun piece1 piece2 ->
    compare (count_constraints puzzle pieces piece1 i j) (count_constraints puzzle pieces piece2 i j)
  ) pieces

(* Fonction pour essayer de placer une pièce dans le puzzle *)
let try_placing_piece puzzle piece i j =
  if piece_fits puzzle piece i j then
    let updated_puzzle = Array.copy puzzle in
    updated_puzzle.(i).(j) <- Some piece;
    Some updated_puzzle
  else
    None

(* Fonction pour résoudre le puzzle en utilisant le backtracking et l'heuristique LCV *)
let rec solve_lcv puzzle pieces i j =
  if i = n && j = 0 then Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let remaining_values = get_remaining_values puzzle pieces i j in
    let sorted_remaining_values = sort_by_lcv puzzle remaining_values i j in
    let rec try_remaining_values = function
      | [] -> None
      | piece :: remaining_pieces ->
        match try_placing_piece puzzle piece i j with
        | Some updated_puzzle ->
          (match solve_lcv updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j with
          | Some _ as solved_puzzle -> solved_puzzle
          | None -> try_remaining_values remaining_pieces)
        | None -> try_remaining_values remaining_pieces
    in
    try_remaining_values sorted_remaining_values  
   
  
  
  *)