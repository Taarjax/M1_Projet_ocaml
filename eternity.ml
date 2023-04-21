(*Projet d'algorithmie, Eternity II - 1 Etudiant *)
(* Auteurs : Rémy Auloy *) 
open Stdlib
open Csv 
open Sys
open Unix

(* Définition des types et constantes *)
type edge = int
type piece = {top: edge; right: edge; bottom: edge; left: edge}
type puzzle = piece option array array

type config = {
  p1_size: int * int;
  p2_puzzles: string list;
  p3_sizes: (int * int) list;
}

(* Liste des couleurs *)
let colors = [1; 2 ; 3; 4; 5; 6; 7; 8; 9]
(* Hash table de colors *)
let color_table = [|"gray"; "red"; "blue"; "green"; "yellow"; "orange"; "purple"; "cyan"; "magenta"; "lime"|]

(* Fonction pour intialiser un puzzle vide *)
let create_empty_puzzle n m = Array.make_matrix n m None

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
let generate_solvable_puzzle (n : int) (m : int) (puzzle : puzzle): puzzle =
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

(* Fonction pour mélanger le puzzle *)
let shuffle_puzzle (puzzle: puzzle) : puzzle =
  let n,m = Array.length puzzle, Array.length puzzle.(0) in
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

(* PREMIER ALGORITHME NAIF *)
(* Fonction pour résoudre le puzzle en utilisant le backtracking *)
let rec solve (puzzle: puzzle) (pieces: piece list) (i: int) (j: int) : puzzle option =
  let n,m = Array.length puzzle, Array.length puzzle.(0) in
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
  


(* ALGORITHME OPTIMISE *)
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

(* Fonction pour résoudre le puzzle en utilisant le backtracking 
   Plus performante car utilise les candidats pour chaque case *)
let rec solve_all (puzzle: puzzle) (pieces: piece list) (i: int) (j: int) : puzzle option =
  let n,m = Array.length puzzle, Array.length puzzle.(0) in
  let corners, borders, inner = separate_pieces pieces in
  let rec try_candidates (candidates: piece list) (next_i: int) (next_j: int) : puzzle option =
    match candidates with
    | [] -> None
    | candidate :: candidates_tail ->
        if piece_fits puzzle candidate i j then
          begin
            let updated_puzzle = Array.copy puzzle in
            updated_puzzle.(i).(j) <- Some candidate;

            let new_remaining_corners, new_remaining_borders, new_remaining_inner =
              let is_corner = (i = 0 && j = 0) || (i = 0 && j = m - 1) || (i = n - 1 && j = 0) || (i = n - 1 && j = m - 1) in
              let is_border = i = 0 || i = n - 1 || j = 0 || j = m - 1 in
              
              if is_corner then
                List.filter ((<>) candidate) corners, borders, inner
              else if is_border then
                corners, List.filter ((<>) candidate) borders, inner
              else
                corners, borders, List.filter ((<>) candidate) inner
            in
            
            match solve_all updated_puzzle (new_remaining_corners @ new_remaining_borders @ new_remaining_inner) next_i next_j with
            | Some _ as solved_puzzle -> solved_puzzle
            | None -> try_candidates candidates_tail next_i next_j
          end
        else
          try_candidates candidates_tail next_i next_j
  in
  if i = n && j = 0 then
    Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let is_corner = (i = 0 && j = 0) || (i = 0 && j = m - 1) || (i = n - 1 && j = 0) || (i = n - 1 && j = m - 1) in
    let is_border = i = 0 || i = n - 1 || j = 0 || j = m - 1 in
    let candidates =
      if is_corner then
        corners
      else if is_border then
        borders
      else
        inner
    in
  try_candidates candidates next_i next_j
  

(* --------------------------AFFICHAGE-------------------------- *)

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
  Stdlib.close_out oc 

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
    Stdlib.close_in ic;
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

(* Fonction pour afficher les pièces d'un puzzle *)
let print_pieces corners borders inner =
  let print_piece_list label pieces =
    print_endline label;
    List.iter (fun piece ->
      print_piece piece;
      print_newline ()) pieces in

  print_piece_list "Corners:" corners;
  print_piece_list "Borders:" borders;
  print_piece_list "Inner pieces:" inner;;

(* Fonction pour créer un dossier s'il n'existe pas *)
let create_directory_if_not_exists path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755

(* Méthode pour lire les lignes d'un fichier*)
let input_lines filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      Stdlib.close_in ic;
      List.rev acc
  in
  read_lines []
;;

(* Fonction pour lire le fichier de configuration *)
let read_config filename =
  if Sys.file_exists filename then
    let config = ref {
      p1_size = (0, 0);
      p2_puzzles = [];
      p3_sizes = [];
    } in
    let read_line line =
      if String.length line > 0 && line.[0] != '#' then
        let key, value = match String.split_on_char ':' line with k :: v :: _ -> String.trim k, String.trim v | _ -> "", "" in
        match key with
        | "p1_size" ->
          let n, m = Scanf.sscanf value "%d, %d" (fun a b -> a, b) in
          config := { !config with p1_size = (n, m) }
        | "p2_puzzles" ->
          let puzzles = String.split_on_char ',' value |> List.map String.trim in
          config := { !config with p2_puzzles = puzzles }
        | "p3_sizes" ->
          let sizes = String.split_on_char ';' value |> List.map (fun s -> Scanf.sscanf (String.trim s) "%d, %d" (fun a b -> a, b)) in
          config := { !config with p3_sizes = sizes }
        | _ -> ()
    in
    let lines = input_lines filename in
    List.iter read_line lines;
    Some !config
  else
    None
;;

(* Programme principaux *)
let p1 n m =
  Random.self_init ();
  let empty_puzzle = create_empty_puzzle n m in

  let dirname = "Puzzle_p1" in
  create_directory_if_not_exists dirname;

  let max_attempts = 10 in
  let rec find_solution attempt soluce shuffled_puzzle =
    if attempt >= max_attempts then
      None
    else
      let pieces = List.flatten (Array.to_list (Array.map Array.to_list shuffled_puzzle)) in
      let pieces_filtered = List.filter_map Fun.id pieces in
      let solution = solve_all empty_puzzle pieces_filtered 0 0 in
      (* let solution = solve empty_puzzle pieces_filtered 0 0 in *)
      match solution with
      | Some _ -> solution
      | None ->
        print_endline "Nouvelle solution générée";
        let new_soluce = generate_solvable_puzzle n m empty_puzzle in
        print_endline "SOLUTION DU PUZZLE";
        print_puzzle new_soluce;
        print_svg new_soluce (Filename.concat dirname "Solution_que_lalgo_doit_avoir.svg");
        let new_shuffled_puzzle = shuffle_puzzle new_soluce in
        print_endline "PUZZLE MELANGE";
        print_puzzle new_shuffled_puzzle;
        print_svg new_shuffled_puzzle (Filename.concat dirname "puzzle_melanger_a_resoudre.svg");
        find_solution (attempt + 1) new_soluce new_shuffled_puzzle
  in

  let soluce = generate_solvable_puzzle n m empty_puzzle in
  print_endline "SOLUTION DU PUZZLE";
  print_puzzle soluce;
  print_svg soluce (Filename.concat dirname "Solution_que_lalgo_doit_avoir.svg");

  let shuffled_puzzle = shuffle_puzzle soluce in
  print_endline "PUZZLE MELANGE";
  print_puzzle shuffled_puzzle;
  print_svg shuffled_puzzle (Filename.concat dirname "puzzle_melanger_a_resoudre.svg");

  let start_time = Sys.time () in
  let solution = find_solution 0 soluce shuffled_puzzle in

  match solution with
  | Some solved_puzzle ->
    print_endline "PUZZLE RESOLU";
    print_puzzle solved_puzzle;
    print_svg solved_puzzle (Filename.concat dirname "puzzle_resolu.svg");
    let end_time = Sys.time () in
    let elapsed_time = end_time -. start_time in
    Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;
  | None -> print_endline "Pas de solution après toutes les tentatives"
;;

let p2 puzzle_filename = 
  let puzzle_importe = import_puzzle puzzle_filename in
  print_endline "PUZZLE DEPUIS FICHIER";
  print_puzzle puzzle_importe;

  let dirname = "Puzzle_p2" in
  create_directory_if_not_exists dirname;

  let n, m = Array.length puzzle_importe, Array.length puzzle_importe.(0) in
  let empty_puzzle = create_empty_puzzle n m in
  let start_time = Sys.time () in

  let pieces = get_pieces_from_puzzle puzzle_importe in
  let solution =  solve_all empty_puzzle pieces 0 0 in
  (* let solution =  solve empty_puzzle pieces 0 0 in. *)

  
  match solution with
  | Some solved_puzzle ->
    print_endline "Puzzle importé résolu!";
    print_puzzle solved_puzzle;
    print_svg solved_puzzle (Filename.concat dirname "Solution_de_lalgo_importer.svg");
    let end_time = Sys.time () in
    let elapsed_time = end_time -. start_time in
    Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;
  | None ->
    print_endline "Puzzle importé non résolu. Réessayer..."
;;

let p3 sizes  =
  Random.self_init ();
  let results = ref [] in
  let remaining_puzzles = ref sizes in
  let dir_name = "Puzzle_p3" in

  create_directory_if_not_exists dir_name;

  while !remaining_puzzles <> [] do
    let new_remaining_puzzles = ref [] in
    for k = 0 to (List.length !remaining_puzzles) - 1 do
      let (n, m) = List.nth !remaining_puzzles k in
      let empty_puzzle = create_empty_puzzle n m in
      let soluce = generate_solvable_puzzle n m empty_puzzle in
      let shuffled_puzzle = shuffle_puzzle soluce in
      let pieces = List.flatten (Array.to_list (Array.map Array.to_list shuffled_puzzle)) in
      let pieces_filtered = List.filter_map Fun.id pieces in

      let start_time = Sys.time () in
      let solution = solve_all empty_puzzle pieces_filtered 0 0 in
      (* let solution = solve empty_puzzle pieces_filtered 0 0 in *)
      match solution with
      | Some solved_puzzle ->
        let end_time = Sys.time () in
        let elapsed_time = end_time -. start_time in
        results := !results @ [((n, m), elapsed_time)];

        (* Générer le fichier SVG pour la solution *)
        let svg_filename = Filename.concat dir_name (Printf.sprintf "puzzle%dx%d.svg" n m) in
        print_svg solved_puzzle svg_filename;
      | None ->
        print_endline ("Pas de solution pour " ^ string_of_int n ^ "x" ^ string_of_int m);
        new_remaining_puzzles := !new_remaining_puzzles @ [(n, m)]
    done;
    remaining_puzzles := !new_remaining_puzzles
  done;

  let csv_data = ["taille"; "temps"] :: (List.map (fun ((n, m), time) -> [string_of_int n ^ "x" ^ string_of_int m; string_of_float time]) !results) in
  Csv.save "results.csv" csv_data 
;;

let rec main () =
  match read_config "config.txt" with
  | Some config ->
    print_endline "Choisissez le programme à exécuter :";
    print_endline "1. Programme 1 (Génération de puzzle aléatoire, mélange et résolution. Configurez la taille du puzzle dans config.txt)";
    print_endline "2. Programme 2 (Résolution de puzzle importé depuis un fichier. Ajouter votre puzzle à importer dans /puzzle_importe et configurez le nom du fichier dans config.txt)";
    print_endline "3. Programme 3 (Résolution de puzzle aléatoire de taille variable. Configurez la liste des tailles dans config.txt)";
    print_endline "0. Quitter";
    print_string "> ";
    (match read_line () with
      | "1" ->
        let n, m = config.p1_size in
        print_endline "Exécution du programme 1";
        p1 n m;
        main ()
      | "2" ->
        print_endline "Exécution du programme 2";
        print_endline "Choisissez un puzzle à importer :";
        List.iteri (fun i puzzle -> Printf.printf "%d. %s\n" (i + 1) puzzle) config.p2_puzzles;
        print_string "> ";
        let puzzle_choice = read_line () in
        (try
          let choice = int_of_string puzzle_choice in
          if choice > 0 && choice <= List.length config.p2_puzzles then
            let puzzle_file = List.nth config.p2_puzzles (choice - 1) in
            p2 puzzle_file;
            main ()
          else
            begin
              print_endline "Choix invalide. Veuillez réessayer.";
              main ()
            end
        with
        | Failure _ ->
          print_endline "Entrée invalide. Veuillez réessayer.";
          main ())
      | "3" ->
        print_endline "Exécution du programme 3";
        p3 config.p3_sizes;
        main ()
      | "0" ->
        print_endline "Programme fermé !";
        exit 0
      | _ ->
        print_endline "Entrée invalide. Veuillez réessayer.";
        main ())
  | None ->
    print_endline "Fichier de configuration introuvable. Veuillez créer un fichier config.txt avec les paramètres appropriés."
;;

let () = main () ;;