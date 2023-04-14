(* Définition des types et constantes *)
type edge = int
type color = int
type piece = {top: edge; right: edge; bottom: edge; left: edge}
type piece_option = piece option

(* Liste des types de bords disponibles *)
let colors = [1; 2; 3; 4; 5; 6; 7; 8; 9]

(* Fonction pour convertir un edge en une couleur *)
let colors_to_string edge =
  match edge with
  | 0 -> "gray"
  | 1 -> "red"
  | 2 -> "blue"
  | 3 -> "green"
  | 4 -> "yellow"
  | 5 -> "orange"
  | 6 -> "purple"
  | 7 -> "cyan"
  | 8 -> "magenta"
  | 9 -> "lime"
  | _ -> "Failed to convert edge to color"
 
(* Dimensions du puzzle *)  
let n, m = 12, 12


(* Fonction pour générer des pièces aléatoires *)
let random_piece () =
  let random_edge () = List.nth colors (Random.int (List.length colors)) in
  {top = random_edge (); right = random_edge (); bottom = random_edge (); left = random_edge ()}

(* Fonction pour générer une bordure aléatoire *)
let random_edge () = List.nth colors (Random.int (List.length colors)
)

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


(* Fonction pour vérifier si une pièce correspond en haut *)
let piece_fits_top puzzle piece i j =
  let gray = 0 in
  match i with
  | 0 -> piece.top = gray
  | _ -> piece.top = (Option.get puzzle.(i - 1).(j)).bottom

(* Fonction pour vérifier si une pièce correspond à gauche *)
let piece_fits_left puzzle piece i j =
  let gray = 0 in
  match j with
  | 0 -> piece.left = gray
  | _ -> piece.left = (Option.get puzzle.(i).(j - 1)).right

(* Fonction pour vérifier si une pièce peut être placée à une position spécifique *)
(* On vérifie uniquement que les bords supérieur et gauche de la piece correspondent aux bords inférieur et droit des pièces adjacentes 
   CAR nous parcouront le puzzle de gauche a droite et de haut en bas*)
let piece_fits puzzle piece i j =
    piece_fits_top puzzle piece i j && piece_fits_left puzzle piece i j

(* Fonction pour faire pivoter une pièce *)
let rotate (piece: piece) : piece =
  { top = piece.left; right = piece.top; bottom = piece.right; left = piece.bottom }
 
(* Fonction pour résoudre le puzzle en utilisant le backtracking *)
let rec solve puzzle pieces i j =
  if i = n && j = 0 then Some puzzle
  else
    let next_i, next_j =
      if j = m - 1 then i + 1, 0
      else i, j + 1
    in
    let rec try_placing_piece piece =
      let fits = piece_fits puzzle piece i j in
      if fits then
        let updated_puzzle = Array.copy puzzle in
        updated_puzzle.(i).(j) <- Some piece;
        match solve updated_puzzle (List.filter ((<>) piece) pieces) next_i next_j with
        | Some _ as solved_puzzle -> solved_puzzle
        | None ->
          let rotated_piece = rotate piece in
          if rotated_piece <> piece then try_placing_piece rotated_piece else None
      else None
    
    in
    let rec try_remaining_pieces = function
      | [] -> None
      | p :: ps ->
          match try_placing_piece p with
          | Some _ as solved_puzzle -> solved_puzzle
          | None -> try_remaining_pieces ps
    in
    try_remaining_pieces pieces

(* Fonction pour mélanger le puzzle *)
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

  (* --------------------------AFFICHAGE-------------------------- *)

  (* Fonction pour afficher une pièce *)
  let print_piece piece =
    Printf.printf "%d %d %d %d ; " piece.top piece.right piece.bottom piece.left
  
  (* Fonction pour afficher le puzzle *)
  let print_puzzle puzzle =
    Array.iter
      (fun row ->
        Array.iter
          (fun piece_option ->
            match piece_option with
            | Some piece -> print_piece piece
            | None -> Printf.printf "......")
          row;
        print_endline "")
      puzzle

  
  (* Fonction pour générer le SVG d'une pièce *)
  let piece_to_svg piece =
    let side_to_polygon side color =
      match side with
      | "top" -> Printf.sprintf "<polygon points=\"0,0 50,50 0,100\" fill=\"%s\" />" color
      | "right" -> Printf.sprintf "<polygon points=\"0,0 50,50 100,0\" fill=\"%s\" />" color
      | "bottom" -> Printf.sprintf "<polygon points=\"100,0 50,50 100,100\" fill=\"%s\" />" color
      | "left" -> Printf.sprintf "<polygon points=\"0,100 50,50 100,100\" fill=\"%s\" />" color
      | _ -> failwith "Invalid side"
    in
  
    let top = side_to_polygon "top" (colors_to_string piece.left) in
    let right = side_to_polygon "right" (colors_to_string piece.top) in
    let bottom = side_to_polygon "bottom" (colors_to_string piece.right) in
    let left = side_to_polygon "left" (colors_to_string piece.bottom) in
  
    Printf.sprintf "<g>\n%s\n%s\n%s\n%s\n</g>" top right bottom left

    (* Fonction pour générer le SVG d'un puzzle *)
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
      
      (* Fonction pour afficher le SVG d'un puzzle *)
      let print_svg puzzle filename =
        pieces_to_svg puzzle filename

(*  --------------------------UTILS--------------------------  *)

(* Fonction pour importer un puzzle depuis un fichier texte *)
let import_puzzle filename =
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
        let nums = List.filter_map (fun s -> try Some (int_of_string s) with Failure _ -> None) (String.split_on_char ' ' part) in
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
let get_pieces_from_puzzle (puzzle: piece option array array) : piece list =
  let pieces = ref [] in
  for i = 0 to Array.length puzzle - 1 do
    for j = 0 to Array.length puzzle.(0) - 1 do
      match puzzle.(i).(j) with
      | Some piece -> pieces := !pieces @ [piece] (* Pour avoir les pieces dans le bon ordre*)
      | None -> ()
    done
  done;
  !pieces

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
  Printf.printf "Temps d'exécution: %f secondes\n" elapsed_time;;

let p2() = 
  let puzzle_115s = import_puzzle "puzzle_115s.txt" in
  print_endline "PUZZLE DEPUIS FICHIER";
  print_puzzle puzzle_115s;
  let start_time = Sys.time () in

  let solution = solve puzzle_115s (get_pieces_from_puzzle puzzle_115s) 0 0 in
  (* let solution = solve_forward_checking puzzle_115s (get_pieces_from_puzzle puzzle_115s) 0 0 in *)
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
  (* p1() *)
  p2();;

let () = main ()





