(* eternityII puzzle 12 by 12 solver in ocaml*)
Random.self_init ();

(* Définition des couleurs *)
type color = | Red | Green | Blue | Yellow | Orange | Purple | Brown | Pink | Cyan | Grey ;;
let colors = [| Red; Green; Blue; Yellow; Orange; Purple; Brown; Pink; Cyan |];;  
let grey = Grey;;
let hex_of_color = function | Red -> "#ff0000" | Green -> "#00ff00" | Blue -> "#0000ff" | Yellow -> "#ffff00" | Orange -> "#ff8000" | Purple -> "#8000ff" | Brown -> "#804000" | Pink -> "#ff00ff" | Cyan -> "#00ffff" | Grey -> "#808080" ;;
let string_of_color = function | Red -> "red" | Green -> "green" | Blue -> "blue" | Yellow -> "yellow" | Orange -> "orange" | Purple -> "purple" | Brown -> "brown" | Pink -> "pink" | Cyan -> "cyan" | Grey -> "grey" ;;


(* type color = | Red | Grey ;;
let colors = [| Red |];;  
let grey = Grey;;
let hex_of_color = function | Red -> "#ff0000"  | Grey -> "#808080" ;;
let string_of_color = function | Red -> "red" | Grey -> "grey" ;; *)


(* Définition de la structure sides pour représenter les 4 côtés d'une pièce *)
type sides = {
  top: color;      (* Valeur du côté supérieur *)
  right: color;    (* Valeur du côté droit *)
  bottom: color;   (* Valeur du côté inférieur *)
  left: color;     (* Valeur du côté gauche *)
};;

(* Définition de la structure piece pour représenter une pièce du puzzle *)
type piece = {
  id: int;            (* Identifiant unique de la pièce *)
  rots: sides array;  (* Tableau des différentes configurations de la pièce *)
  mutable cell: cell ref option;     (* Cellule actuellement occupée par la pièce *)
}

(* Définition de la structure cell pour représenter une cellule du plateau *)
and cell = {
  idCell :int;
  mutable piece: piece option; (* Pièce actuellement placée dans la cellule *)
  mutable next: cell option;   (* Cellule suivante dans le sens des aiguilles d'une montre *)
  mutable top: cell option;    (* Cellule voisine au-dessus *)
  mutable right: cell option;  (* Cellule voisine à droite *)
  mutable bottom: cell option; (* Cellule voisine en dessous *)
  mutable left: cell option;   (* Cellule voisine à gauche *)
};;

(* Définition de la structure board pour représenter le plateau *)
type board = { 
  mutable pieces : piece array;  (* Tableau des pièces du puzzle *)
  cells : cell array;   (* Tableau des cellules du plateau *)
  mutable placed_count : int;    (* Nombre de pièces déjà placées sur le plateau *)
  mutable remaining_count : int; (* Nombre de pièces restantes à placer *)
};;


(* Renvoie une couleur aléatoire sauf le gris *)
let random_color () = colors.(Random.int (Array.length colors));;
Printf.printf "%s " (string_of_color (random_color ()));;

(* Méthode pour créer un identifiant unique pour une pièce *)
let last_piece_id = ref 0 
let next_piece_id () = 
  last_piece_id := !last_piece_id + 1;
  !last_piece_id;;

let last_cell_id = ref 0
let next_cell_id () = 
  last_cell_id := !last_cell_id + 1;
  !last_cell_id;;

(* Méthode pour créer une pièce *)
let create_piece type_piece : piece = 
  match type_piece with
  | "corner" -> 
    let c1 = random_color () in
    let c2 = random_color () in
    let id = next_piece_id () in
    let rots = [| {top = c1; right = c2; bottom = grey; left = grey};
                  {top = grey; right = c1; bottom = c2; left = grey};
                  {top = grey; right = grey; bottom = c1; left = c2};
                  {top = c2; right = grey; bottom = grey; left = c1} |] in
  {id = id; rots = rots; cell = None;}
  | "edge" ->
    let c1 = random_color () in 
    let c2 = random_color () in
    let c3 = random_color () in
    let id = next_piece_id () in
    let rots = [| {top = grey ; right = c1; bottom = c2; left = c3};
                  {top = c3; right = grey; bottom = c1; left = c2};
                  {top = c2; right = c3; bottom = grey; left = c1};
                  {top = c1; right = c2; bottom = c3; left = grey} |] in
  {id = id; rots = rots; cell = None;}
                  | "inside" -> 
    let c1 = random_color () in 
    let c2 = random_color () in
    let c3 = random_color () in
    let c4 = random_color () in
    let id = next_piece_id () in
    let rots = [| {top = c1; right = c2; bottom = c3; left = c4};
                  {top = c4; right = c1; bottom = c2; left = c3};
                  {top = c3; right = c4; bottom = c1; left = c2};
                  {top = c2; right = c3; bottom = c4; left = c1} |] in
    {id = id; rots = rots; cell = None;}
    | _ -> failwith "Invalid piece type";;

(* Afficher une piece *)
let print_piece piece = 
  Printf.printf "ID : %d\n" piece.id;
  for i = 0 to 3 do
    Printf.printf "Top : %s, Right : %s, Bottom : %s, Left : %s\n" (string_of_color piece.rots.(i).top) (string_of_color piece.rots.(i).right) (string_of_color piece.rots.(i).bottom) (string_of_color piece.rots.(i).left);
  done;;

(* Fonction pour faire pivoter une structure sides de n rotations *)


(* Fonction pour créer un plateau de m lignes et n colonnes *)
let create_board m n : board = 
  let nb_corner = 4 in 
  let nb_edge = 2 * (m + n - 4) in

  (* On créer les pièces du plateau *)
  let pieces = Array.init (n * m) (fun i -> 
    let type_piece = 
      if i < nb_corner then 
        "corner"
      else if i < nb_corner + nb_edge then 
        "edge"
      else 
        "inside" 
      in
    create_piece type_piece
    )in

  (* On créer les cellules du plateau *)
  let cells = Array.init (n * m) (fun i -> 
    let id = next_cell_id () in
    let cell = {idCell = id; piece = None; next = None; top = None; right = None; bottom = None; left = None;} in
    cell
    ) in
  (* On relie les cellules entre elles *)
  for i = 0 to (n * m) - 1 do
    let cell = cells.(i) in
    if i mod n <> 0 then cell.left <- Some cells.(i - 1);
    if i mod n <> n - 1 then cell.right <- Some cells.(i + 1);
    if i >= n then cell.top <- Some cells.(i - n);
    if i < (n * m) - n then cell.bottom <- Some cells.(i + n);
  done;

  (* On relie les cellules aux pièces *)
  for i = 0 to (n * m) - 1 do
    let cell = cells.(i) in
    let piece = pieces.(i) in
    cell.piece <- Some piece;
    piece.cell <- Some (ref cell)
  done;
  {pieces = pieces; cells = cells; placed_count = 0; remaining_count = (n * m);}
;;

let piece_to_svg piece = 
  let color_to_hex c = hex_of_color c in
  let side_to_polygon side =
    match side with
    | "top" -> Printf.sprintf "<polygon points=\"0,0 50,50 0,100\" fill=\"%s\" />" (color_to_hex piece.rots.(0).top)
    | "right" -> Printf.sprintf "<polygon points=\"0,0 50,50 100,0\" fill=\"%s\" />" (color_to_hex piece.rots.(0).right)
    | "bottom" -> Printf.sprintf "<polygon points=\"100,0 50,50 100,100\" fill=\"%s\" />" (color_to_hex piece.rots.(0).bottom)
    | "left" -> Printf.sprintf "<polygon points=\"0,100 50,50 100,100\" fill=\"%s\" />" (color_to_hex piece.rots.(0).left)
    | _ -> failwith "Invalid side"
  in

  let top = side_to_polygon "top" in
  let right = side_to_polygon "right" in
  let bottom = side_to_polygon "bottom" in
  let left = side_to_polygon "left" in

  (* pièce *)  
  Printf.sprintf "<g id=\"piece-%d\">\n%s\n%s\n%s\n%s\n</g>" piece.id top right bottom left
;;


(* Fonction pour créer un fichier SVG contenant les pièces *)
let pieces_to_svg pieces width height filename =
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">" in
  let footer = "</svg>" in
  let piece_width = 100 in
  let piece_height = 100 in

  let body = Array.mapi (fun i piece ->
    let x = (i mod width) * piece_width in
    let y = (i / width) * piece_height in
    let cellule_svg = piece_to_svg piece in
    match piece.cell with
    | Some cell_ref ->
      let cell = !cell_ref in
      Printf.sprintf "<g id=\"cell-%d\" transform=\"translate(%d, %d)\">%s</g>"
        cell.idCell x y cellule_svg
    | None -> "" (* Ne pas afficher la pièce si elle n'a pas de cellule associée *)
  ) pieces in

  let svg = header ^ (String.concat "\n" (Array.to_list body)) ^ footer in
  let oc = open_out filename in
  output_string oc svg;
  close_out oc
;;

let can_be_next_to piece1 side piece2 =
  match side with
  | "top" -> piece1.rots.(0).bottom = piece2.rots.(0).top
  | "right" -> piece1.rots.(0).right = piece2.rots.(0).left
  | "bottom" -> piece1.rots.(0).top = piece2.rots.(0).bottom
  | "left" -> piece1.rots.(0).left = piece2.rots.(0).right
  | _ -> failwith "Invalid side"
;;


let board = create_board 2 2;;
pieces_to_svg board.pieces 2 2 "pieces.svg";;

(* On affichage chaque pièce du puzzle et sa cellulle associée *)
let rotate s rot =
  let ret = s in
  for i = 1 to rot do
    let tmp = ret.top in
    ret.top <- ret.left;
    ret.left <- ret.bottom;
    ret.bottom <- ret.right;
    ret.right <- tmp;
  done;
  ret;;
  

(* 
let next_cell current_cell =
  match current_cell.next with
  | Some next_cell -> next_cell
  | None -> failwith "No next cell"
  let is_complete board =
    board.remaining_count = 0
  
  let board_is_complete board =
    Array.for_all (fun cell -> cell.piece <> None) board.cells
  
  let find_next_empty_cell board =
    let rec find_empty_cell_helper index =
      if index < Array.length board.cells then
        if board.cells.(index).piece = None then Some board.cells.(index)
        else find_empty_cell_helper (index + 1)
      else None
    in
    find_empty_cell_helper 0

let rec solve_puzzle board =
  let fits current_cell piece =
    let check_side side =
      match side with
      | "top" -> (match current_cell.top with
                  | None -> true
                  | Some top_cell -> (match top_cell.piece with
                                      | None -> true
                                      | Some top_piece -> can_be_next_to top_piece "bottom" piece))
      | "right" -> (match current_cell.right with
                    | None -> true
                    | Some right_cell -> (match right_cell.piece with
                                          | None -> true
                                          | Some right_piece -> can_be_next_to right_piece "left" piece))
      | "bottom" -> (match current_cell.bottom with
                     | None -> true
                     | Some bottom_cell -> (match bottom_cell.piece with
                                            | None -> true
                                            | Some bottom_piece -> can_be_next_to bottom_piece "top" piece))
      | "left" -> (match current_cell.left with
                   | None -> true
                   | Some left_cell -> (match left_cell.piece with
                                        | None -> true
                                        | Some left_piece -> can_be_next_to left_piece "right" piece))
      | _ -> failwith "Invalid side"
    in
    check_side "top" && check_side "right" && check_side "bottom" && check_side "left"
  in

  let rec try_fit current_cell remaining_pieces =
    match remaining_pieces with
    | [] -> false
    | piece :: remaining ->
      let rotations = Array.length piece.rots in
      let rec try_rotations rot =
        if rot < rotations then
          let _ = piece.rots.(0) <- piece.rots.(rot) in
          if fits current_cell piece then (
            current_cell.piece <- Some piece;
            piece.cell <- Some (ref current_cell);
            if is_complete board then true
            else (
              let next = next_cell current_cell in
              if try_fit next remaining_pieces then true
              else (
                current_cell.piece <- None;
                piece.cell <- None;
                try_rotations (rot + 1)
              )
            )
          ) else try_rotations (rot + 1)
        else false
      in
      try_rotations 0 || try_fit current_cell remaining 
    in

    if board_is_complete board then true
    else 
      let current_cell = find_next_empty_cell board in
      match current_cell with
      | None -> false
      | Some current_cell -> try_fit current_cell (Array.to_list board.pieces)
  ;;


  let generate_puzzles_until_solved () =
    let rec helper () = 
      let pieces = create_board 2 2 in 
      let board = {pieces = pieces; cells = Array.make 4 {piece = None; top = None; right = None; bottom = None; left = None; next = None}; placed_count = 0; remaining_count = 4} in
      if solve_puzzle board then (
        let solution = pieces in
        pieces_to_svg solution 2 2 "solution.svg";
        solution
      )
      else helper ()
    in
    helper ()
  ;;

  let solution = generate_puzzles_until_solved () in
  let _ = pieces_to_svg solution 2 2 "solution.svg" in
  ()
;; *)





(* let rec generate_corner_pieces colors = 
  match colors with
  | [] -> []
  | hd::tl -> 
    let remaining_colors = List.filter (fun c -> c != hd) tl in
    let corner_pieces = List.fold_left (fun acc color -> {id = next_piece_id (); rots = [| {top = hd; right = color; bottom = grey; left = grey}; {top = grey; right = hd; bottom = color; left = grey}; {top = grey; right = grey; bottom = hd; left = color}; {top = color; right = grey; bottom = grey; left = hd} |]; cell = None;} :: acc) [] remaining_colors in
    corner_pieces @ (generate_corner_pieces tl);;

let rec generate_edge_pieces colors =
  let rec helper remaing_colors side1 side2 acc = 
    match remaing_colors with
    | [] -> acc
    | hd :: tl ->
      let edge_piece = {id = next_piece_id (); rots = [| {top = side1; right = hd; bottom = side2; left = grey}; {top = grey; right = side1; bottom = hd; left = side2}; {top = side2; right = grey; bottom = side1; left = hd}; {top = hd; right = side2; bottom = grey; left = side1} |]; cell = None;} in
      helper tl side1 side2 (edge_piece :: acc)
  in
  match colors with
  | [] -> []
  | hd :: tl ->
    let remaining_colors = List.filter (fun c -> c != hd) tl in
    let edge_pieces = List.fold_left (fun acc color -> helper remaining_colors hd color acc) [] remaining_colors in
    edge_pieces @ (generate_edge_pieces tl)

let rec generate_inside_pieces colors = 
  let rec helper remaining_colors side1 side2 side3 acc =
    match remaining_colors with
    | [] -> acc
    | hd :: tl ->
      let inside_piece = {id = next_piece_id (); rots = [| {top = side1; right = hd; bottom = side2; left = side3}; {top = side3; right = side1; bottom = hd; left = side2}; {top = side2; right = side3; bottom = side1; left = hd}; {top = hd; right = side2; bottom = side3; left = side1} |]; cell = None;} in
      helper tl side1 side2 side3 (inside_piece :: acc)
  in
  match colors with
  | [] -> []
  | hd :: tl ->
    let remaining_colors = List.filter (fun c -> c != hd) tl in
    let inside_pieces = List.fold_left (fun acc color -> helper remaining_colors hd color (List.hd remaining_colors) acc) [] remaining_colors in
    inside_pieces @ (generate_inside_pieces tl)  



(* Méthode qui génère toutes les combinaisons de couleur pour un plateau de taille n*m *)
let generate_all_combinations m n = 
  let nb_corner = 4 in
  let nb_edge = 2 * (m + n - 4) in
  let nb_inside = (m - 2) * (n - 2) in

  let coner_pieces = generate_corner_pieces (Array.to_list colors) in
  let edge_pieces = generate_edge_pieces (Array.to_list colors) in
  let inside_pieces = generate_inside_pieces (Array.to_list colors) in

  let rec take_n lst n = 
    match lst with 
    | [] -> []
    | hd::tl -> if n = 0 then 
                  []
                else 
                  hd::(take_n tl (n-1))
    in
    
    let shuffled_corner_pieces = List.sort (fun _ _ -> (Random.int 3) - 1 ) coner_pieces in
    let shuffled_edge_pieces = List.sort (fun _ _ -> (Random.int 3) - 1 ) edge_pieces in
    let shuffled_inside_pieces = List.sort (fun _ _ -> (Random.int 3) - 1 ) inside_pieces in

    (take_n shuffled_corner_pieces nb_corner) @ (take_n shuffled_edge_pieces nb_edge) @ (take_n shuffled_inside_pieces nb_inside);;


let corner_pieces = generate_corner_pieces (Array.to_list colors);;
let edge_pieces = generate_edge_pieces (Array.to_list colors);;
let inside_pieces = generate_inside_pieces (Array.to_list colors);;
let all_pieces = corner_pieces @ edge_pieces @ inside_pieces;;

let shuffle lst = 
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond;;


let take n lst =
  let rec aux acc x =
    if List.length acc = n then List.rev acc
    else match x with
          | [] -> List.rev acc
          | hd :: tl -> aux (hd :: acc) tl
  in aux [] lst  
let create_board_with_all_combination m n = 
  let shuffled_pieces = shuffle all_pieces in
  let board = take (m*n) shuffled_pieces in
  Array.of_list board;;

let board = create_board_with_all_combination 4 4;;
Array.iter print_piece board;;
pieces_to_svg board 4 4 "board.svg";; *)

