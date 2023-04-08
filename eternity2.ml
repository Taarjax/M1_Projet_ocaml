(* eternityII puzzle 12 by 12 solver in ocaml*)
Random.self_init ();

(* Définition des couleurs *)
type color = | Red | Green | Blue | Yellow | Orange | Purple | Brown | Pink | Cyan | Grey ;;
let colors = [| Red; Green; Blue; Yellow; Orange; Purple; Brown; Pink; Cyan |];;  
let grey = Grey;;
let hex_of_color = function | Red -> "#ff0000" | Green -> "#00ff00" | Blue -> "#0000ff" | Yellow -> "#ffff00" | Orange -> "#ff8000" | Purple -> "#8000ff" | Brown -> "#804000" | Pink -> "#ff00ff" | Cyan -> "#00ffff" | Grey -> "#808080" ;;
let string_of_color = function | Red -> "red" | Green -> "green" | Blue -> "blue" | Yellow -> "yellow" | Orange -> "orange" | Purple -> "purple" | Brown -> "brown" | Pink -> "pink" | Cyan -> "cyan" | Grey -> "grey" ;;


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
  cell: cell ref option;     (* Cellule actuellement occupée par la pièce *)
}

(* Définition de la structure cell pour représenter une cellule du plateau *)
and cell = {
  mutable piece: piece option; (* Pièce actuellement placée dans la cellule *)
  mutable next: cell option;   (* Cellule suivante dans le sens des aiguilles d'une montre *)
  mutable top: cell option;    (* Cellule voisine au-dessus *)
  mutable right: cell option;  (* Cellule voisine à droite *)
  mutable bottom: cell option; (* Cellule voisine en dessous *)
  mutable left: cell option;   (* Cellule voisine à gauche *)
};;

(* Définition de la structure board pour représenter le plateau *)
type board = { 
  pieces : piece array;  (* Tableau des pièces du puzzle *)
  cells : cell array;   (* Tableau des cellules du plateau *)
  mutable placed_count : int;    (* Nombre de pièces déjà placées sur le plateau *)
  mutable remaining_count : int; (* Nombre de pièces restantes à placer *)
};;


(* Renvoie une couleur aléatoire sauf le gris *)
let random_color () = colors.(Random.int (Array.length colors));;
Printf.printf "%s " (string_of_color (random_color ()));;


let last_piece_id = ref 0 
let next_piece_id () = 
  last_piece_id := !last_piece_id + 1;
  !last_piece_id;;

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

let print_piece piece = 
  Printf.printf "ID : %d\n" piece.id;
  for i = 0 to 3 do
    Printf.printf "Top : %s, Right : %s, Bottom : %s, Left : %s\n" (string_of_color piece.rots.(i).top) (string_of_color piece.rots.(i).right) (string_of_color piece.rots.(i).bottom) (string_of_color piece.rots.(i).left);
  done;;

(* Fonction pour faire pivoter une structure sides de n rotations *)
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

(* let piece = create_piece "corner";;
let piece2 = create_piece "edge";;
let piece3 = create_piece "inside";;
print_piece piece;;
print_piece piece2;;
print_piece piece3;; *)

(* Fonction pour créer un plateau de taille width * height *)
let create_board m n = 
  let nb_corner = 4 in 
  let nb_edge = 2 * (m + n - 4) in

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
    ) in
    pieces;;

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
    let piece_svg = piece_to_svg piece in
    Printf.sprintf "<g id=\"piece-%d\" transform=\"translate(%d, %d)\">%s</g>"
      piece.id x y piece_svg
  ) pieces in

  let svg = header ^ (String.concat "\n" (Array.to_list body)) ^ footer in
  let oc = open_out filename in
  output_string oc svg;
  close_out oc
;;

let board = create_board 4 4;;
Array.iter print_piece board;;
pieces_to_svg board 4 4 "puzzle_pieces.svg";



