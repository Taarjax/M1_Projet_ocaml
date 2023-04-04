(* ETERNITY II PROJECT *)
open Printf;;


(* ----------------------------------------------------------------------------------
                                  Définition des types
    ---------------------------------------------------------------------------------- *)
type color = 
  |  Red 
  | Green
  | Blue
  | Yellow
  | Orange
  | Purple
  | Grey 
  | Brown
  | Pink
  | Cyan;;

let string_of_color c = 
  match c with
  | Red -> "Red"
  | Green -> "Green"
  | Blue -> "Blue"
  | Yellow -> "Yellow"
  | Orange -> "Orange"
  | Purple -> "Purple"
  | Grey -> "Grey"
  | Brown -> "Brown"
  | Pink -> "Pink"
  | Cyan -> "Cyan";;

let color_to_rgb color = 
  match color with
  | Red -> "255,0,0"
  | Green -> "0,255,0"
  | Blue -> "0,0,255"
  | Yellow -> "255,255,0"
  | Orange -> "255,165,0"
  | Purple -> "128,0,128"
  | Grey -> "128,128,128"
  | Brown -> "165,42,42"
  | Pink -> "255,192,203"
  | Cyan -> "0,255,255";;

(* ----------------------------------------------------------------------------------
                                  Création des pièces
    ---------------------------------------------------------------------------------- *)
(* Type d'une pièce *)
type piece = {
  edge_colors : color list;
  id : int;  
};;

(* Une piece ne peut pas avoir le même identifiant *)
let last_piece_id = ref 0 
let next_piece_id () = 
  last_piece_id := !last_piece_id + 1;
  !last_piece_id;;

(* Méthode pour créer une pièce de centre *)
let piece_counter = ref 0;;
let create_piece c1 c2 c3 c4 : piece =
  let piece = { id = !piece_counter; edge_colors = [c1; c2; c3; c4];} in
  piece_counter := !piece_counter + 1;
  piece;;


(* Méthode pour afficher une pièce format SVG*)
(* Couleur GAUCHE HAUT DROITE BAS *)
let piece_to_svg piece = 
  let svg_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"100\" height=\"100\">\n" in
  let svg_footer = "</svg>\n" in
  let triangle1 = "<polygon points=\"0,0 50,50 0,100\" fill=\"rgb(" ^ (color_to_rgb (List.nth piece.edge_colors 0)) ^ ")\" />\n" in
  let triangle2 = "<polygon points=\"0,0 50,50 100,0\" fill=\"rgb(" ^ (color_to_rgb (List.nth piece.edge_colors 1)) ^ ")\" />\n" in
  let triangle3 = "<polygon points=\"100,0 50,50 100,100\" fill=\"rgb(" ^ (color_to_rgb (List.nth piece.edge_colors 2)) ^ ")\" />\n" in
  let triangle4 = "<polygon points=\"0,100 50,50 100,100\" fill=\"rgb(" ^ (color_to_rgb (List.nth piece.edge_colors 3)) ^ ")\" />\n" in
  svg_header ^ triangle1 ^ triangle2 ^ triangle3 ^ triangle4 ^ svg_footer;;

let create_piece_svg piece = 
  let file_name = "piece_" ^ string_of_int piece.id ^ ".svg" in
  let svg = piece_to_svg piece in
  let oc = open_out file_name in
  Printf.fprintf oc "%s" svg;
  close_out oc;;

(* Affichage des propriétés d'une pièce *)
let print_piece piece = 
  Printf.printf "ID : %d\n" piece.id;
  Printf.printf "Couleurs : %s\n" (String.concat ", " (List.map string_of_color piece.edge_colors));;


(* ----------------------------------------------------------------------------------
                                  Création du plateau
    ---------------------------------------------------------------------------------- *)
(* Type d'un plateau *)
type board = piece option array array ;;

(* Méthode pour créer un plateau *)
let create_board n m : board =
  Array.make_matrix n m None;;
 
(* Nombre de coins : 4
   Nombre de bord : 4 * (n - 2) 
   Nombre de pièce intérieurs : (n - 2) * (n - 2) 
*)



(* ----------------------------------------------------------------------------------
                                 GETTER & SETTER
    ---------------------------------------------------------------------------------- *)

(* Méthode pour retourner une piece du plateau à la position (x,y) *)
let get_piece board x y : piece option = 
  board.(x).(y);;

(* Méthode pour modifier la pièce à la position (x,y) *)
let set_piece board x y piece : unit = 
  board.(x).(y) <- Some piece;;


(* ----------------------------------------------------------------------------------
                                MANIPULATION DES PIECES
  ---------------------------------------------------------------------------------- *)

(* Méthode pour rotationner une pièece de 1 2 ou 3 quart 
  Les couleurs seront donc décalées de 1 2 ou 3 dans le sens horaire *)
let rotate_piece piece n : piece =
  let new_edge_colors = match n with 
  | 1 -> [List.nth piece.edge_colors 3; List.nth piece.edge_colors 0; List.nth piece.edge_colors 1; List.nth piece.edge_colors 2]
  | 2 -> [List.nth piece.edge_colors 2; List.nth piece.edge_colors 3; List.nth piece.edge_colors 0; List.nth piece.edge_colors 1]
  | 3 -> [List.nth piece.edge_colors 1; List.nth piece.edge_colors 2; List.nth piece.edge_colors 3; List.nth piece.edge_colors 0]
  | _ -> failwith "Invalid rotation number"
  in { id = piece.id; edge_colors = new_edge_colors }


let flip_vertical piece : piece = 
  { id = piece.id; edge_colors = [List.nth piece.edge_colors 2; List.nth piece.edge_colors 1; List.nth piece.edge_colors 0; List.nth piece.edge_colors 3] }

let flip_horizontal piece : piece = 
  { id = piece.id; edge_colors = [List.nth piece.edge_colors 0; List.nth piece.edge_colors 3; List.nth piece.edge_colors 2; List.nth piece.edge_colors 1] }

  
  


(* ----------------------------------------------------------------------------------
                                 TESTS
    ---------------------------------------------------------------------------------- *)

(* On fait tourner la piece jusqu'a ce qu'elle revienne a son état intiale.
   Si => true => la méthode rotate_piece fonctionne correctement
   Sinon => false *)
   (* OK *)
let is_rotation_valid piece1 piece2 : bool = 
  let rec aux piece1 piece2 i = 
    if i = 4 then false
    else if piece1.edge_colors = piece2.edge_colors then true
    else aux piece1 (rotate_piece piece2 1) (i + 1)
  in aux piece1 piece2 0;;

  (* OK *)
let is_flip_vertical_valid piece1 piece2 : bool = 
  piece1.edge_colors = (flip_vertical piece2).edge_colors;;

  (* OK *)
let is_flip_horizontal_valid piece1 piece2 : bool =
  piece1.edge_colors = (flip_horizontal piece2).edge_colors;;


(* ----------------------------------------------------------------------------------
                                 MAIN
    ---------------------------------------------------------------------------------- *)
    

(* let piece2 = rotate_piece piece1 1;; *)
(* let piece2 = flip_vertical piece1;; *)
(* let piece2 = flip_horizontal piece1;; *)
print_piece piece1;;
print_piece piece2;;
create_piece_svg piece1;;
create_piece_svg piece2;;


(* Test rotation de la piece 1 et 2 *)
(* let result = is_rotation_valid piece1 piece2 in
Printf.printf "Is rotation valid: %b\n" result;; *)

(* Test flip vertical de la piece 1 et 2 *)
(* let result = is_flip_vertical_valid piece1 piece2 in
Printf.printf "Is flip vertical valid: %b\n" result;; *)

(* Test flip horizontal de la piece 1 et 2 *)
(* let result = is_flip_horizontal_valid piece1 piece2 in
Printf.printf "Is flip horizontal valid: %b\n" result;; *)