(* ETERNITY II PROJECT *)

(* Type d'une couleur *)
type color = {r : int; g : int; b : int}

(* Type d'une pièce *)
type piece = {
  colors : color list;
  edges : color array;
  id : int;  
}

(* Type d'un plateau *)
type board = piece option array array

(* Méthode pour créer une pièece *)
let create_piece colors = 
  let id = Random.int 1000 in
  let edges = Array.of_list colors in
  {colors; edges; id} (*Création de la pièce*)

(* Méthode pour créer un plateau *)
let create_board () =
  Array.make_matrix 12 12 None

(* Méthode pour afficher une pièce format SVG*)
let piece_to_svg piece = 
  let svg_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"100\" height=\"100\">\n" in
  let svg_footer = "</svg>\n" in
  let triangle1 = "<polygon points=\"0,0 50,50 0,100\" fill=\"rgb(" ^ (string_of_int (List.nth piece.colors 0).r) ^ "," ^ (string_of_int (List.nth piece.colors 0).g) ^ "," ^ (string_of_int (List.nth piece.colors 0).b) ^ ")\" />\n" in
  let triangle2 = "<polygon points=\"0,0 50,50 100,0\" fill=\"rgb(" ^ (string_of_int (List.nth piece.colors 1).r) ^ "," ^ (string_of_int (List.nth piece.colors 1).g) ^ "," ^ (string_of_int (List.nth piece.colors 1).b) ^ ")\" />\n" in
  let triangle3 = "<polygon points=\"100,0 50,50 100,100\" fill=\"rgb(" ^ (string_of_int (List.nth piece.colors 2).r) ^ "," ^ (string_of_int (List.nth piece.colors 2).g) ^ "," ^ (string_of_int (List.nth piece.colors 2).b) ^ ")\" />\n" in
  let triangle4 = "<polygon points=\"0,100 50,50 100,100\" fill=\"rgb(" ^ (string_of_int (List.nth piece.colors 3).r) ^ "," ^ (string_of_int (List.nth piece.colors 3).g) ^ "," ^ (string_of_int (List.nth piece.colors 3).b) ^ ")\" />\n" in
  svg_header ^ triangle1 ^ triangle2 ^ triangle3 ^ triangle4 ^ svg_footer


(* Création d'une pièce aléatoire *)
let colorsPiece = [{r=255; g=0; b=0}; {r=0; g=255; b=0}; {r=0; g=0; b=255}; {r=255; g=255; b=0}]
let piece = create_piece colorsPiece

let svg = piece_to_svg piece

let oc = open_out "piece.svg";;
Printf.fprintf oc "%s" svg;;
close_out oc;;






