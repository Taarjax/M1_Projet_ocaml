let (m,n) = (6,6) in
let generate_puzzle () =
  let puzzle = create_puzzle m n in
  let pieces = create_pieces m n in
  let rec aux i j =
    if i=m then puzzle
    else if j=n then aux (i+1) 0
    else
      let pieces = List.filter (fun p -> is_valid p puzzle i j) pieces in
      let p = List.nth pieces (Random.int (List.length pieces)) in
      puzzle.(i).(j) <- Some p;
      aux i (j+1)
  in aux 0 0
in

let puzzle = generate_puzzle () in
let solve_puzzle puzzle pieces =
  let rec solve puzzle pieces i j =
    if i=m then Some puzzle
    else if j=n then solve puzzle pieces (i+1) 0
    else
      let pieces = List.filter (fun p -> is_valid p puzzle i j) pieces in
      let rec aux pieces =
        match pieces with
          | [] -> None
          | p::pieces ->
            puzzle.(i).(j) <- Some p;
            let sol = solve puzzle pieces i (j+1) in
            puzzle.(i).(j) <- None;
            match sol with
              | None -> aux pieces
              | Some sol -> Some sol
      in aux pieces
  in solve puzzle pieces 0 0
in
let sol = solve_puzzle puzzle (create_pieces m n) in
print_puzzle sol

(* check if the piece fits in the puzzle *)
let is_valid piece puzzle i j =
  let (m,n) = (Array.length puzzle, Array.length puzzle.(0)) in
  if i>0 then
    match puzzle.(i-1).(j) with
      | None -> true
      | Some p -> piece.(0) = p.(2)
  else if j>0 then
    match puzzle.(i).(j-1) with
      | None -> true
      | Some p -> piece.(3) = p.(1)
  else true
in