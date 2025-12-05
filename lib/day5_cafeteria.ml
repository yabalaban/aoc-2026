(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines;;

let range (line: string): int * int = 
  line |> String.split_on_char '-' |> List.map int_of_string |> Array.of_list |> (fun x -> x.(0), x.(1));;
  
let id (line: string): int = line |> int_of_string;;

let split_by_val (v: 'b) (lst: 'a list): 'a list * 'a list =
  let rec loop a b = match a with 
  | [] -> (List.rev b, [])
  | x :: xs when x = v -> (List.rev b, xs)
  | x :: xs -> loop xs (x :: b) in
  loop lst [];;

let solve_first (file: string): int = 
  let is_fresh (ranges: (int * int) list) (id: int) =    
    let inside (range: int * int) (id: int) = (fst range) <= id && id <= (snd range) in 
    let rec loop lst = match lst with 
    | [] -> false
    | x :: xs -> (inside x id) || (loop xs) in
    loop ranges in 

  let ranges, ids = file |> read_lines |> (split_by_val "") |> (fun x -> (List.map range (fst x), List.map id (snd x))) in 
  ids |> List.map (is_fresh ranges) |> List.filter (fun x -> x) |> List.length;;

let solve_second (file: string): int = 
  let ranges = file |> read_lines |> (split_by_val "") |> (fun x -> List.map range (fst x)) in 
  let cmp (x, y) (x', y') = let x_ = compare x x' in
    if x_ != 0 then x_ else compare y y' in  
  let red acc (x, y) = match acc with 
  | (x', y') :: xs when x <= y' -> (x', max y y') :: xs 
  | _ -> (x, y) :: acc in   
  let range (x, y) = y - x + 1 in 
  ranges |> List.sort cmp |> List.fold_left red [] |> List.map range |> List.fold_left (+) 0;;