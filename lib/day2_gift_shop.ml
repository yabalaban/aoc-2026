(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let parse (line : string) : (string * string) list = 
  let ranges = String.split_on_char ',' line in 
  let splitr = fun x -> 
      let split = String.split_on_char '-' x in
      List.hd split, List.tl split |> List.hd in  
  List.map splitr ranges;; 
      
let solve (f: string * string -> int list) (file: string) : int =
  let lines = read_lines file in
  let ranges = List.map parse lines |> List.hd in 
  List.map f ranges |> List.flatten |> List.sort_uniq compare |> List.fold_left (+) 0;;

let make_invalid_id (n : int) (x: int) : int = 
  let s = string_of_int x in
  List.init n (fun _ -> s) |> String.concat "" |> int_of_string;;

let invalid_id_in_range (range: int * int) (x: int) : bool = 
  fst range <= x && x <= snd range;; 

let invalid_ids_of_len (range: int * int) (l: int * int): int list = 
  let min = List.init (fst l) (fun x -> if x == 0 then "1" else "0") |> String.concat "" |> int_of_string in 
  let max = List.init (fst l) (fun _ -> "9") |> String.concat "" |> int_of_string in 
  let candidates = List.init (max - min + 1) (fun x -> x + min) in 
  List.map (make_invalid_id (snd l)) candidates |> List.filter (invalid_id_in_range range);;

let invalid_ids (f: string * string -> (int * int) list) (range: string * string) : int list = 
  let irange = (int_of_string (fst range)), (int_of_string (snd range)) in 
  let lens = f range in
  List.map (invalid_ids_of_len irange) lens |> List.flatten;; 
    
let solve_first (file: string) : int = 
  let lens = fun range -> 
    let l = fun x -> String.length x in 
    let llen, ulen = l (fst range), l (snd range) in 
     List.init (ulen - llen + 1) (fun x -> x + llen) |> List.filter (fun x -> x mod 2 == 0) |> List.map (fun x -> (x / 2, 2)) in
  solve (invalid_ids lens) file;;

let solve_second (file: string) : int = 
  let divisors n =
    let rec aux i acc =
      if i > n then List.rev acc
      else if n mod i = 0 then aux (i + 1) (i :: acc)
      else aux (i + 1) acc
    in
    aux 1 [] in
  let lens = fun range -> 
    let l = fun x -> String.length x in 
    let llen, ulen = l (fst range), l (snd range) in 
    let f x = divisors(x) |> List.filter (fun d -> d != 1) |> List.map (fun d -> (x / d, d)) in 
    let res = List.init (ulen - llen + 1) (fun x -> x + llen) |> List.map f |> List.flatten in
    res; in
  solve (invalid_ids lens) file;;