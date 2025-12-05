module IntSet = Set.Make(Int);;

let indices n = List.init n (fun x -> x);;

let choice (pos, c) = if c == '@' then Some pos else None;;

let cartesian lhs rhs =
  let inner x = List.map (fun y -> (x, y)) rhs in
  let outer x = List.map inner x in
  List.concat (outer lhs);;

let read_grid (file_name : string) : IntSet.t * int * int =
  let lines = In_channel.with_open_text file_name In_channel.input_lines in
  let (w, h) = List.hd lines |> String.length, List.length lines in
  let ind = indices (w * h) in
  let grid = String.concat "" lines |> String.to_seq |> List.of_seq |> List.combine ind |> List.filter_map choice in
  (IntSet.of_list grid, w, h);;

let solve (file_name : string) (early_stop: bool) : int =
  let (grid, w, h) = read_grid file_name in
  let index p (x, y) = p + x + y * w in
  let filter p np = np >= 0 && np < w * h && p != np && not (np mod w = w - 1 && p mod w = 0) && not (np mod w = 0 && p mod w = w - 1) in
  let explode p = cartesian [-1; 0; 1] [-1; 0; 1] |> List.map (index p) |> List.filter (filter p) |> (fun x -> (p, x)) in

  let rec loop grid acc =
    let pred (el, l) = l |> List.map (fun x -> if IntSet.mem x grid then 1 else 0) |> List.fold_left (+) 0 |> (fun x -> if x < 4 then Some el else None) in
    let ngrid = grid |> IntSet.elements |> List.map explode |> List.filter_map pred |> IntSet.of_list in
    let inter = IntSet.inter grid ngrid in
    let length = inter |> IntSet.to_list |> List.length in
    match (length, early_stop) with
    | 0, _ -> acc
    | _, true -> length
    | _, _ -> loop (IntSet.diff grid inter) (acc + length) in

  loop grid 0;;

let solve_first (file_name : string) : int = solve file_name true;;
let solve_second (file_name : string) : int = solve file_name false;;