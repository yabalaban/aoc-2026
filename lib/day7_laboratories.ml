(* move to utils/whatever *)

module IntSet = Set.Make(Int);;
module IntMap = Map.Make(Int);;

let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines;;

let entrances c line = 
  let f acc (i, ch) = match ch with 
    | ch when ch == c -> IntSet.add i acc
    | _ -> acc in
  line |> String.to_seqi |> Seq.fold_left f IntSet.empty;;

let start = entrances 'S';;
let splitters = entrances '^';;
let get_or_default k default map = Option.value (IntMap.find_opt k map) ~default;;
let set_of_map_keys map = IntMap.fold (fun k _ acc -> IntSet.add k acc) map IntSet.empty;;
let apply f set map = set |> IntSet.to_list |> List.fold_left (fun m k -> f k m) map;;
let distribute k map = 
  let v = IntMap.find k map in
  let pv = get_or_default (k - 1) 0 map in
  let nv = get_or_default (k + 1) 0 map in
  let map = IntMap.add (k - 1) (pv + v) map in 
  let map = IntMap.add (k + 1) (nv + v) map in 
  IntMap.remove k map;;

let solve (input: string list): (int * (int IntMap.t)) = 
  let s = input |> List.hd |> start in 
  let f (acc, dist) l = 
    let hit = l |> splitters |> IntSet.inter (set_of_map_keys dist) in 
    let dist = dist |> apply distribute hit in 
    (acc + (IntSet.cardinal hit), dist) in 
  let dist = s |> IntSet.to_list |> List.map (fun x -> (x, 1)) |> IntMap.of_list in 
  input |> List.tl |> List.fold_left f (0, dist);;

let solve_first (file_name: string): int =
  solve (read_lines file_name) |> fst;;

let solve_second (file_name: string): int =
  solve (read_lines file_name) |> snd |> IntMap.to_list |> List.split |> snd |> List.fold_left ( + ) 0;;