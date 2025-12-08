module IntMap = Map.Make(Int);;

let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines;;

let [@warning "-8"] parse line = line |> String.split_on_char ',' |> List.map int_of_string |> function | x::y::z::[] -> (x, y, z);;

let ipow2 x = x * x;;
let distance (x, y, z) (x', y', z') = ipow2 (x - x') + ipow2 (y - y') + ipow2 (z - z');;
let cartesian l l' = List.concat_map (fun x -> List.map (fun y -> (x, y)) l') l;;
let counter arr = Array.fold_left (fun acc x -> IntMap.update x (function | None -> Some 1 | Some v -> Some (v + 1)) acc) IntMap.empty arr;;
let update uf v v' = uf |> Array.iteri (fun (i) (v_) -> if v_ = v then uf.(i) <- v');;
let stop uf (x, _, _) (x', _, _) = uf |> counter |> IntMap.cardinal |> (fun l -> if l == 1 then let () = Printf.printf "%d " (x * x') in raise Exit;);;

let solve n topn lines full_stop =
  let len = List.length lines in
  let uf = Array.init len (fun x -> x) in
  let comp ((_, p1), (_, p2)) ((_, p1'), (_, p2'))  = compare (distance p1 p2) (distance p1' p2') in
  let comp2 (_, s) (_, s') = -(compare s s') in
  let coords = List.map parse lines |> List.combine (Array.to_list uf) |> (fun l -> cartesian l l) |> List.filter (fun ((_, x), (_, y)) -> x < y) |> List.sort comp in
  let () = coords |> List.take n |> List.iter (fun ((i, p), (i', p')) -> update uf uf.(i') uf.(i); if full_stop then stop uf p p') in
  uf |> counter |> IntMap.bindings |> List.sort comp2 |> List.take topn |> List.map snd |> List.fold_left ( * ) 1;;


let solve_first (file_name: string) : int =
  solve 1000 3 (read_lines file_name) false;;

let solve_second (file_name: string) : int =
  solve 1000000 3 (read_lines file_name) true;;