let read_lines file_name = In_channel.with_open_text file_name In_channel.input_lines;;

let [@warning "-8"] parse line = line |> String.split_on_char ',' |> List.map int_of_string |> function | x::y::[] -> (x, y);;

let abs x = if x < 0 then -x else x;;
let area ((x, y), (x', y')) = succ (abs (x - x')) * succ (abs (y - y'));;
let cartesian l l' = List.concat_map (fun x -> List.map (fun y -> (x, y)) l') l;;
let solve f lines = lines |> List.map parse |> (fun l -> cartesian l l |> List.filter (f l)) |> List.map area |> List.fold_left max 0;;
let shift l = List.append (List.tl l) [List.hd l];;

let solve_first (file_name: string) : int = solve (fun _ _ -> true) (read_lines file_name);;

let inside_polygon polygon points =
  let in_orange x (x', x'') = min x' x'' < x && x < max x' x'' in
  let in_range x (x', x'') = min x' x'' <= x && x <= max x' x'' in
  let on_line (x, y) ((x', y'), (x'', y'')) = (y' == y'' && y == y' && in_range x (x', x'')) || (x' == x'' && x == x' && in_range y (y', y'')) in
  let on_edge polygon point = List.exists (on_line point) polygon in
  let inside polygon (x, y) =
    let [@warning "-8"] f (acc, d) edge = match edge with
      | ((x', y'), (_, y'')) when y' != y'' && in_range y (y', y'') ->
        let d' = compare y' y'' in
        if x' < x && d != d' then (acc + 1, d') else (acc, d)
      | _ -> (acc, d) in
    List.fold_left f (0, 0) polygon |> (fun x -> (fst x) mod 2 = 1) in
  let intersect polygon points =
    let rect = List.combine points (shift points) in
    let inter l l' = match (l, l') with
      | (((x, y), (x', y')), ((l, m), (l', _))) when x = x' && l != l' -> in_orange x (l, l') && in_orange m (y, y')
      | (((x, y), (x', y')), ((l, m), (_, m'))) when y = y' && m != m' -> in_orange y (m, m') && in_orange l (x, x')
      | _ -> false in
    let check edge = polygon |> List.exists (fun e -> inter edge e) in
    rect |> List.exists check in
  not (intersect polygon points) && points |> List.for_all (fun p -> (on_edge polygon p) || (inside polygon p));;

let solve_second (file_name: string) : int =
  let f points ((x, y), (x', y')) =
    let polygon = List.combine points (shift points) in
    inside_polygon polygon [(x, y); (x', y); (x', y'); (x, y')] in
  solve f (read_lines file_name);;