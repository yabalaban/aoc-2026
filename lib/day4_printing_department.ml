(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines;;

let indices arr = List.init (Array.length arr) (fun x -> x);;

type grid_t = { h: int; w: int; cells: char array };;

let parse (line : string): char list =
  line |> String.to_seq |> List.of_seq;;

let solve_grid (f: grid_t -> int -> int) (grid: grid_t): int list = 
  grid.cells |> indices |> List.map (f grid) |> List.filter (fun x -> x >= 0);;

let solve_grid_rec (f: grid_t -> int -> int) (grid: grid_t) : int list = 
  let update idc grid = 
    let () = List.iter (fun p -> grid.cells.(p) <- '.') idc in
    grid in
  let rec loop grid: int list = 
    let idc = solve_grid f grid in 
    match List.length idc with 
    | 0 -> [];
    | _ -> List.append idc (loop (update idc grid)); in 
  loop grid;;

let solve (f: grid_t -> int list) (file: string) : int =
  let lines = file |> read_lines |> List.map parse in 
  let grid = { h = lines |> List.length; w = lines |> List.hd |> List.length; cells = lines |> List.flatten |> Array.of_list } in
  f grid |> List.length;;

let lf_conv (grid: grid_t) (pos: int): int = 
  let deltas = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in 
  let resolve (x, y): int = if pos mod grid.w == 0 && x == -1 then -1 
    else if pos mod grid.w == grid.w - 1 && x == 1 then -1 
    else pos + x + y * grid.w in
  let valid x = 0 <= x && x < grid.w * grid.h in
  let pred pos = if grid.cells.(pos) == '@' then 1 else 0 in 
  let indices = deltas |> List.map resolve |> List.filter valid in 
  let count = indices |> List.map pred |> List.fold_left (+) 0 in 
  if count < 4 && grid.cells.(pos) == '@' then pos else -1

let solve_first (file: string) : int =
  solve (solve_grid lf_conv) file;;

let solve_second (file: string) : int =
  solve (solve_grid_rec lf_conv) file;;
  
