(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines;;

let op (s: string) : (int -> int -> int) = match s with
  | "+" -> (+) 
  | "*" -> ( * ) 
  | _ -> raise Exit;;

let pos_ops (line: string) : (int list) * ((int -> int -> int) list) = 
  line |> String.to_seqi |> Seq.map (fun (i, c) -> (i, String.make 1 c)) 
  |> Seq.filter (fun (_, s) -> s <> " ") |> Seq.map (fun (i, s) -> (i, op s)) 
  |> List.of_seq |> List.split;;

let problems (pos: int list) (line: string) : string list =
  let l = String.length line in
  let rec loop s p =
    match p with
    | [] -> [String.sub line s (l - s)]
    | x :: xs -> String.sub line s (x - s) :: loop x xs
  in
  loop (List.hd pos) (List.tl pos);;

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows);;

let solve f (file_name: string) = 
  let input = file_name |> read_lines |> List.rev in 
  let (pos, ops) = input |> List.hd |> pos_ops in
  let problems = input |> List.tl |> List.map (problems pos) |> transpose |> f |> List.map (List.map String.trim) in 
  let [@warning "-8"] eval (op, lst) = lst |> List.map String.trim |> List.filter ((<>) "") |> List.map int_of_string |> (fun (x :: xs) -> List.fold_left op x xs) in 
  List.combine ops problems |> List.map eval |> List.fold_left (+) 0;; 

let solve_first (file_name: string): int = 
  solve (fun x -> x) file_name;;

let map (problems: string list list): string list list =
  let break s = s |> String.to_seq |> List.of_seq |> List.map (String.make 1) in 
  let fold acc l = List.map2 (fun a b -> String.concat "" [b; a]) acc l in
  let [@warning "-8"] lfold (lst: string list list): string list = match lst with (x:: xs) -> List.fold_left fold x xs in
  problems |> List.map (List.map break) |> List.map lfold;; 

let solve_second (file_name: string): int = 
  solve map file_name;;