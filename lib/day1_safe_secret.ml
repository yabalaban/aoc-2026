(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let parse (line : string) : int -> int = 
  let op = match line.[0] with
  | 'L' -> (-)
  | 'R' -> (+)
  | _ -> raise Exit in

  let len = (String.length line) - 1 in 
  let sub = String.sub line 1 len in 
  let d = int_of_string sub in 
  fun x -> op x d;;

let solve (f: int -> int -> int) (file: string) : int =
  let lines = read_lines file in
  let ops = List.map parse lines in 
  let upd = fun acc op -> 
    let pos = fst acc in 
    let zeros = snd acc in 
    let () = Printf.printf "pos=%d zeros=%d\n%!" pos zeros in
    let npos = op pos in
    let nzeros = zeros + f pos npos in
    let () = Printf.printf "[upd] new acc = (%d, %d)\n%!" npos nzeros in
    (npos mod 100, nzeros); in
  let run = List.fold_left upd (50, 0) ops in 
  snd run;;

let solve_first (file: string) : int = 
  let f = fun (_: int) (npos: int): int -> if npos mod 100 = 0 then 1 else 0; in 
  solve f file;;

let solve_second (file: string) : int = 
  let sign x = compare x 0 in 
  let f = fun (pos: int) (npos: int): int -> 
    let e = if sign(pos) != sign(npos) && pos != 0 then 1 else 0 in 
    e + abs(npos) / 100; in 
  solve f file;;