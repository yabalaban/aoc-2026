(* move to utils/whatever *)
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines;;

let parse (line : string) : int list =
    let ioc c = int_of_char c - int_of_char '0' in
    line |> String.to_seq |> List.of_seq |> List.map ioc;;

let tuple_op f (a: 'a * 'a) : 'a =
    f (fst a) (snd a);;

let headn (n: int) (lst: int list) : int list =
  let rec loop (l: int list) (acc: int list) (n: int) : int list
    = match (l, n) with
    | ([], _) -> acc
    | (x :: _, 1) -> x :: acc
    | (x :: xs, n) -> loop xs (x :: acc) (n - 1)
  in
  loop lst [] n |> List.rev;;

let tailn (n: int) (lst: int list) : int list =
  lst |> List.rev |> headn (List.length lst - n) |> List.rev;;

let ipow (x: int) (n: int) : int =
  let rec loop (x: int) (n: int) (acc: int) : int =
    match n with
    | 0 -> acc
    | n -> loop x (n - 1) (acc * x)
  in
  loop x n 1;;

let pushdown (u: int list) (l: int list) : int list =
    let rec loop (u: int list) (acc: int list) (l: int list) : int list =
      match (u, acc, l) with
      | (_, xs, []) -> List.rev xs
      | ([], xs, ys) -> List.rev xs @ ys
      | (u :: us, xs, y :: ys) -> if u > y
            then loop (y :: us) (u :: xs) ys
        else if u == y
            then loop (u :: us) (y :: xs) ys
        else
            loop us (y :: xs) (ys)
        in
    loop u [] l;;

let solve (f: int list -> int) (file: string) : int =
  let banks = file |> read_lines |> List.map parse in
  List.map f banks |> List.fold_left (+) 0;;

let turn_valves_n (n: int) (lst: int list)  : int =
  let rec loop (l: int list) (s: int list): int list =
    match l with
    | [] -> s
    | x :: xs -> loop xs (pushdown [x] s)
    in

  let rlst = List.rev lst in
  let hd = (headn n rlst) |> List.rev in
  let acc = loop (tailn n rlst) hd in
  let it = List.init (List.length acc) (ipow 10) |> List.rev in
  List.combine it acc |> List.map (tuple_op ( * )) |> List.fold_left (+) 0;;

let solve_first (file: string) : int =
  solve (turn_valves_n 2) file;;

let solve_second (file: string) : int =
  solve (turn_valves_n 12) file;;