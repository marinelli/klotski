(*
 * Author      : Giorgio Marinelli
 * Year        : 2016
 *
 * Description : A Klotski game solver
 *
 *)




(*
 * The prelude
 *)


exception NotFound


type 'e rel = 'e -> 'e list ;;

type 'e prop = 'e -> bool ;;

type ('a, 'set) set_operations =
  { empty : 'set                 (* The empty set. *)
  ; mem   : 'a -> 'set -> bool   (* [mem x s = true] iff [x] is in [s]. *)
  ; add   : 'a -> 'set -> 'set   (* [add s x] is the set [s] union {x}. *)
  }
;;

type ('configuration, 'move) puzzle =
  { move           : 'configuration -> 'move -> 'configuration
  ; possible_moves : 'configuration -> 'move list
  ; final          : 'configuration -> bool
  }
;;

type piece_kind = S | H | V | C | X ;;

type piece = piece_kind * int ;;

let string_of_piece : piece -> string =
  fun (k, _) ->
    match k with
    | S -> "S"
    | H -> "H"
    | V -> "V"
    | C -> "C"
    | X -> "X"
;;
        
let x = (X, 0) and s = (S, 0) and h = (H, 0) ;;
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3)) ;;
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3)) ;;
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ] ;;

type board = piece array array ;;

let initial_board =
  [| [| v0 ; s  ; s  ; v1 |];
     [| v0 ; s  ; s  ; v1 |];
     [| v2 ; h  ; h  ; v3 |];
     [| v2 ; c0 ; c1 ; v3 |];
     [| c2 ; x  ; x  ; c3 |] |]
;;

let initial_board_simpler =
  [| [| c2 ; s  ; s  ; c1 |] ;
     [| c0 ; s  ; s  ; c3 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| x  ; x  ; x  ; x  |] |]
;;

let initial_board_trivial =
  [| [| x  ; s  ; s  ; x  |] ;
     [| x  ; s  ; s  ; x  |] ;
     [| x  ; x  ; x  ; x  |] ;
     [| x  ; x  ; x  ; x  |] ;
     [| x  ; x  ; x  ; x  |] |]
;;

type direction = { dcol : int ; drow : int } ;;
type move = Move of piece * direction * board ;;
let move _ (Move (_, _, b)) = b ;;


let print_board : board -> unit =
  fun cur_board ->
    Array.iter
      ( fun l ->
          let s = Array.fold_right
                    ( fun x y -> (string_of_piece x) ^ " " ^ y )
                    l
                    ""
          in
          print_string s ;
          print_newline ()
      )
      cur_board
;;




(*
 * Extra functions on lists, arrays, and matrices
 *)


let rec list_uniq : 'a list -> 'a list =
  fun l ->
    match l with
    | []      -> []
    | x :: xs -> x :: list_uniq (List.filter (fun y -> x <> y) xs)
;;


let list_product2 : 'a list -> 'b list -> ('a * 'b) list =
  fun l1 l2 ->
    List.concat (List.map (fun x -> List.map (fun y -> (x, y)) l2) l1)
;;


let array_iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit =
  fun f a b ->
    if Array.length a <> Array.length b then
      invalid_arg "array_iter2: arrays must have the same length"
    else
      for i = 0 to Array.length a - 1 do
        f (Array.unsafe_get a i) (Array.unsafe_get b i)
      done
;;


let array_fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a =
  fun f x a ->
    let r = ref x
    in
    for i = 0 to Array.length a - 1 do
      r := f !r (Array.unsafe_get a i)
    done ; !r
;;


let array_fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a =
  fun f a x ->
    let r = ref x
    in
    for i = Array.length a - 1 downto 0 do
      r := f (Array.unsafe_get a i) !r
    done ; !r
;;


let matrix_width : 'a array array -> int =
  fun m ->
    if Array.length m = 0 then 0 else Array.length m.(0)
;;


let matrix_height : 'a array array -> int =
  fun m ->
    Array.length m
;;


let matrix_iter : ('a -> unit) -> 'a array array -> unit =
  fun f m ->
    Array.iter (fun v -> Array.iter (fun x -> f x) v) m
;;


let matrix_iteri : ((int * int) -> 'a -> unit) -> 'a array array -> unit =
  fun f m ->
    Array.iteri (fun j v -> Array.iteri (fun i x -> f (i, j) x) v) m
;;


let matrix_iter2 :  ('a -> 'b -> unit) -> 'a array array -> 'b array array -> unit =
  fun f m1 m2 ->
    array_iter2 (fun x y -> array_iter2 f x y) m1 m2
;;


let matrix_map : ('a -> 'b) -> 'a array array -> 'b array array =
  fun f m ->
    Array.map (fun v -> Array.map (fun x -> f x) v) m
;;


let matrix_mapi : ((int * int) -> 'a -> 'b) -> 'a array array -> 'b array array =
  fun f m ->
    Array.mapi (fun j v -> Array.mapi (fun i x -> f (i, j) x) v) m
;;


let matrix_fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array array -> 'a =
  fun f x m ->
    array_fold_left (fun x' y' -> (array_fold_left f x' y')) x m
;;


let matrix_fold_right : ('b -> 'a -> 'a) -> 'b array array -> 'a -> 'a =
  fun f m x ->
    array_fold_right (fun x' y' -> (array_fold_right f x' y')) m x
;;




(*
 * Some extra functions
 *)


let rec loop : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a =
  fun p f x ->
    match p x with
    | true -> x
    | _    -> loop p f (f x)
;;


let rec exists : ('a -> bool) -> 'a list -> bool =
  fun p l ->
    match l with
    | []      -> false
    | x :: xs -> if p x then true else exists p xs
;;


let rec find : ('a -> bool) -> 'a list -> 'a =
  fun p l ->
    match l with
    | []      -> raise NotFound
    | x :: xs -> if p x then x else find p xs
;;




(*
 * The problem solver
 *)


let near : int rel =
  fun x -> [x - 2 ; x - 1 ; x ; x + 1 ; x + 2]
;;


let flat_map : 'e rel -> ('e list -> 'e list) =
  fun r -> fun l -> List.fold_right (fun x y -> (r x) @ y) l []
;;


let rec iter_rel : 'e rel -> int -> 'e rel =
  fun rel n ->
    if n < 1 then
      fun x -> [x]
    else
      fun x -> list_uniq (flat_map rel ((iter_rel rel (n - 1)) x))
;;


let solve : 'a rel -> 'a prop -> 'a -> 'a =
  let rec solve' =
    fun r p l ->
      try
        find p l
      with
      | NotFound -> solve' r p (flat_map r l)
  in
  fun r p x ->
    solve' r p [x]
;;


let solve_path : 'a rel -> 'a prop -> 'a -> 'a list =
  let path_rel =
    fun r ->
    fun l ->
      match l with
      | []     -> []
      | x :: _ -> List.map (fun y -> y :: l) (r x)
  in
  let path_prop =
    fun p ->
    fun l ->
      match l with
      | []     -> false
      | x :: _ -> p x
  in
  fun r p x ->
    List.rev (solve (path_rel r) (path_prop p) [x])
;;


let archive_map : ('a, 'set) set_operations -> 'a rel -> ('set * 'a list) -> ('set * 'a list) =
  fun opset r (s, l) ->
    List.fold_left
      (
        fun (s', l') y ->
          if opset.mem y s' then
            (s', l')
          else
            (opset.add y s', y :: l')
      )
      (s, [])
      (flat_map r l)
;;


let solve' : ('a, 'set) set_operations -> 'a rel -> 'a prop -> 'a -> 'a =
  let rec solve'' =
    fun opset r p (s, l) ->
      try
        find p l
      with
      | NotFound -> solve'' opset r p (archive_map opset r (s, l))
  in
  fun opset r p x ->
    solve'' opset r p (opset.empty, [x])
;;


let solve_path' : ('a list, 'set) set_operations -> 'a rel -> 'a prop -> 'a -> 'a list =
  let path_rel =
    fun r ->
    fun l ->
      match l with
      | []     -> []
      | x :: _ -> List.map (fun y -> y :: l) (r x)
  in
  let path_prop =
    fun p ->
    fun l ->
      match l with
      | []     -> false
      | x :: _ -> p x
  in
  fun opset r p x ->
    List.rev (solve' opset (path_rel r) (path_prop p) [x])
;;


let solve_puzzle : ('c, 'm) puzzle -> ('c list, 's) set_operations -> 'c -> 'c list =
  fun puzzle_game opset conf ->
    let r =
      fun c ->
        List.map (puzzle_game.move c) (puzzle_game.possible_moves c)
    and p =
      puzzle_game.final
    in
    solve_path' opset r p conf
;;




(*
 * A solver for Klotski
 *)


type coordinates = int * int
;;


let board_width : int = matrix_width initial_board
and board_height : int = matrix_height initial_board
;;


let all_directions : direction list =
  [
    { dcol = 0  ; drow = 1  } ;
    { dcol = 0  ; drow = -1 } ;
    { dcol = 1  ; drow = 0  } ;
    { dcol = -1 ; drow = 0  }
  ]
;;


let final : board -> bool =
  fun board ->
    let line4 = board.(3)
    and line5 = board.(4)
    in
    line4.(1) = (S, 0) && line4.(2) = (S, 0) &&
    line5.(1) = (S, 0) && line5.(2) = (S, 0)
;;


let find_piece_coordinates : board -> piece -> coordinates list =
  fun b p ->
    let result = ref []
    in
    matrix_iteri
      (fun (x, y) p' -> if p' = p then result := (x, y) :: !result else ()) b ;
    !result
;;


let new_coordinates : coordinates list -> direction -> coordinates list =
  fun l { drow ; dcol } ->
    List.map (fun (x, y) -> (x + dcol, y + drow)) l
;;


let valid_coordinates : (int * int) list -> board -> piece -> bool =
  fun l b p ->
    let prop (x, y) b =
      if
        (x >= 0) && (x < board_width) &&
        (y >= 0) && (y < board_height)
      then
        b.(y).(x) = p || b.(y).(x) = (X, 0)
      else
        false
    in
    List.fold_right (fun x y -> prop x b && y) l true
;;


let move_piece : board -> piece -> direction -> board option =
  fun b p d ->
    let old_pos = find_piece_coordinates b p
    in
    let new_pos = new_coordinates old_pos d
    in
    if not (valid_coordinates new_pos b p) then
      None
    else
      let b' =
        matrix_mapi
          (
            fun (i, j) x ->
              let is_old = List.mem (i, j) old_pos
              and is_new = List.mem (i, j) new_pos
              in
              match (is_new, is_old) with
              | (true, _) -> p
              | (_, true) -> (X, 0)
              | _         -> x
          ) b
      in
      Some b'
;;


let pieces_on_board : board -> piece list =
  fun b ->
    list_uniq (matrix_fold_right (fun x y -> if x <> (X, 0) then x :: y else y) b [])
;;


let possible_moves : board -> move list =
  fun b ->
    let all_pieces_on_board = pieces_on_board b
    in
    let all_moves = list_product2 all_pieces_on_board all_directions
    in
    List.fold_right
      (
        fun (xp, xd) y ->
          match move_piece b xp xd with
          | None    -> y
          | Some b' -> (Move (xp, xd, b')) :: y
      )
      all_moves
      []
;;


module Boards =
struct
  type t = board ;;

  let compare_pieces : piece -> piece -> int =
    let compare_kind : piece_kind -> piece_kind -> int =
      fun pk1 pk2 ->
        match (pk1, pk2) with
        | (_, _) when pk1 = pk2                        ->  0
        | (S, _)                                       ->  1
        | (H, _) when pk2 <> S                         ->  1
        | (C, _) when pk2 <> S && pk2 <> H             ->  1
        | (V, _) when pk2 <> S && pk2 <> H && pk2 <> C ->  1
        | (_, _)                                       -> -1
    in
    fun (pk1, pn1) (pk2, pn2) ->
      match ((pk1, pn1), (pk2, pn2)) with
      | _ when (compare_kind pk1 pk2) < 0 -> -1
      | _ when (compare_kind pk1 pk2) > 0 ->  1
      | _ when pn1 < pn2                  -> -1
      | _ when pn1 > pn2                  ->  1
      | _                                 ->  0
  ;;

  exception Equal ;;
  exception Lower_than ;;
  exception Greater_than ;;

  let compare : board -> board -> int =
    fun b1 b2 ->
      try
        matrix_iter2
          (
            fun p1 p2 ->
              let result = compare_pieces p1 p2
              in
              match result with
              | _ when result < 0 -> raise Lower_than
              | _ when result > 0 -> raise Greater_than
              | _                 -> ()
          ) b1 b2 ;
        raise Equal
      with
      | Equal        ->  0
      | Lower_than   -> -1
      | Greater_than ->  1
  ;;

end
;;

module BoardSet = Set.Make (Boards)
;;


let solve_klotski : board -> board list =
  fun b ->
    let klotski_puzzle : (board, move) puzzle =
      {
        move ;
        possible_moves ;
        final
      }
    and klotski_opset : (board list, 's) set_operations =
      {
        empty = BoardSet.empty ;
        mem = (fun xs -> BoardSet.mem (List.hd xs)) ;
        add = (fun xs -> BoardSet.add (List.hd xs))
      }
    in
    solve_puzzle klotski_puzzle klotski_opset b
;;


let () =
  let cur_board = initial_board_simpler
  in
  let solution  = solve_klotski cur_board
  in
  ( print_newline ()

  ; print_string "The initial board is:\n\n"

  ; print_board cur_board

  ; print_newline ()

  ; print_string "This is a possible solution:\n\n"
  
  ; List.iter
      ( fun m -> print_board m ; print_newline () )
      solution
  )
;;

