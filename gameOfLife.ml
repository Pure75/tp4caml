#use "list_tools.ml" ;;

(*Rules 0*)

let new_cell = 1 ;;
let empty = 0 ;;
let is_alive cell = cell <> empty ;;

let rules0 cell near =
if cell = 1 then if near = 2 || near = 3 then 1 else 0
else
if near = 3 then 1 else 0 ;;

(*count_neighbours*)

let count_neighbours (x,y) board size =
  let aux (z,t) =
if z < 0 || t < 0 || z >= size || t >= size then 0
else get_cell (z,t) board
in
  ((aux (x,y+1))
  +(aux (x,y-1))
  +(aux (x-1,y))
  +(aux (x-1,y-1))
  +(aux (x-1,y+1))
  +(aux (x+1,y))
  +(aux (x+1,y-1))
  +(aux (x+1,y+1))) ;;

(*count_neighbours (0,0) board 10 ;;*)

(*La vie*)

open Random ;;

(*seed_life*)

let rec seed_life board size nb_cell =
if nb_cell = 0 then board
else
  let a = Random.int size and b = Random.int size in
if get_cell (a,b) board = 1 then seed_life board size nb_cell
else
  let c = put_cell 1 (a,b) board in  seed_life c size (nb_cell - 1) ;;

(*seed_life board 10 5 ;;*)


(*New Board*)

let new_board size nb =
  let m = init_board (size,size) 0
  in seed_life m size nb ;;

(*Next Generation*)

let next_generation board size =
  let rec aux (x,y) board2 =
    match (x,y) with
      | (x,y) when x = size && y = size -> board2
      | (x,y)  when y = size -> aux (x+1,0) board2
      | (_,_) -> aux (x,y+1) (put_cell (rules0 (get_cell (x,y) board) (count_neighbours (x,y) board size)) (x,y) board2)
  in aux (0,0) board ;;

(*next_generation board 10 ;;*)

(*game*)

let rec game board size n =
 match n with
  | 0 -> ()
  | n -> draw_board board size ;
    game (next_generation board size) size (n - 1) ;;

(*game board 10 3 ;;*)

(*new_game*)

let new_game size nb n =
  open_window size ;
game (new_board size nb) size n ;;

(*new_game 10 10 2 ;;*)

