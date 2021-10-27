(*Length*)

let rec length l = match l with

| [] -> 0

| e::reste -> length reste + 1 ;;

(*length [1;2;3;6;6;0] ;;*)

(*apprend*)

let rec apprend (k,l) = match k with
  | []   -> l
  | e::reste -> e:: apprend (reste,l);;


(*apprend ([1;2;3],[4;5]) ;;*)

(*product*)

let rec product l = match l with
  | [] -> 1
  | e::reste -> e * product reste ;;

(*product [] ;;*)
(*product [1;2;3;4;5] ;;*)

(*nth*)

let nth i l =
let rec aux i l =
match (i,l) with
  |(i,[]) -> 0
  |(1,e::reste) -> e
  |(_,_::reste) -> aux (i-1) reste
in aux (i+1) l ;;

(*nth 4 [1;3;5;6;7] ;;*)

(*search_pos*)

let search_pos x l =
  let rec aux x l y =
    match l with
      | [] -> 0
      | e::reste -> if e = x then y
        else aux x reste y+1
  in aux x l 0 ;;

(*search_pos 0 [1;2;4;0;5] ;;*)

(*assoc*)

let rec assoc x l = if x < 0 then failwith "assoc : negative degree"
  else
  match l with
    | [] -> 0
    | (a,b)::reste -> if b = x then a
      else assoc x reste ;;

(*assoc 3 [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;*)
(*assoc 1 [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;*)
(*assoc (-1) [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;*)

(*1.2*)

(*init_list*)

let init_list n x =
  if n < 0 then invalid_arg "init_list : n must be a natural"
  else
    let rec aux n x l = if n = 0 then l
      else
        aux (n-1) x (x::l)
    in aux n x [] ;;

(*init_list 5 0 ;;*)
(*init_list 0 'a' ;;*)
(*init_list  (-5) 1.5 ;;*)

(*put_list*)

let rec put_list v i list =
match (i,list) with
  | (0,e::l) -> v::l
  | (_,[]) -> list
  | (x,e::l) -> e::put_list v (i-1) l ;;

(*put_list 'x' 3 ['-';'-';'-';'-';'-';'-'] ;;*)


(*init_board*)

let rec init_board (l,c) v =
if l < 0 || c < 0 then failwith "init_board = l,c must be > 0"
 else
  let l2 = init_list c v in
  match (l,c) with
  | (0,c) -> []
  | (l,c) -> l2::init_board (l-1,c) v ;;

(*init_board (5,3) 0 ;;*)


(*get_cell*)


let get_cell (x,y) board =
  let size = length board in if size < 0 then 0
    else
    let rec aux (a,b) board =
     match board with
  |[] -> 1
  |e::board when a = x -> let rec aux2 (a,b) l = match l with
      | [] -> 1
      | d::l when b = y -> d
      | d::l when b <> y -> aux2 (a,b+1) l
                           in aux2 (x,0) e
  |e::board when a <> x -> aux (a+1,b) board
    in aux (0,0) board ;;


(*get_cell (2, 1) [[1; 0; 0]; [0; 0; 0]; [0; 2; 0]; [0; 0; 0]; [0; 0; 0]] ;;*)


(*put_cell*)

let put_cell v (x,y) board =
    let z = let rec aux (a,b) board =
     match board with
  |[] -> []
  |e::board when a = x -> e
  |e::board when a <> x -> aux (a+1,b) board
    in aux (0,0) board
    in let p = put_list v y z
    in put_list p x board ;;

(*put_cell 2 (2, 1) [[1; 0; 0]; [0; 0; 0]; [0; 0; 0]; [0; 0; 0]; [0; 0; 0]] ;;*)


(*#load "graphics.cma" ;;
open  Graphics  ;;
let  open_window  size = open_graph (" " ^ string_of_int  size ^ "x" ^string_of_int (size +20)) ;;*)
(*open_window 800 ;;*)


(*draw_cell*)

let grey = rgb 127 127 127 ;;

let draw_cell (x,y) size color =
   set_color color ; fill_rect (x+1) (y+1) size size ;
set_color grey ; draw_rect (x+1) (y+1) size size ;;

(*draw_cell (100,100) 300 black ;;
clear_graph () ;;*)


(*draw_board*)


let  cell_color = function
| 0 -> white
| _ -> black  ;;

let draw_board board size = clear_graph () ;
  let rec aux2 x y l size = match l with
      |[] -> ()
      |e::l -> draw_cell (x*size,y*size) size (cell_color e) ;
aux2 x (y+1) l size  in
  let rec aux x y board size =
match board with
  | [] -> ()
  | l::reste -> aux2 x y l size ; aux (x+1) y reste size
  in aux 0 0 board size ;;

(*let  board = [[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];[0; 0; 0; 0; 0; 0; 0; 0; 0; 0]] ;;*)

(*draw_board board 50 ;;
clear_graph () ;;*)
