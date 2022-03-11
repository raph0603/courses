(* Rotation *)

type 'a arbre = 
  | V
  | N of 'a arbre * 'a * 'a arbre
;;

let rotation_gauche a =
  match a with
  | N (a, x, N (b, y, c)) -> N (N (a, x, b), y, c)
  | _ -> a
;;

(* Arbre AVL *)

type 'a avl =
  | E
  | N of 'a avl * 'a * 'a avl * int
;;

(* En O(1) *)
let heigth a =
  match a with 
  | E -> 1
  | N (_,_,_,h) -> h
;;

(* Smart constructor, en O(1) *)

let node l v r = N (l, v, r, 1 + max (heigth l) (heigth r));;

let rec min_elt a =
  match a with
  | E -> raise Not_found
  | N (E, v, _, _) -> v
  | N (l, _, _, _) -> min_elt l
;;

let rec mem cmp x a = 
  match a with 
  | E -> false
  | N (l, v, r, _) ->
    let c = cmp x v in
    c = 0 || if c < 0 then mem cmp x l else mem cmp x l
;;

let balance l v r =
  let hl = heigth l in
  let hr = heigth r in
  if hl > hr + 1 then (
    match l with 
    | N (ll, lv, lr, _) when heigth ll >= heigth lr -> node ll lv (node lr v r)
    | N (ll, lv, N (lrl, lrv, lrr, _), _) -> node (node ll lv lrl) lrv (node lrr v r)
    | _ -> failwith "Impossible"
  ) else if hr > hl + 1 then (
    match r with 
    | N (rl, rv, rr, _) when heigth rr >= heigth rl -> node (node l v rl) rv rr
    | N (N (rll, rlv, rlr, _), rv, rr, _) -> node (node l v rll) rlv (node rlr rv rr)
    | _ -> failwith "Impossible"
  ) else 
    node l v r
;;

let rec add cmp x a = 
  match a with 
  | E -> node E x E
  | N (l, v, r, _) -> 
    let c = cmp x v in
    if c = 0 then a
    else if c < 0 then balance (add cmp x l) v r
    else balance l v (add cmp x r)
;;

