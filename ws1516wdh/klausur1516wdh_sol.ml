open Thread
open Event

(* 3. *)

type 'a t = Leaf | Node of 'a * 'a t * 'a t
type 'a c = CLeaf | Uneq of 'a * 'a c * 'a c | Eq of 'a * 'a c

let rec compress t = 
  match t with 
  | Leaf -> CLeaf 
  | Node (v, l, r) -> if l = r then Eq (v, compress l) else Uneq (v, compress l, compress r)

let rec count t = 
  match t with 
  | CLeaf -> 0
  | Eq (v, c) -> 1 + 2*(count c)
  | Uneq (v, l, r) -> 1 + (count l) + (count r)

let rec merge f a b = 
  match a with 
  | CLeaf -> b 
  | Eq (va, ca) -> (match b with 
      | CLeaf -> a
      | Eq (vb, cb) -> Eq (f va vb, merge f ca cb)
      | Uneq (vb, lb, rb) -> let lnew = merge f ca lb in let rnew = merge f ca rb in
        if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew))
  | Uneq (va, la, ra) -> (match b with
      | CLeaf -> a
      | Eq (vb, cb) -> let lnew = merge f la cb in let rnew = merge f ra cb in
        if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew)
      | Uneq (vb, lb, rb) -> let lnew = merge f la lb in let rnew = merge f ra rb in
        if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew)
    )

(* --- Test environment --*)