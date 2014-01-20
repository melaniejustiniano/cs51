(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)

module type DICT = 
sig
  type key   
  type value 
  type dict

  (* An empty dictionary *)
  val empty : dict 
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
  val insert : dict -> key -> value -> dict
  val remove : dict -> key -> dict
  val choose : dict -> (key * value * dict) option
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  val run_tests : unit -> unit
end


(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string
end


(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_key = string_of_int
  let string_of_value v = v

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]

  let num_values = List.length possible_values

  let current_index = ref 0
end



(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
struct
  open Order;;
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  let empty = [] ;;

  let fold f d = List.fold_left (fun a (k,v) -> f k v a) d 

  let rec lookup d k = 
    match d with 
      | [] -> None
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with
          | Eq -> Some v1
          | Greater -> lookup d1 k 
          | _ -> None)

  let member d k = 
    match lookup d k with 
      | None -> false 
      | Some _ -> true

  let rec insert d k v = 
    match d with 
      | [] -> [(k,v)]
      | (k1,v1)::d1 -> 
        (match D.compare k k1 with 
          | Less -> (k,v)::d
          | Eq -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k = 
    match d with 
      | [] -> []
      | (k1,v1)::d1 ->
	(match D.compare k k1 with 
          | Eq -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)
	  
  let choose d = 
    match d with 
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string = 
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^ 
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left f "" d

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  let run_tests () = ()

end    


(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  type key = D.key
  type value = D.value

  type pair = key * value

  type dict = 
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3
        
  let empty : dict = Leaf

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (l, (k, v), r) -> f k v (fold f (fold f u r) l) 
    | Three(l, (k1, v1), m, (k2, v2), r) ->
      f k2 v2 (f k1 v1 (fold f (fold f (fold f u r) m) l))

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let rec string_of_dict (d: dict) : string =
    match d with
    | Leaf -> ""
    | Two (a, (x, y), c) -> string_of_dict a ^ "(" ^ string_of_key x ^ 
      "," ^ string_of_value y ^ ") " ^ string_of_dict c
    | Three (a, (x, y), c, (x1, y1), r) ->
      string_of_dict a ^ "(" ^ string_of_key x ^ 
      "," ^ string_of_value y ^ ") " ^ string_of_dict c ^ "(" ^
	string_of_key x1 ^  "," ^ string_of_value y1 ^ ") " ^
	string_of_dict r
      
  let rec string_of_tree (d: dict) : string = 
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left) 
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) -> 
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ "," 
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict) 
      (x: pair) (x_other: dict) : kicked = 
    let (wk, _), (xk, _) = w, x in
    match D.compare wk xk with
    | Greater -> Done(Three(x_other, x, w_left, w, w_right))
    | Less | Eq -> Done(Three(w_left, w, w_right, x, x_other))    

  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (wk, _), (xk, _), (yk, _) = w, x, y in
    match D.compare wk xk with
    | Less | Eq -> Up(Two(w_left,w,w_right),x,Two(other_left,y,other_right))
    | Greater ->
      match D.compare wk yk with
      | Less | Eq-> Up(Two(other_left,x,w_left),w,Two(w_right,y,other_right))
      | Greater -> Up(Two(other_left,x,other_right),y,Two(w_left,w,w_right))

  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)
      | Two(left, (k2, v2),right) ->
	insert_downward_two (k,v) (k2,v2) left right
      | Three(left,(xk,xv),middle,(yk,yv),right) ->
    	insert_downward_three (k,v) (xk,xv) (yk,yv) left middle right
	  
  and insert_downward_two ((k,v): pair) ((k1,v1): pair) 
      (left: dict) (right: dict) : kicked = 
    match D.compare k k1 with
    | Greater ->
      let dright = insert_downward right k v in
      (match dright with
      | Up (l,w,r)-> insert_upward_two w l r (k1,v1) left  
      | Done d -> Done (Two(left,(k1,v1), d)))
    | Less ->
      let dleft = insert_downward left k v in
      (match dleft with
      | Up (l,w,r)-> insert_upward_two w l r (k1,v1) right  
      | Done d -> Done(Two (d,(k1,v1),right)))
    | Eq ->
      Done (Two(left, (k,v), right))
	
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair) 
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Eq -> Done (Three(left,(k,v),middle,(k2,v2),right))
    | Less ->
      let dleft = insert_downward left k v in
      (match dleft with
      | Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) middle right
      | Done d -> Done (Three(d,(k1,v1),middle,(k2,v2),right)))
    | Greater ->
      match D.compare k k2 with
      | Eq -> Done (Three(left,(k1,v1),middle,(k,v),right))
      | Less ->
	let dmiddle = insert_downward middle k v in
	(match dmiddle with
	| Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) left right
	| Done d -> Done(Three(left,(k1,v1),d,(k2,v2),right)))
      | Greater -> 
	let dright = insert_downward right k v in
	match dright with
	| Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) left middle
	| Done d -> Done (Three(left,(k1,v1),middle,(k2,v2),d))

  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
    | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
    | Done x -> x
      
  let remove_upward_two (n: pair) (rem: pair option) 
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) ->
	Absorbed (rem, Two (Two (a,x,b), y, Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d ->
	Absorbed (rem, Two (Two (a,x,b), y, Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e ->
	Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->	
	Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
	Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
	Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Eq -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Eq, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Eq -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair) 
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) -> 
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less -> 
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Eq, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) -> 
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) -> 
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) -> 
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) -> 
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) -> 
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two(l,(a,b),r) ->
      (match D.compare k a with
      | Eq -> Some b
      | Less -> lookup l k
      | Greater -> lookup r k)
    | Three(l,(a,b),m,(c,d),r) ->
      match D.compare k a with
      | Eq -> Some b
      | Less -> lookup l k
      | Greater ->
	match D.compare k c with
	| Eq -> Some d
	| Less -> lookup m k
	| Greater -> lookup r k
       
  let rec member (d: dict) (k: key) : bool =
    match d with
    | Leaf -> false
    | Two(l,(a,b),r) ->
      (match D.compare k a with
      | Eq -> true 
      | Less -> member l k 
      | Greater -> member r k)
    | Three(l,(a,b),m,(c,d),r) ->
      match D.compare k a with
      | Eq -> true
      | Less -> member l k
      | Greater ->
	match D.compare k c with
	| Eq -> true
	| Less -> member m k
	| Greater -> member r k
        
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two(l, (k,v), r) -> Some (k, v, remove d k)
    | Three(l, (k,v), m, x, r) -> Some (k, v, remove d k)

  let rec balanced (d: dict) : bool =
    let rec helper (d: dict) (h: int) : bool * int =
      match d with
      | Leaf -> (true, h)
      | Two (l,p,r) -> 
	let (lb, lh) = helper l (h + 1) in
	let (rb, rh) = helper r (h + 1) in
	(lh = rh && lb && rb, lh)
      | Three (l,p,m,p2,r) -> 
	let (lb, lh) = helper l (h + 1) in
	let (mb, mh) = helper m (h + 1) in
	let (rb, rh) = helper r (h + 1) in
	(lh = mh && mh = rh && lb && mb && rb, lh)
    in
    let (b, height) = helper d 0 in
    b

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  let run_tests () = ()
end

module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
  (* AssocListDict(D) *)
   BTDict(D) 
