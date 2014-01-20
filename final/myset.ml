(* Definitions for sets. *)

(* An interface for set modules *)
module type SET = 
sig

  (* Type of elements in the set. *)
  type elt  

  (* Abstract type for the set. *)
  type set  

  (* An empty set. *)
  val empty : set

  (* Input: set. Returns true if empty. *)
  val is_empty : set -> bool

  (* Input: element and set. Returns set with element added. *)
  val insert : elt -> set -> set

  (* Input: element. Creates a set of one element. *)
  val singleton : elt -> set

  (* Input: two sets. Returns a combined set. *)
  val union : set -> set -> set

  (* Input: two sets. Returns a set of elements in both. *)
  val intersect : set -> set -> set

  (* Input: element and set. Returns set without element. *)
  val remove : elt -> set -> set

  (* Input: set and element. Returns true if element is in set. *)
  val member : set -> elt -> bool

  (* Input: set. Returns random element and new set without that element.
   * If the set is empty, return None. *)
  val choose : set -> (elt * set) option

  (* Fold function. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a
  
  (* Input: set. Returns string of the set. *)
  val string_of_set : set -> string
  
  (* Input: element. Returns string of element. *)
  val string_of_elt : elt -> string

  (* For testing. *)
  val run_tests : unit -> unit
end



(* Parameters for sets: 
 * Element type. Comparison function. String adjustment. *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string
end



(* COMPARABLE signature for ints. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = 
    []

  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false

  let singleton x = 
    [x]

  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = 
    List.fold_right insert xs ys

  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = 
    C.string_of_t

  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"

  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let run_tests () = ();

end

(* Make: a functor that creates a SET by calling our ListSet functors. *)                                  *)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  ListSet (C)
