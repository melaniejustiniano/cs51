open Order

(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig 

  type node = int

  type graph 

  (* An empty graph. *)    
  val empty : graph

  (* Input: graph. Returns a list of the nodes in the graph. *)
  val nodes : graph -> node list

  (* Input: graph. Returns true if empty. *)
  val is_empty : graph -> bool
    
  (* Inputs: graph and node. Returns graph with node added. *)
  val add_node : graph -> node -> graph

  (* Input: graph, beginning node, ending node, and edge weight *)
  val add_edge : graph -> node -> node -> float -> graph
    
  (* Input: graph and node ID. Returns list of neighbors. *)
  (* Returns 'None' if the node is not in the graph. *)
  val neighbors : graph -> int -> (node * float) list option

  (* Input: graph and node. Checks if the node is in the graph. *)    
  val has_node : graph -> node -> bool

  (* Input: graph. Returns the number of nodes in the graph. *)
  val node_count : graph -> int

  (* Input: graph. Returns a string interpretation of a graph. *)
  val string_of_graph : graph -> string

  (* Input: list of edges. Returns graph. *)
  val from_edges : (node * float * node) list -> graph

end
   

    
(* We'll represent a graph as an edge dictionary:
 * dictionary: node -> neighbor set
 * Every node in the graph must be a key in the dictionary. *)

module Graph : GRAPH =
struct
  open Order
  type node = int
    
  module NeighborSet = Myset.Make
    (
      struct
        type t = node * float
        let compare (n1, w1) (n2, w2) = int_compare n1 n2
        let string_of_t (n, w) = string_of_int n ^ ", " ^ string_of_float w
      end
    )

  module EdgeDict = Dict.Make
  (
    struct
      type key = node
      type value = NeighborSet.set
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value ns = NeighborSet.string_of_set ns
    end
  )
    
  module IntNode = Dict.Make
  (
    struct 
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = string_of_int
    end
  )
    
  type graph = 
    { edges : EdgeDict.dict ;
      node_count : int ;
      index_to_node_map : IntNode.dict }

  let empty : graph = 
    { edges = EdgeDict.empty;
      node_count = 0;
      index_to_node_map = IntNode.empty }

  let add_node g n =
    (* Check if node is already in graph. *)
    if EdgeDict.member g.edges n then g
    else
      { edges = EdgeDict.insert g.edges n (NeighborSet.empty) ;
        node_count = g.node_count + 1 ;
        index_to_node_map = 
          IntNode.insert g.index_to_node_map g.node_count n }

  let nodes g =
    EdgeDict.fold (fun k v r -> k :: r) [] g.edges
      
  let is_empty g = 
    (g.node_count = 0)
          
  (* Adds the nodes if they aren't already present. *)
  let add_edge g src dst wt =
    let new_neighbors = match EdgeDict.lookup g.edges src with
      | None -> NeighborSet.insert (dst, wt) NeighborSet.empty 
      | Some s -> NeighborSet.insert (dst, wt) s
    in
    (* Ensures both src and dst in the graph before adding edge. *)
    let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       node_count = g'.node_count;
       index_to_node_map = g'.index_to_node_map}

  let neighbors g node_id : (node * float) list option = 
    match EdgeDict.lookup g.edges node_id with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun neigh r -> neigh :: r) [] s)
          
  let has_node g n = 
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

  let node_count g =
    g.node_count

  let string_of_graph g = 
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)

  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) ->
      if wt < 0. then failwith "No negative edge weights."
      else add_edge g src dst wt) empty es
end



(* Graph representation as a matrix. *)

module Matrix : GRAPH =
struct
  type node = int
    
  type adj_mat = float array array

  type graph = 
    { mutable node_count : int; 
	    size : int; (* Maximum number of nodes. *)
		  nodes : node array;
		  m : adj_mat } (* Cost between nodes. *)

  let empty =
    {node_count = 0; size = 1; nodes = Array.make 1 0;
     m = Array.create_matrix 1 1 infinity}
      
  let nodes g = 
    let rec arraytolist nodea i currentlist =
      if i = g.node_count then currentlist
      else Array.get nodea i :: (arraytolist nodea (i + 1) currentlist)
    in
    arraytolist g.nodes 0 []
      
  let is_empty g = 
    (g.node_count = 0)
    
  let has_node g n =
    let rec aux i =
      (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
    in aux 0 ;;

  let add_node g n =
    if g.node_count = g.size then failwith "The graph is full."
    else if has_node g n then failwith "The node already exists."
    else (g.nodes.(g.node_count) <- n; g.node_count <- g.node_count + 1); g

 (* The function index returns the index of the node n in the graph g. 
  * If the node does not exist, an exception is thrown. *)
  let index n g = 
    let rec aux i = 
      if i >= g.size then raise (Failure "Not_found")
      else if g.nodes.(i) = n then i 
      else aux (i+1)
    in aux 0 ;;

  (* Adds the nodes if they aren't already present. *)
  let add_edge g e1 e2 c = 
    try
      let x = index e1 g and y = index e2 g in 
      g.m.(x).(y) <- c ; g
    with Not_found -> failwith "Node does not exist.";;
  
  (* Returns None if node isn't in the graph. *)
  let neighbors g n =
    (* Row of matrix containing a nodes's neighbors is g.m.n. *) 
    let rec aux n i neighbor_list = 
      if i = g.size then neighbor_list
     (* Puts all of the neighbors in a list. *) 
     else 
	match g.m.(n).(i) with 
	| infinity -> aux n (i+1) neighbor_list
	| _ -> aux n (i + 1) ((i, g.m.(n).(i))::neighbor_list)
   in
   let list = aux n 0 [] in
   match list with
   | [] -> None
   | _ -> Some list
     
  let node_count g = 
    g.node_count
    
  (* Placeholder. *)
  let string_of_graph g = ""
	
  (* Helper function to count nodes in an edge list. *)
  let sized nfnlist =
    let all = 
      List.fold_left (fun l (src, wt, dst) -> src :: dst :: l) [] nfnlist in
    let rec unique nodes =
      match nodes with
      | [] -> []
      | head :: tail ->
	let newtail = unique tail in
	if List.mem head newtail then newtail else head :: newtail
    in
    List.length (unique all)
            
  let from_edges es =
    let s = sized es in
    let g =
      {node_count = 0; size = s; nodes = Array.make s 0;
       m = Array.create_matrix s s infinity}
    in
    List.fold_left (fun g (src, wt, dst) ->
      if wt < 0. then failwith "No negative edge weights."
      else add_edge g src dst wt) g es
        
end