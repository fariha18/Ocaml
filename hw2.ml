(*FARIHA RAHMAN*)
(*I PLEDGE MY HONOR THAT I HAVE ABIDED BY THE STEVENS HONOR SYSTEM*)

(**Given*)
type 'a gt = Node of 'a *('a gt) list

let t : int gt =
Node (33,
    [Node (12,[]) ;
    Node (77,
        [Node (37,
            [Node (14, [])]) ;
    Node (48, []) ;
    Node (103, [])])
    ])

(**sample function that given n builds a tree*)
(**let mk_leaf (n:’a) : ’a gt =
    Node(n,[])*)

(*1.Implement #height*)
let rec height t = 
    match t with 
  | Node(_ ,[]) -> 1
  | Node(_, ct) -> 
  1 + (List.fold_right (fun x y -> max x y) 
      (List.map height ct) 0)

(*2.Implement #size*)
let rec size t =
    match t with
  | Node(_ ,[]) -> 1
  | Node(_, mt) -> 
  1 + (List.fold_right (fun x y -> x + y) 
      (List.map size mt) 0)

(*3.Implement #paths_to_leaves*)
let rec paths_to_leaves: 'a gt -> int list list =
    fun t ->
    match t with
    | Node(_, []) -> [[]]
    | Node(x, r) -> List.flatten (List.mapi (fun i cd -> (List.map (fun l -> i :: l) (paths_to_leaves cd))) r) 

(*4.Implement #is_leaf_perfect*)
(**helper function for is_leaf_perfect*)
let rec leafPerfect: 'a list list -> bool =
    fun t ->
    match t with
    | [] -> true
    | [x] -> true
    | x::y::t -> if List.length x != List.length y then false
                else leafPerfect (y::t)

let is_leaf_perfect: 'a gt -> bool =
    fun t -> 
    leafPerfect(paths_to_leaves t)

(*5.Implement #preorder*)
let rec preorder: 'a gt -> 'a list =
    fun t ->
    match t with 
    | Node(x,[]) -> [x]
    | Node(x, r) -> List.fold_left (fun h t -> h @ t) [x] (List.map preorder r)

(*6.Implement #mirror*)
let rec mirror: 'a gt -> 'a gt =
    fun t ->
    match t with 
    | Node(x, []) -> Node(x, [])
    | Node(x, r) -> Node(x, List.map mirror (List.rev r))

(*7.Implement #mapt*)
let rec mapt: ('a -> 'b) -> 'a gt -> 'b gt = 
    fun f (Node(x, r)) -> 
    match r with 
    | [] -> Node(f x, [])
    | l -> Node(f x, List.map (fun i -> mapt f i) l)  
    
(*8.Implement #foldt*)
let rec foldt: ('a -> 'b list -> 'b) -> 'a gt -> 'b =
    fun t (Node(x, r)) ->
    t x @@ (List.map (foldt t) r)

let sumt t =
    foldt (fun i rt -> i + List.fold_left (fun i j -> i+j) 0 rt) t

let memt t x = 
    foldt (fun i rt -> i=x || List.exists (fun i -> i) rt) t

(*9.Implement #mirror'*)
let rec mirror' t = 
 foldt (fun i rt -> Node(i, List.rev rt)) t