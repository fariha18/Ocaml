(*FARIHA RAHMAN*)
(*I PLEDGE MY HONOR THAT I HAVE ABIDED BY THE STEVENS HONOR SYSTEM*)
type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(*2.2-1 implement mirror_image function*)
let mirror_image0 : int -> int = 
fun i -> 
match i  with 
| 0 -> 0
| 1 -> 1
| 2 -> 4
| 3 -> 5
| 4 -> 2
| 5 -> 3
| _ -> failwith "error-invalid"

let mirror_image: int list -> int list = 
    fun j ->
    List.map (mirror_image0) j

(*2.2-2 implement rotate_90_letter function*)
let rotate_90_letter0 : int -> int =
fun i ->
match i with
| 0 -> 0
| 1 -> 1
| 2 -> 3
| 3 -> 4
| 4 -> 5
| 5 -> 2
| _ -> failwith "error-invalid"

let rotate_90_letter : int list  -> int list =
fun j ->
List.map (rotate_90_letter0) j

(*2.2-3 implement rotate_90_word function*)
let rec rotate_90_word0 : int list -> int list =
    fun i ->
    match i with
    | [] -> []
    | h::t -> if h = 0 then 0 :: rotate_90_word0 t
                else if h = 1 then 1 :: rotate_90_word0 t
                else if h = 2 then 3 :: rotate_90_word0 t
                else if h = 3 then 4 :: rotate_90_word0 t
                else if h = 4 then 5 :: rotate_90_word0 t
                else if h = 5 then 2 :: rotate_90_word0 t
                else rotate_90_word0 t

let rotate_90_word : int list list -> int list list =
fun j ->
List.map (rotate_90_word0) j

(*2.2-4 implement repeat function*)
let rec repeat : int -> 'a -> 'a list = 
fun n x ->
match n with
| 0 -> []
| _ -> x :: repeat (n-1) x

(*2.2-5 implement pantograph function*)
let pantoHelper : int -> 'a -> 'a list = 
fun n x ->
if x = 0 || x = 1 then [x]
else repeat n x

let pantograph : int -> int list -> int list = 
fun n p ->
List.flatten (List.map(pantoHelper n) p)

(*2.2-5 implement pantograph function using pantograph_nm*)
let rec pantograph_nm : int -> int list -> int list =
fun n p ->
match p with
| [] -> []
| h :: t -> if (h = 0 || h = 1) then [h] @ pantograph_nm n t
                else if (h > 1 && h < 6) then repeat n h @ pantograph_nm n t
                else failwith "error-invalid"

(*2.2-5 implement pantograph function using pantograph_f*)
let pantograph_f : int -> int list -> int list =
fun n p ->
List.fold_right (fun i t -> (pantoHelper n i) @ t) p []

(*2.2-6 implement coverage function*)
let coverageHelper : int * int -> int -> int * int = 
    fun (x,y) e ->
    match e with
    | 0 -> (x,y)
    | 1 -> (x,y)
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1,y)
    | _ -> failwith "error-invalid"

let rec coverageHelper1 : int*int -> int list -> (int*int) list =
    fun (x,y) f ->
    match f with
    | [] -> []
    | h::t -> coverageHelper (x,y) h  :: coverageHelper1 (coverageHelper (x,y) h) t

let coverage : int*int -> int list -> (int*int) list =
    fun (x,y) l ->
    (x,y) :: coverageHelper1 (x,y) l

(*2.2-7 implement compress function*)
let rec tuple : 'a * int -> 'a list -> 'a * int = 
    fun (x,y) i ->
    match i with 
    | [] -> (x,y)
    | h::t -> if x=h then tuple(x,y+1) t
                else (x,y)

let rec identical : 'a -> 'a list -> 'a list =
    fun j k ->
    match k with
    | [] -> []
    | h::t -> if h=j then identical j t
                else h::t

let rec compress : int list -> (int*int) list =
    fun i ->
    match i with 
    | [] -> []
    | x::y::t when x = y -> tuple (x, 2) t :: compress (identical x t)
    | h::t -> (h, 1) :: compress t

(*2.2-8 implement uncompress function*)
let rec uncompressHelper : 'a * int -> 'a list =
    fun (x,y) ->
    match (x,y) with
    | (x,0) -> []
    | (x,y) -> [x] @ uncompressHelper (x,y-1)

let rec uncompress : (int*int) list -> int list =
    fun l ->
    match l with
    | [] -> []
    | (x,y)::t -> uncompressHelper(x,y) @ uncompress t

let uncompress_m : (int*int) list -> int list =
    fun l ->
    List.flatten (List.map (uncompressHelper) l)

let uncompress_f : (int*int) list -> int list =
    fun l ->
    List.fold_right (fun (x,y) t -> (repeat y x) @ t) l []

(*2.2-9 implement optimize function*) 
let rec eliminator : int list -> int list =
    fun i ->
    match i with
    | [] -> []
    | h::t -> if h = 1 then eliminator t
                else h::t

let rec optimizeHelper : int list -> int list = 
(*helper function to carry current state of pen*)
    fun j ->
    match j with
    | [] -> [] 
    | [x] -> [x]
    | x::y::t when (x = y && (x = 1 || x = 0)) -> optimizeHelper (y::t)
    | x::y::t -> x :: optimizeHelper (y::t) 

let optimize : program -> program = 
    fun k ->
    eliminator (optimizeHelper k)