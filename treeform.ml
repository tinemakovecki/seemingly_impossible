(* type definitions *)

type nat = int
exception DontKnow of nat
type tree =
  | Answer of bool
  | Question of nat * (bool -> tree)

(* FUNCTIONS *)

(* val to_tree : ((nat -> bool) -> bool) -> tree *)
let to_tree f =
  (*a' is the starting sequence where the functional has not checked any link,
    we specify more 'known' links as more of the sequence is inspected by f *)
  let rec a' n =  raise (DontKnow n)
  (*"search" follows the decision making process of the functional
    and generates the desired tree along the way. Vertices "Question (n, branch)" 
    are appended when f checks the n-th link of the sequence
    and the branches in a vertex are decided by further actions of f. *)
  and search f b' =
    (* Try to find the result of the functional without checking any unknown links of the sequence b'*)
    try
      Answer (f b')
    with DontKnow n ->
      (*If the functional checks an unknown link an exception is raised
        and another "branch" is added to the decision making tree.
        The branches are based on the n-th link of the parameter sequence b',
        "newb'" is a new sequence where the n-th link is not unknown anymore.
        We continue figuring out what the result of the functional is 
        now that the parameter sequence has one more known link *)
      (
      let branch b =
        let newb' k =
          if k = n then b else (b' k)
        in
        search f newb'
      in
      Question (n, branch)
      )
  in
  (* We start a search with sequence a' which doesn't have any checked links *)
  search f a'

  
(* val from_tree : tree -> ((nat -> bool) -> bool) *)
let rec from_tree t a' =
  match t with
    | Answer b -> b (* If the tree only contains a leaf we have found the result. *)
    | Question (k, branch) ->
      (*Otherwise (a' k) decides which branch of the tree we follow.
        val branch : bool -> tree, therefore (branch (a' k))
        is a tree. We continue inspecting the selected subtree. *)
      (
      let subtree =
        branch (a' k)
      in
      from_tree subtree a'
      )


(* TODO: tidy up *)
(* val epsilon_tree : tree -> (nat -> bool) *)
(* "epsilon_tree" constructs a sequence for which the functional (from_tree t) will
  evaluate true if such a sequence exists. If no such sequence exists the function
  still returns a sequence. *)
let epsilon_tree t = 
  (* We define a recursive function which adjusts a given sequence to find the answer
    and a starting sequence "a'" to pass onto the "construct" function to start with. *)
  let rec a' n = true
  and construct b' t =
    match t with
      (* If the function has reached a leaf we return the sequence as it is since there
        is no more tree to inspect. Otherwise we adjust the sequence based on the current
        root of the tree and continue with the appropriate tree branch. *)
      | (Answer _) -> b'
      | Question (n, branch) ->
        (
        (* We define the corresponding functional of the tree we are inspecting as "f" and
          a 'testing' sequence "checkb'" with the n-th link being true. We are working with
          a boolean tree so this sequence corresponds to the tree's 'true' branch. *)
        let f = from_tree t 
        and checkb' l = (if l = n then true else (b' l))
        in
        (* If there exists a sequence that fits our criteria with the n-th link true, we use
          that, otherwise we set the n-th link of the sequence to false before moving on. *)
        let nextb' k =
          if k = n
          then (if (f (construct checkb' (branch true))) then true else false)
          else (b' n)
        in
        construct nextb' (branch (nextb' n))
        )
  in
  construct a' t
  
(* val epsilon : ((nat -> bool) -> bool) -> (nat -> bool) *)
let rec epsilon p = epsilon_tree (to_tree p)

(* val exists : ((nat -> bool) -> bool) -> bool *)
let exists p = p (epsilon p)

(* TODO: proper queue/heap implementation *)
(* val bfs_proto : (nat -> a) -> tree list -> unit *)
let rec bfs_proto f queue = 
  (* Performs bfs search on a tree queue and applies the
  function f to the nodes of the tree along the way. *)
  match queue with
    | [] -> ()
    | t::ts -> 
      match t with
        | Answer b -> ()
        | Question (n, branch) ->
          (
          f n;
          let fbranch = branch false
          and tbranch = branch true
          in let q = ts @ [tbranch; fbranch]
          in 
          bfs_proto f q
          )

type path = Steps of (nat * bool) list

(* val path_seq : path -> (nat -> bool) *)
let path_seq path = 
  let rec transform p a' =
    match p with
      | Steps [] -> a'
      | Steps ((n, b)::xs) -> 
        (
        let b' k = if k = n then b else (a' k)
        and new_p = Steps xs
        in 
        transform new_p b'
        )
  and c' n = true
  in
  transform path c'
  

(* TODO: testing *)
(* val bfs_path : (tree * path) list -> path *)
let rec bfs_path queue = 
(* Performs a bfs search on a tree and returns a path which is
  equivalent to the sequence an epsilon functional would return. *)
  match queue with (* elements of queue: (tree, Steps w)  *)
    | [] -> Steps []
    | (t, Steps w)::ts -> 
      match t with
        | Answer true -> Steps w
        | Answer false -> if ts = [] then Steps w else bfs_path ts
        | Question (n, branch) ->
          (
          let fbranch = branch false
          and tbranch = branch true
          in 
          let q = ts @ [(tbranch, Steps ((n, true)::w));
                        (fbranch, Steps ((n, false)::w))]
          in 
          bfs_path q
          )

(* val bfs_epsilon_tree : tree -> (nat -> bool) *)
(*  "bfs_epsilon_tree" uses bfs to construct a sequence for which the functional 
  (from_tree t) will evaluate true if such a sequence exists. If no such sequence
  exists the function still returns a sequence. *)
let bfs_epsilon_tree t = 
  let way = bfs_path [(t, Steps [])]
  in
  path_seq way


(* WORK IN PROGRESS *)

type tree_construct =
  | Unfinished
  | Answer_c of bool
  | Question_c of nat * (bool -> tree_construct)

(* val cons_to_tree : tree_construct -> tree *)
let rec cons_to_tree tree_cons =
  (* Transforms a tree_construct into a regular tree. *)
  match tree_cons with
    | Answer_c b -> Answer b
    | Question_c (n, branch) -> Question (n, (fun b -> cons_to_tree (branch b)))
    | Unfinished -> failwith "cannot express unfinished in tree type"

(* val replace : path -> tree_construct -> tree_construct -> tree_construct *)
let rec replace way t subtree = 
  (* Replaces the element of 't' located at 'way' with 'subtree'. *)
  match way with
    | Steps [] -> subtree
    | Steps ((_, x)::xs) -> 
      match t with
        | Question_c (n, branch) -> 
          let new_branch b =
            if b = x then replace (Steps xs) (branch x) subtree
            else branch b
          in
          Question_c (n, new_branch)
		(* might be better to handle Answer_c and Unfinished seperatly, it's not quite the same *)
		| _-> failwith "location is invalid"

(* val find_unfinished : (tree * path) list -> (nat * bool) list *)
let rec find_unfinished q = (* the argument is a stack of trees and paths to them *)
  (* Searches through a tree_construct using dfs and finds parts that are Unfinished. *)
  match q with
    | [] -> []
    | (t', Steps w)::ts -> 
      match t' with
        | Unfinished -> w
        | (Answer_c _) -> find_unfinished ts
        | (Question_c (n, branch)) -> 
          (
          let t1 = (branch true, Steps ((n, true)::w))
          and t2 = (branch false, Steps ((n, false)::w))
          in
          find_unfinished (t1::t2::q)
          )

(* val tree_part : bool -> nat list -> tree_construct *)
let rec tree_part b l =
  (* Constructs a tree part, with Question_c nat values being taken from l and all 'false'
     branches marked as Unfinished. The tree part ends with an Answer_c containing value b. *)
  match l with
    | [] -> Answer_c b
    | (x::xs) -> 
      (
      let branch = function
        | false -> Unfinished
        | true -> tree_part b xs
      in
      Question_c (x, branch)
      )

(* val to_tree_ref : ((nat -> bool) -> bool) -> tree *)
let to_tree_ref f =
  (* Construct a tree from a functional using refrences *)
  (* TODO: replace the mess by making auxilliary functions 
     TODO: TESTING! *)
  let order = ref [];
  in
  let rec a' n =
    order := n :: !order;
    true
  in (* val construct : tree_construct list -> (nat -> bool) -> tree_construct  *)
  let rec construct t g = 
    let way = List.rev (find_unfinished [(t, Steps [])])
    in 
    if way = [] then t
    else 
      (
      let aux_seq n = 
        let rec list_seq l k =
          match l with
            | [] -> a' k
            | (i, b)::l2 -> if i = k then b else list_seq l2 k
        in
        list_seq way n
      in
      let ans = f aux_seq
      and ord = List.rev !order (* which order the functional checked new links in *)
      in
      order := [];
      let t_new = tree_part ans ord
      in
      let t' = replace (Steps way) t t_new
      in
      construct t' g
      )
  in
  let start_t = tree_part (f a') (List.rev !order)
  in
  construct start_t f 


  
  (* TODO: function: tree_construct -> tree, once finished *)
  
(* TEST CHAMBER *)

(* functionals *)

let g a' =
  match (a' 7) with
    | true -> true
    | false -> not (a' 12)

(* Have tried an example with up to 14 branchings, 
  did not notice significant delays in processing. *)
let f a' =
  match (a' 7) with
    | _ -> 
    (match (a' 9) with
      | _ ->
      (match (a' 5) with
        | _ ->
        (match (a' 3) with
          | true -> false
          | false -> true
        )
      )
    )

let t = to_tree f
    
(* sequences *)

let a1 n = true

let a2 n =
  match n with
    | 7 -> false
    | 12 -> false
    | _ -> true
    
let a3 n =
  match (n mod 2) with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "invalid input"

    
(* FOR THE FUTURE *)

(* Pričakujemo, da velja: za vse

    φ : (nat -> bool) -> bool

   in

    f : nat -> bool

   velja

     from_tree (to_tree φ) f = φ f
*)
