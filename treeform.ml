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


(* WORK IN PROGRESS *)

(* val bfs_epsilon_tree : tree -> (nat -> bool) *)
(* "bfs_epsilon_tree" uses bfs to construct a sequence for which the functional 
  (from_tree t) will evaluate true if such a sequence exists. If no such sequence
  exists the function still returns a sequence. *)
(*
let bfs_epsilon_tree t = 
  let rec a' n = true
  and construct b' t n m =
    (* construct will only inspect the tree up to a certain depth, which shall 
	  increase in each recursive function call as needed.
      n - maximum depth
      m - current depth  *)
	(* requires rewrite *)
	match (n-m) with
	  | 0 -> construct b' t (n+1) m
	  | _ ->
	  ( (* requires rewrite *)
      match t with
	    | (Answer _) -> b'
	    | Question (n, branch) ->
	      (
		  let f = from_tree t 
		  and checkb' l = (if l = n then true else (b' l))
		  in
	      let nextb' k =
		    if k = n
		    then (if (f (construct checkb' (branch true))) then true else false)
		    else (b' n)
		  in
		  construct nextb' (branch (nextb' n))
		  )
	  )
  in
  construct a' t
*)

(* TEST CHAMBER *)

(* functionals *)

let g a' =
  match (a' 7) with
    | true -> true
    | false -> not (a' 12)
  
let t = to_tree g

let g' = from_tree t

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

	
(* FOR THE FUTURE *)

(* Pričakujemo, da velja: za vse

    φ : (nat -> bool) -> bool

   in

    f : nat -> bool

   velja

     from_tree (to_tree φ) f = φ f
*)
