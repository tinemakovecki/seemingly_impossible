(* type definitions *)

type nat = int
exception DontKnow of nat
type tree =
  | Answer of bool
  | Question of nat * (bool -> tree)

(* WORK IN PROGRESS *)

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


(* TEST CHAMBER *)

let g a' =
  match (a' 7) with
    | true -> true
    | false -> not (a' 12)
  
let t = to_tree g

let g' = from_tree t

let a1 n = true

let a2 n =
  match n with
    | 7 -> false
	| 12 -> false
	| _ -> true
	
let a3 n =
  match n with
    | 7 -> false
	| 12 -> true
	| _ -> true

	
(* FOR THE FUTURE *)

(* Pričakujemo, da velja: za vse

    φ : (nat -> bool) -> bool

   in

    f : nat -> bool

   velja

     from_tree (to_tree φ) f = φ f
*)

(* val epsilon_tree : tree -> (nat -> bool) *)
let epsilon_tree t = failwith "not implemented"

(* val epsilon : ((nat -> bool) -> bool) -> (nat -> bool) *)
let epsilon p = epsilon_tree (to_tree p)

let exists p = p (epsilon p)
