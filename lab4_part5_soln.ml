# 1 "lab4_part5.mlpp"
(*

                  CS51 Lab 4
              Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab4_part1.ml -- Part 1: Implementing Modules
   lab4_part2.ml -- Part 2: Files as modules
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
-> lab4_part5.ml -- Part 5: Functors (this file)

 *)

(*====================================================================
Part 5: Functors

Although we made the Queue module's type abstract, the Stack module
is not (yet) quite as abstract. In this part, we'll re-write the Stack
module so that its type is also abstract, but at the same time we'll
add some functionality.

Occasionally, it's useful to be able to take a data structure and
"serialize" it; transform its representation in memory into a
format, like a string, that can be saved or transmitted. It can later
be deserialized to recreate the structure in memory. You may notice
that this provides an opportunity to break an abstraction barrier:
serializing an object, manipulating its representation, and then
deserializing back to an object could allow for some invariant to be
violated. Even so, serialization has its uses when the circumstances
are warranted. As a result, we'll add extremely basic support
for serialization.

In order to do this with an abstract type for an element in a stack,
we also need to have a function that can accept an element and
return a string.

We can use a functor to generate a stack that bundles everything
together.

In order to do this, we'll first define a module interface, called
SERIALIZE, that requires that a module expose both a data type and
a function that converts that type into a string representation of
its value.
......................................................................*)

module type SERIALIZE =
  sig
    type t
    val serialize : t -> string
  end ;;

(*......................................................................
Now we'll define a STACK interface. Notice that unlike the INT_STACK,
we'll add additional functions for serialization, a couple of higher-
order functions, and an abstract data type.
......................................................................*)

module type STACK =
  sig
    exception Empty
    type element
    type stack
    val empty : unit -> stack
    val push : element -> stack -> stack
    val top : stack -> element
    val pop : stack -> stack
    val serialize : stack -> string
    val map : (element -> element) -> stack -> stack
    val filter : (element -> bool) -> stack -> stack
    val fold_left : ('a -> element -> 'a) -> 'a -> stack -> 'a
  end ;;

(*......................................................................
Exercise 5A: Now, re-write your stack implementation to use abstract
data types, and also complete the other functions that the signature
requires. Don't forget that you can implement other functions that are
not specified by the signature if you would like.

The serialize function should construct a string in the following
form:

    "N0:N1:N2:...:N"

where N0 was the *first* element pushed onto the stack, N1 was the
*second*, and so on, with N being the *most recent* element pushed to
the stack. (This would make deserialization easier, since elements
could be pushed as soon as they were read.)
......................................................................*)

module MakeStack (Element: SERIALIZE) : (STACK with type element = Element.t) =
  struct
    exception Empty

    type element = Element.t

    type stack = element list

    let empty () : stack = []

    
# 112
    let push (el: element) (s: stack) : stack = el :: s

    
# 115
    let pop_helper (s: stack) : (element * stack) =
      
# 119
      match s with
      | [] -> raise Empty
      | h :: t -> (h, t)

    
# 128
    let top (s: stack) : element = fst (pop_helper s)

    
# 135
    let pop (s: stack) : stack = snd (pop_helper s)

    
# 142
    let map (f: element -> element) (s: stack) : stack = List.map f s

    
# 149
    let filter (f: element -> bool) (s: stack) : stack = List.filter f s

    
# 152
    let fold_left (f: 'a -> element -> 'a) (init: 'a) (s: stack) : 'a =
      
# 156
      List.fold_left f init s

    
# 159
    let serialize (s: stack) : string =
      
# 163
      let f x y = Element.serialize y ^ (if x <> "" then ":" ^ x else "") in
      fold_left f "" s
  
# 166
  end ;;

(*......................................................................
Exercise 5B: Now, make a module "IntStack" by applying the functor
that you just defined to an appropriate module for serializing ints.
......................................................................*)

# 179
(* SMS 160630: For reasons I don't understand, the next line can't specify
the signature for IntSerialize explicitly, e.g., 

    module IntSerialize : SERIALIZE = ...

because that causes a problem when applying MakeStack. Same with
IntStringSerialize in the next exercise *)
module IntSerialize =
  struct
    type t = int
    let serialize n = string_of_int n
  end ;;

module IntStack = MakeStack(IntSerialize) ;;

# 195
(*......................................................................
Exercise 5C: Make a module "IntStringStack" that creates a stack with
an (int, string) tuple type. Its serialize function should output
values in the form:

    "(N,'S')"

Where N is the int, and S is the string.

For this oversimplified serialization function, you may assume that
the string will be made up of alphanumeric characters only.
......................................................................*)

# 214
module IntStringSerialize =
  struct
    type t = (int * string)
    let serialize x = "(" ^ string_of_int (fst x) ^ ",'" ^ (snd x) ^ "')"
  end ;;

module IntStringStack = MakeStack(IntStringSerialize) ;;
