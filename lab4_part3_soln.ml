# 1 "lab4_part3.mlpp"
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
-> lab4_part3.ml -- Part 3: Interfaces as abstraction barriers (this file)
   lab4_part4.ml -- Part 4: Polymorphic abstract types
   lab4_part5.ml -- Part 5: Functors

 *)

(*====================================================================
Part 3: Interfaces as abstraction barriers

It may feel at first that it's redundant to have both signatures and
implementations for modules. However, as we saw at the end of Part 2,
the use of signatures allows for us to define a set of functionality
that might be shared between two related (but still different)
modules.  It might also be used as a way to hide, within a single
module, various helper functions or implementation details that we (as
the developer of a module) would rather not expose. In other words, it
provides a rigid abstraction barrier that makes it harder for a
programmer using your module to break some invariant.

Let's say we want to build a data structure called a stack. This
structure is sometimes referred to as a LIFO (Last-In First-Out)
structure, which means that the last element to be added to it is the
first one to be removed. Much like a stack of plates: you begin with
an empty pile, "push" one or more plates to the top of the stack, and
at any point might "pop" a plate off the top of the pile.

To get started, we can implement this as a list. "Push" and "pop" are
commonly the names of the functions associated with adding an element
to the top or removing the topmost element, respectively.

Normally, a "pop" function modifies the stack by removing the topmost
element from the stack and then returns the value of that element.
However, this is not very friendly with the functional paradigm as it
causes a side-effect. As a result, we'll separate "pop" into two
functions: "top", which returns the topmost element from the stack,
and "pop", which returns a modified stack with the topmost element
removed.

......................................................................
Exercise 3A: Complete the stack module implementation.
......................................................................*)

module IntListStack =
  struct
    exception EmptyStack
    type stack = int list

    (* Returns an empty stack *)
    
# 67
    let empty () : stack = []

    
# 70
    (* Add an element to the top of the stack *)
    
# 74
    let push (i : int) (s : stack) : stack = i :: s

    
# 77
    (* Return the value of the topmost element on the stack *)
    
# 81
    let top (s: stack) : int =
      match s with
      | [] -> raise EmptyStack
      | h :: _ -> h

    
# 87
    (* Return a modified stack with the topmost element removed *)
    
# 91
    let pop (s : stack) : stack =
      match s with
      | [] -> raise EmptyStack
      | _::t -> t
  
# 96
  end ;;

(*
Great! You've created your stack. Let's use it and consider some
implications.

......................................................................
Exercise 3B: Write a function "small_stack" that uses the IntListStack
implementation to create a new stack with the values "5" and then "1"
pushed in that order.
......................................................................*)

let small_stack (): IntListStack.stack =
  
# 112
  IntListStack.empty () |> IntListStack.push 5 |> IntListStack.push 1 ;;

# 115
(*......................................................................
Exercise 3C: Now, use IntListStack methods to write an expression that
stores the value of the topmost element in your stack in "last_el".
......................................................................*)

# 123
let last_el = IntListStack.top (small_stack ()) ;;

# 126
(*......................................................................
Based on our requirements above, what should last_el contain?

Look more closely at the type of data stored in small_stack: it's of
type int list. This is expected, since that's how we defined it, but
this also means that we have the ability to modify the data structure
without using the provided *abstraction* specified by the module.

In other words, the IntListStack module is intended to enforce the
invariant that the elements will always be pushed and popped in a LIFO
manner. But, we could altogether circumvent that by changing the list.

......................................................................
Exercise 3D: Write a function that, given a stack, will invert it
without using any of the IntListStack methods.
......................................................................*)

let invert_stack (s: IntListStack.stack) : IntListStack.stack =
  
# 147
  List.rev s ;;

# 150
(*
Now what would be the result of the top operation on invert_stack?

......................................................................
Exercise 3E: Write an expression using IntListStack methods to get the
top value from a small_stack inverted with invert_stack and store it
in bad_el.
......................................................................*)

# 162
let bad_el = IntListStack.top (invert_stack (small_stack ())) ;;

# 165
(*......................................................................
This is bad. We have broken through the *abstraction barrier* defined
by the IntListStack module. You may wonder: "if I know that the module
is defined by a list, why not have the power to update it manually?"

Several reasons:

First, as we've just done, it was entirely possible
for us as a user of the module to completely change the internal
representation. Imagine what would happen for a more complex module
that allowed us to break an invariant! From Problem Set 3, what would
break if a person could change a zero bignum to also set the negative
flag? Or pass a list of coefficients that are larger than the base?

Second, what if we want to change the module implementation for a more
sophisticated version? Stacks are fairly simple, yet there are
multiple reasonable ways of implementing stack regimen. And more
complex data structures could certainly need several iterations of
implementation before settling on a final version.

Let's preserve the abstraction barrier by writing an interface for
this module. The signature will use an *abstract type* for stacks,
without revealing the concrete implementation type. Because of that,
users of modules satisfying the signature will have no access to the
values of the type except through the abstraction provided by the
functions and values in the signature.

......................................................................
Exercise 3F: Define an interface for an integer stack, called
INT_STACK. Be very careful in deciding what type of data your function
types in the signature should use; it's not "int list", even though
that's the type you used in your implementation.
......................................................................*)

module type INT_STACK =
  sig
    
# 202
    type stack
    exception EmptyStack
    val empty : unit -> stack
    val push : int -> stack -> stack
    val top : stack -> int
    val pop : stack -> stack
  
# 209
  end ;;

(*
Now, we'll apply the INT_STACK interface to the IntListStack.
 *)

module SafeIntListStack = (IntListStack : INT_STACK) ;;

(*......................................................................
Exercise 3G: Write a function "safe_stack" that uses SafeIntListStack
methods to return a stack of type SafeIntListStack, with the integers
5 and then 1 pushed onto it.

Notice: what type is safe_stack? You should no longer be able to
perform list operations directly on it, which means the stack
preserves its abstraction barrier.
......................................................................*)

# 230
let safe_stack () : SafeIntListStack.stack =
  let open SafeIntListStack in
  empty () |> push 5 |> push 1 ;;
