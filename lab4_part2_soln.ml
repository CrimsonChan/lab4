# 1 "lab4_part2.mlpp"
(*
			      CS51 Lab 4
			  Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab4_part1.ml -- Part 1: Implementing Modules
-> lab4_part2.ml -- Part 2: Files as modules (this file)
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
   lab4_part5.ml -- Part 5: Functors

 *)

(*====================================================================
Part 2: Files as modules

A useful feature of OCaml is that it *automatically* wraps functions
and values defined in a single file into a module named after that
file during compilation. The module name is the name of the file with
the first letter capitalized. This functionality is in addition to the
manual definition of modules as you've just used in Part 1, but it is
a convenient way of separating logic into separate namespaces when
writing a large program.

There are several other source files included in this lab, other than
the lab4 ML files. Take a look at gabbi.ml and sam.ml to see what
functions and values they contain. You will *not* need to modify
gabbi.ml or sam.ml to complete any of the exercises below.

The `ocamlbuild` command should automatically find modules that you've
written in the same directory as your source, compile those additional
files, and link them to your compiled program. You can then access
functions from those files under the module name, which (again) is the
name of the file with the first letter capitalized.

If you're testing with the top-level, like utop, it will not
automatically find those modules and evaluate them for you. However,
you can do so manually yourself with the mod_use directive, like this:

    #mod_use "gabbi.ml" ;;

......................................................................
Exercise 2A: Apply the values 4.0 and 2.5 to Gabbi's least favorite
function, and store the result in least_fav.
......................................................................*)

# 57
let least_fav = Gabbi.least_favorite_function 4.0 2.5 ;;

# 60
(*......................................................................
Exercise 2B: Apply the same values to Sam's favorite function, naming
the result most_fav.
......................................................................*)

# 68
let most_fav = Sam.favorite_function 4.0 2.5 ;;

# 71
(*......................................................................
We hope you'll find the module system easy to use and convenient, once
you get a hang of the conventions.

Let's investigate one way that a signature could be useful. Although
there are some differences between module Sam and module Gabbi,
there are also some similarities. Several functions and values have
the same *name* and *type*, even if they are implemented differently. We
can leverage this, and write a signature that can be applied to both
modules and will be useful to determine which functions we can call
in an identical fashion between both modules.

......................................................................
Exercise 2C: Define a signature called TF that maximally exposes as
much of both modules as possible, that is, those values and functions
that have the same name and type.
......................................................................*)

# 97
module type TF =
  sig
    type info
    val info : info
    val hometown : string
    val print_info : unit -> unit
    val grade_assignment : int -> string
    val favorite_function : float -> float -> float
    val fold : int list -> int -> int
  end ;;

# 109
(*......................................................................
Exercise 2D: Now, create two new modules, named TFGabbi and TFSam,
that correspond to the Gabbi and Sam modules, respectively, but
restricted to the TF signature.

Hint: Creation of each module should only take one line of
code. You'll just want to replace the trivial module definition
"struct end" in the code below.
......................................................................*)

# 123
module TFGabbi = (Gabbi : TF) ;;
module TFSam = (Sam : TF) ;;
