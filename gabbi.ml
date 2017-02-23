(*

A collection of statements contained in an "Gabbi" module.

 *)

type house =
   Adams | Lowell | Quincy |
   Kirkland | Winthrop | Eliot |
   Leverett | Mather | Dunster |
   Pforzheimer | Cabot | Currier

type info = {
   hometown : string;
   year : int;
   concentration : string;
   house : house
}

let hometown = "Raleigh, NC"
let year = 2018
let concentration = "Computer Science, but I'm also getting a super cool masters degree."
let house = Kirkland
let fold = List.fold_right (+)

let info = {
   hometown;
   year;
   concentration;
   house
}

let grade_assignment assignment =
  "Everyone gets 0/0 for pset " ^ string_of_int assignment ^ "!"

let favorite_function x y = x +. float_of_int (succ (int_of_float y))
let least_favorite_function = ( ** )

let print_info () =
   let _ = print_string (
       info.hometown ^ "\n" ^
       string_of_int year ^ "\n" ^
       info.concentration ^ "\n") in
   match info.house with
   | Cabot -> print_string "Kirkland!\n"
   | _ -> failwith "Do any other houses matter?"
