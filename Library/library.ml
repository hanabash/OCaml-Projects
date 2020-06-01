let welcome = "Welcome to the Oakland, California Municipal Library (OCaML)"

(* These types are defined for you. You should not modify them *)
type catalog_item = Book of string * string | Movie of string * int | CD of string * string | Computer
type checkout_entry = Item of catalog_item | New of checkout_entry | Pair of checkout_entry * checkout_entry
type checkout = Checkout of checkout_entry * int option

(* Examples *)
(* These are some examples of checkout_item. You should test locally with these before submitting *)
let i0 = Book ("Types and Programming Languages", "Benjamin Pierce")
let i1 = Movie ("The Imitation Game", 2014)
let i2 = Computer

(* These are some examples of checkout_entry. You should test locally with these before submitting *)
let e0 = Item i0
let e1 = Item i1
let e2 = Item i2

let e3 = Item (CD ("Songs to Test By", "Aperture Science Psychoacoustic Laboratories"))
let e4 = New (Item (Book ("UNIX: A History and a Memoir", "Brian W. Kernighan")))

let e5 = Pair (
    Item (Movie ("WarGames", 1983)),
    Item (Movie ("Sneakers", 1992))
)

let e6 = Pair (
    Pair (
        Item (Book ("The Unix Programming Environment", "Brian W. Kernighan and Rob Pike")), 
        Item (Book ("The C Programming Language", "Brian Kernighan and Dennis Ritchie"))
    ),
    Item (Book ("The AWK Programming Language", "Alfred V. Aho, Brian W. Kernighan, and Peter J. Weinberger"))
)

(* This is an exmaple of a checkout list. You should test locally with it before submitting *)
let checked_out = [Checkout (e1, Some 2); Checkout (e2, None); Checkout (e4, Some 1); Checkout (e5, Some 2)]

(* Display item as string *)
let string_of_item (i : catalog_item) : string = 
	match i with
	Book (title, author) -> title ^ " by " ^ author
	| Movie (name, year) -> name ^ " (" ^ (string_of_int year) ^ ")"
	| CD (album, artist) -> album ^ " by " ^ artist
	| Computer -> "Public Computer"

(* Display entry as string *)
let rec string_of_entry (e : checkout_entry) : string =
	match e with
	Item (cat_item) -> string_of_item (cat_item)
	| New (new_entry) -> "(NEW) " ^ string_of_entry new_entry
	| Pair (entry_one, entry_two) -> string_of_entry entry_one ^ " and " ^ string_of_entry entry_two

(* Helper function for daily_fine *)
let classify_item (it : catalog_item) : string =
	match it with
	Book (a, b) -> "Book"
	| Movie (c, d) -> "Movie"
	| CD (e, f) -> "CD"
	| Computer -> "Computer"

(* Return the daily fine for an overdue item *)
let rec daily_fine (entry: checkout_entry) : float =
	match entry with
	Item (cat_item) ->
	if (classify_item cat_item) = "Book" then 0.25
	else if (classify_item cat_item) = "Movie" then 0.50
	else if (classify_item cat_item) = "CD" then 0.50
	else 0.00
	| New (some_entry) -> 2. *. daily_fine some_entry
	| Pair (first_entry, second_entry) -> daily_fine first_entry +. daily_fine second_entry

(* Given a list of items and days overdue, compute the total fine *)
let rec total_fine (l : checkout list) : float =
	match l with
	[] -> 0.00
	| hd::tl -> 
		(match hd with
		Checkout (some_entry, some_option) -> 
			(match some_option with
			None -> 0.00 +. total_fine tl
			| Some n -> ((float_of_int n) *. (daily_fine some_entry)) +. total_fine tl))
;;

