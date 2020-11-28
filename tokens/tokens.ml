type t =
	| Lambda of Location.t
	| Identifier of string * Location.t
	| LParen of Location.t
	| RParen of Location.t
	| Dot of Location.t
	| Colon of Location.t
	| Arrow of Location.t

let location (token : t) : Location.t =
	match token with
	| Lambda l -> l
	| Identifier (_, l) -> l
	| LParen l -> l
	| RParen l -> l
	| Dot l -> l
	| Colon l -> l
	| Arrow l -> l

let to_string (token : t) : string =
	match token with
	| Lambda l -> Printf.sprintf "Lambda %s" (Location.to_string l)
	| LParen l -> Printf.sprintf "LParen %s" (Location.to_string l)
	| RParen l -> Printf.sprintf "RParen %s" (Location.to_string l)
	| Dot l -> Printf.sprintf "Dot %s" (Location.to_string l)
	| Colon l -> Printf.sprintf "Colon %s" (Location.to_string l)
	| Arrow l -> Printf.sprintf "Arrow %s" (Location.to_string l)
	| Identifier (name, l) ->
		Printf.sprintf "Identifier %s %s" name (Location.to_string l)

let of_string (s : string) : t option =
	match String.split_on_char ' ' s with
	| "Lambda"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Lambda l)
		| None -> None
		end
	| "LParen"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LParen l)
		| None -> None
		end
	| "RParen"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (RParen l)
		| None -> None
		end
	| "Dot"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Dot l)
		| None -> None
		end
	| "Colon"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Colon l)
		| None -> None
		end
	| "Arrow"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Arrow l)
		| None -> None
		end
	| "Identifier"::name::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Identifier (name, l))
		| None -> None
		end
	| _ -> None

let literal (token: t) : string =
	match token with
	| Lambda _ -> "Lambda"
	| LParen _ -> "("
	| RParen _ -> ")"
	| Dot _ -> "."
	| Colon _-> ":"
	| Arrow _ -> "->"
	| Identifier (n, _) -> n 
