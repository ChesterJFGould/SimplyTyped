type kind =
	| Base of string
	| Function of kind * kind

let rec string_of_kind (k : kind) : string =
	match k with
	| Base k -> k
	| Function (f, t) -> Printf.sprintf "(%s->%s)"
		(string_of_kind f) (string_of_kind t)

let kind_from (kind : kind) : kind =
	match kind with
	| Base k -> Base k
	| Function (f, _) -> f

let kind_of_string (s : string) : kind option =
	let rec base (cs : char Stream.t) (ident : string) : kind option =
		match Stream.npeek 2 cs with
		| ')'::_ -> Some (Base ident)
		| ['-'; '>'] -> Some (Base ident)
		| c::_ -> Stream.junk cs; base cs (ident ^ (String.make 1 c))
		| [] -> Some (Base ident)
	and func (cs : char Stream.t) : kind option =
		match Stream.peek cs with
		| Some '(' ->
			Stream.junk cs;
			let f = kind cs
			in begin match Stream.npeek 2 cs with
			| ['-'; '>'] ->
				Stream.junk cs; Stream.junk cs;
				let t = kind cs
				in begin match (f, t, Stream.peek cs) with
				| (None, _, _) -> None
				| (_, None, _) -> None
				| (Some f, Some t, Some ')') -> Stream.junk cs ;Some (Function (f, t))
				| _ -> None
				end
			| _ -> None
			end
		| _ -> None
	and kind (cs : char Stream.t) : kind option =
		match Stream.peek cs with
		| Some '(' -> func cs
		| Some _ -> base cs ""
		| None -> None
	in kind (Stream.of_list (List.init (String.length s) (String.get s)))

type t =
	| Bound of string * kind * Location.t
	| Free of string * kind * Location.t
	| Lambda of string * t * kind * Location.t
	| Application of t * t * kind * Location.t
	| Environment of string * t * t * kind * Location.t

let kind (node : t) : kind =
	match node with
	| Bound (_, k, _) -> k
	| Free (_, k, _) -> k
	| Lambda (_, _, k, _) -> k
	| Application (_, _, k, _) -> k
	| Environment (_, _, _, k, _) -> k

let location (node : t) : Location.t =
	match node with
	| Bound (_, _, l) -> l
	| Free (_, _, l) -> l
	| Lambda (_, _, _, l) -> l
	| Application (_, _, _, l) -> l
	| Environment (_, _, _, _, l) -> l

let rec serialize (oc : out_channel) (node : t) : unit =
	match node with
	| Bound (n, k, l) -> Printf.fprintf oc "Bound %s %s %s\n"
		n (string_of_kind k) (Location.to_string l)
	| Free (n, k, l) -> Printf.fprintf oc "Free %s %s %s\n"
		n (string_of_kind k) (Location.to_string l)
	| Lambda (var, b, k, l) ->
		Printf.fprintf oc "Lambda %s %s %s\n"
			var (string_of_kind k) (Location.to_string l);
		serialize oc b
	| Application (left, right, k, l) ->
		Printf.fprintf oc "Application %s %s\n"
			(string_of_kind k) (Location.to_string l);
		serialize oc left;
		serialize oc right
	| Environment (var, sub, b, k, l) ->
		Printf.fprintf oc "Environment %s %s %s\n"
			var (string_of_kind k) (Location.to_string l);
		serialize oc sub;
		serialize oc b

let rec deserialize (ic : in_channel) : (t, string) result =
	match String.split_on_char ' ' (input_line ic) with
	| "Bound"::n::k::l -> 
		begin match (kind_of_string k, Location.of_string_list l) with
		| (None, _) -> Error (Printf.sprintf "Failed to deserialize type %s" k)
		| (_, None) -> Error (Printf.sprintf "Failed to deserialize location %s" (String.concat " " l))
		| (Some k, Some l) -> Ok (Bound (n, k, l))
		end
	| "Free"::n::k::l ->
		begin match (kind_of_string k, Location.of_string_list l) with
		| (None, _) -> Error (Printf.sprintf "Failed to deserialize type %s" k)
		| (_, None) -> Error (Printf.sprintf "Failed to deserialize location %s" (String.concat " " l))
		| (Some k, Some l) -> Ok (Free (n, k, l))
		end
	| "Lambda"::var::k::l ->
		begin match (kind_of_string k, Location.of_string_list l, deserialize ic) with
		| (None, _, _) -> Error (Printf.sprintf "Failed to deserialize type %s" k)
		| (_, None, _) -> Error (Printf.sprintf "Failed to deserialize location %s" (String.concat " " l))
		| (_, _, Error e) -> Error e
		| (Some k, Some l, Ok b) -> Ok (Lambda (var, b, k, l))
		end
	| "Application"::k::l ->
		let left = deserialize ic
		in let right = deserialize ic
		in
		begin match (kind_of_string k, Location.of_string_list l, left, right) with
		| (None, _, _, _) -> Error (Printf.sprintf "Failed to deserialize type %s" k)
		| (_, None, _, _) -> Error (Printf.sprintf "Failed to deserialize location %s" (String.concat " " l))
		| (_, _, Error e, _) -> Error e
		| (_, _, _, Error e) -> Error e
		| (Some k, Some l, Ok left, Ok right) -> Ok (Application (left, right, k, l))
		end
	| "Environment"::var::k::l ->
		let sub = deserialize ic
		in let b = deserialize ic
		in
		begin match (kind_of_string k, Location.of_string_list l, sub, b) with
		| (None, _, _, _) -> Error (Printf.sprintf "Failed to deserialize type %s" k)
		| (_, None, _, _) -> Error (Printf.sprintf "Failed to deserialize location %s" (String.concat " " l))
		| (_, _, Error e, _) -> Error e
		| (_, _, _, Error e) -> Error e
		| (Some k, Some l, Ok sub, Ok b) -> Ok (Environment (var, sub, b, k, l))
		end
	| s -> Error (Printf.sprintf "Cannot deserialize \"%s\"" (String.concat " " s))

let fprint_tree (oc : out_channel) (node : t) : unit =
	let rec tree (node : t) (indent : string) (last : bool) : unit =
		Printf.fprintf oc "%s" indent;
		let next_indent =
			if last then begin
				Printf.fprintf oc "\\-";
				indent ^ "  "
			end else begin
				Printf.fprintf oc "|-";
				indent ^ "| "
			end
		in match node with
		| Bound (n, k, _) -> Printf.fprintf oc "%s : %s\n" n (string_of_kind k)
		| Free (n, k, _) -> Printf.fprintf oc "%s : %s\n" n (string_of_kind k)
		| Lambda (var, b, k, _) ->
			Printf.fprintf oc "λ %s : %s\n" var (string_of_kind k);
			tree b next_indent true
		| Application (l, r, k, _) ->
			Printf.fprintf oc "@ : %s\n" (string_of_kind k);
			tree l next_indent false;
			tree r next_indent true
		| Environment (var, sub, b, k, _) ->
			Printf.fprintf oc ":= %s : %s\n" var (string_of_kind k);
			tree sub next_indent false;
			tree b next_indent true
	in tree node "" true

let rec as_string (node : t) : string =
	match node with
	| Bound (n, _, _) -> n
	| Free (n, k, _) -> Printf.sprintf "(%s : %s)" n (string_of_kind k)
	| Lambda (var, b, k, _) -> Printf.sprintf "λ %s : %s. %s" var (string_of_kind (kind_from k)) (as_string b)
	| Application (l, r, _, _) -> Printf.sprintf "(%s) (%s)" (as_string l) (as_string r)
	| Environment (var, sub, b, _, _) -> Printf.sprintf "(%s) [%s := %s]" (as_string b) var (as_string sub)
