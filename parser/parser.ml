let parse_type_qualifier (tq : TokenQueue.t)
: (Nodes.kind, string * Location.t) result =
	let rec parse_type () : (Nodes.kind, string * Location.t) result =
		let (left : (Nodes.kind, string * Location.t) result) =
			match TokenQueue.next tq with
			| Some (Identifier (name, _)) -> Ok (Base name)
			| Some (LParen _) ->
				let k = parse_type ()
				in begin match TokenQueue.next tq with
				| Some (RParen _) -> k
				| Some t -> Error (Printf.sprintf "Expected closing parenthesis but instead
					found %s" (Tokens.literal t), (Tokens.location t))
				| None -> Error ("Unexpected end of file, expected closing parenthesis",
					tq.location)
				end
			| Some t -> Error (Printf.sprintf "Expected a type, instead found %s"
				(Tokens.literal t), (Tokens.location t))
			| None -> Error ("Unexpected end of file, expected a type", tq.location)
		in match (left, TokenQueue.peek tq) with
		| (Ok left, Some (Arrow _)) ->
			TokenQueue.junk tq;
			begin match parse_type () with
			| Ok right -> Ok (Function (left, right))
			| Error e -> Error e
			end
		| _ -> left
	in match TokenQueue.next tq with
	| Some (Colon _) -> parse_type ()
	| Some t -> Error (Printf.sprintf "Expected type qualifier, instead found %s"
		(Tokens.literal t), Tokens.location t)
	| None -> Error ("Unexpected end of file, expected type qualifier",
		tq.location)

let check_application (left : Nodes.t) (right : Nodes.t)
: (Nodes.kind, string * Location.t) result =
	match Nodes.kind left with
	| Base k -> Error (Printf.sprintf "Cannot use type %s as function type in
		application" k, (Nodes.location left))
	| Function (f, _) when f <> (Nodes.kind right) ->
		Error (Printf.sprintf "Cannot apply type %s to function of type %s"
			(Nodes.string_of_kind (Nodes.kind right))
			(Nodes.string_of_kind (Nodes.kind left)), (Nodes.location right))
	| Function (_, t) -> Ok t
	
let rec parse_expression (tq : TokenQueue.t) (env : (string * Nodes.kind) list)
: (Nodes.t option, string * Location.t) result =
	match TokenQueue.peek tq with
	| Some (Identifier _) -> parse_variable tq env
	| Some (Lambda _) -> parse_lambda tq env
	| Some (LParen _) ->
		TokenQueue.junk tq;
		let expr = parse tq env
		in begin match TokenQueue.next tq with
		| Some (RParen _) -> expr
		| Some t -> Error (Printf.sprintf "Expected closing parenthesis but
			instead found  %s" (Tokens.literal t), (Tokens.location t))
		| None -> Error ("Unexpected end of file, expected closing parenthesis",
			tq.location)
		end
	| Some _ -> Ok None
	| None -> Ok None
and parse_variable (tq : TokenQueue.t) (env : (string * Nodes.kind) list)
: (Nodes.t option, string * Location.t) result =
	match TokenQueue.next tq with
	| Some (Identifier (name, l)) ->
		begin match List.find_opt (fun (v, _) -> v = name) env with
		| Some (_, k) -> Ok (Some (Bound (name, k, l)))
		| None ->
			begin match parse_type_qualifier tq with
			| Ok k -> Ok (Some (Free (name, k, l)))
			| Error e -> Error e
			end
		end
	| Some t -> Error (Printf.sprintf "Expected an Identifier but instead found %s"
		(Tokens.literal t), (Tokens.location t))
	| None -> Error (Printf.sprintf "Unexpected end of file, expected an Identifier", tq.location)
and parse_lambda (tq : TokenQueue.t) (env : (string * Nodes.kind) list)
: (Nodes.t option, string * Location.t) result =
	match TokenQueue.npeek 2 tq with
	| [Lambda l; Identifier (var, _)] ->
		TokenQueue.njunk 2 tq;
		begin match parse_type_qualifier tq with
		| Ok f ->
			begin match TokenQueue.next tq with
			| Some (Dot _) ->
				begin match parse tq ((var, f)::env) with
				| Ok (Some body) -> Ok (Some (Lambda (var, body,
					Function (f, (Nodes.kind body)), l)))
				| Ok None -> Error ("Unexpected end of file, expected lambda body",
					tq.location)
				| Error e -> Error e
				end
			| Some t -> Error (Printf.sprintf "Expected a dot but instead found %s"
				(Tokens.literal t), (Tokens.location t))
			| None -> Error ("Unexpected end of file, expected a dot", tq.location)
			end
		| Error e -> Error e
		end
	| [Lambda _; t] -> Error (Printf.sprintf "Expected an Identifier but instead
		found %s" (Tokens.literal t), Tokens.location t)
	| t::_ -> Error (Printf.sprintf "Expected a Lambda but instead found %s"
		(Tokens.literal t), Tokens.location t)
	| _ -> Error ("Unexpected end of file, expected a Lambda", tq.location)
and parse_application (left : Nodes.t) (tq : TokenQueue.t) (env : (string * Nodes.kind) list)
: (Nodes.t option, string * Location.t) result =
	match parse_expression tq env with
	| Ok (Some right) ->
		begin match check_application left right with
		| Ok k -> parse_application (Application (left, right, k, Nodes.location left)) tq env
		| Error e -> Error e
		end
	| Ok None -> Ok (Some left)
	| Error e -> Error e
and parse (tq : TokenQueue.t) (env : (string * Nodes.kind) list)
: (Nodes.t option, string * Location.t) result =
	match parse_expression tq env with
	| Ok (Some left) -> parse_application left tq env
	| r -> r

let () : unit =
	let parse_channel (ic : in_channel) : unit =
		match parse (TokenQueue.of_channel ic) [] with
		| Ok (Some n) -> Nodes.serialize stdout n
		| Ok None -> ()
		| Error (err, l) -> Printf.eprintf "Error at %s : %s\n"
				(Location.to_string l) err;
			exit 1
	in
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			parse_channel (open_in Sys.argv.(i))
		done
	else
		parse_channel stdin
