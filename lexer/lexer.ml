let simpleSymbolTokenizer (cq : CharQueue.t) : (Tokens.t option, string) result =
	let location = Location.copy cq.location
	in let (token : Tokens.t option)  =
		match CharQueue.peek cq with
		| Some '(' -> Some (LParen location)
		| Some ')' -> Some (RParen location)
		| Some '.' -> Some (Dot location)
		| Some ':' -> Some (Colon location)
		| _ -> None
	in
	if Option.is_some token then CharQueue.junk cq;
	Ok token

let lambdaTokenizer (cq : CharQueue.t) : (Tokens.t option, string) result =
	let location = Location.copy cq.location
	in match CharQueue.npeek 2 cq with
	| '\\'::_ -> CharQueue.junk cq; Ok (Some (Lambda location))
	(*Unicode lambda*)
	| ['\xCE'; '\xBB'] -> CharQueue.njunk 2 cq; Ok (Some (Lambda location)) 
	| _ -> Ok None

let arrowTokenizer (cq : CharQueue.t) : (Tokens.t option, string) result =
	let location = Location.copy cq.location
	in match CharQueue.npeek 2 cq with
	| ['-'; '>'] -> CharQueue.njunk 2 cq; Ok (Some (Arrow location))
	| _ -> Ok None

let identifierTokenizer (cq : CharQueue.t) : (Tokens.t option, string) result =
	let alphaCharset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	in let alphaNumCharset = alphaCharset ^ "0123456789"
	in let rec loop (ident : string) : string =
		match CharQueue.peek cq with
		| Some c when String.contains alphaNumCharset c ->
			CharQueue.junk cq; loop (ident ^ (String.make 1 c))
		| _ -> ident
	in let location = Location.copy cq.location
	in match CharQueue.peek cq with
	| Some c when String.contains alphaCharset c ->
		CharQueue.junk cq; Ok (Some (Identifier (loop (String.make 1 c), location)))
	| _ -> Ok None

let rec lex (cq : CharQueue.t) : unit =
	let tokenizers = [|
		simpleSymbolTokenizer;
		lambdaTokenizer;
		arrowTokenizer;
		identifierTokenizer;
	|]
	in let rec try_tokenize (i : int) : ((Tokens.t, string) result) option =
		if i < Array.length tokenizers then
			match tokenizers.(i) cq with
			| Ok (Some t) -> Some (Ok t)
			| Ok None -> try_tokenize (i + 1)
			| Error e -> Some (Error e)
		else None
	in match try_tokenize 0 with
	| Some (Ok t) -> print_endline (Tokens.to_string t); lex cq
	| Some (Error e) -> raise (Failure e)
	| None ->
		begin match CharQueue.peek cq with
		| Some ' ' | Some '\n' -> CharQueue.junk cq; lex cq
		| Some c ->
			Printf.eprintf "Error: Unexpected character %C at %s\n"
				c (Location.to_string cq.location);
			exit 1;
		| None -> ()
		end

let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			lex (CharQueue.of_channel (open_in Sys.argv.(i)) Sys.argv.(i))
		done
	else
		lex (CharQueue.of_channel stdin "stdin")
