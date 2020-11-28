let rec substitute (sub : Nodes.t) (var : string) (node : Nodes.t) : Nodes.t =
	match node with
	| Bound (v, _, _) when v = var -> sub
	| Lambda (v, b, k, l) when v <> var ->
		Lambda (v, substitute sub var b, k, l)
	| Application (left, right, k, l) ->
		Application (substitute sub var left, substitute sub var right, k, l)
	| Environment (v, s, b, k, l) when v <> var ->
		Environment (v, substitute sub var s, substitute sub var b, k, l)
	| n -> n

let rec beta_reduce (node : Nodes.t) : Nodes.t =
	match node with
	| Application (left, right, _, _) ->
		begin match beta_reduce left with
		| Lambda (v, b, k, l) -> beta_reduce (Environment (v, right, b, k, l))
		| n ->
			begin match Nodes.kind n with
			(* While this is a type error, I'm leaving type checking to the parser *)
			| Base k -> Application (n, beta_reduce right, Base k, Nodes.location left)
			| Function (_, t) -> Application (n, beta_reduce right, t, Nodes.location left)
			end
		end
	| Environment (var, sub, b, _, _) -> beta_reduce (substitute sub var b)
	| n -> n
let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			match Nodes.deserialize (open_in Sys.argv.(i)) with
			| Ok n -> print_endline (Nodes.as_string (beta_reduce n))
			| Error e -> prerr_endline e; exit 1
		done
	else
		match Nodes.deserialize stdin with
		| Ok n -> print_endline (Nodes.as_string (beta_reduce n))
		| Error e -> prerr_endline e; exit 1
