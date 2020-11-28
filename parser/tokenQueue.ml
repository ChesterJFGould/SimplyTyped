(*This is necessary so that if we get an EOF we can tell the user where*)

type t = {
	stream : Tokens.t Stream.t;
	mutable location : Location.t;
}

let of_channel (ic : in_channel) : t =
	let rec next_token (_ : int) : Tokens.t option =
		try
			match Tokens.of_string (input_line ic) with
			| Some t -> Some t
			| None -> next_token 0
		with End_of_file -> None
	in {
		stream = Stream.from next_token;
		location = Location.of_file "";
	}

let next (tq : t) : Tokens.t option =
	try
		let current = Stream.next tq.stream
		in
		tq.location <- Tokens.location current;
		Some current
	with Stream.Failure -> None

let junk (tq : t) : unit =
	try
		let current = Stream.next tq.stream
		in
		tq.location <- Tokens.location current;
		()
	with Stream.Failure -> ()

let rec njunk (n : int) (tq : t) : unit =
	match n with
	| 0 -> ()
	| n -> junk tq; njunk (n - 1) tq

let peek (tq : t) : Tokens.t option = Stream.peek tq.stream

let npeek (n : int) (tq : t) : Tokens.t list = Stream.npeek n tq.stream
