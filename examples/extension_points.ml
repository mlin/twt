let usr = [%nop]

let%nop [@nop] _ = 0

let f x =
	let%nop x' = x+1
	x' [@nop]
