let rec main magic_number =
 Printf.printf "Your guess? "
 let guess = int_of_string (read_line ())
 if guess > magic_number then
  Printf.printf "Too high!\n"
  main magic_number
 else if guess < magic_number then
  Printf.printf "Too low!\n"
  main magic_number
 else
  Printf.printf "You win!\n"
  exit 0

Random.self_init ()

main (Random.int 100)
