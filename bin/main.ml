type 'a unit_of_measurement = 
  | Celsius 
  | Fahrenheit 
  | Kelvin

let convert_value : type a b. a unit_of_measurement -> b unit_of_measurement -> float -> float = fun from_unit to_unit value ->
  match from_unit, to_unit with 
  | Celsius, Fahrenheit -> value *. (9. /. 5.) +. 32. 
  | Celsius, Kelvin     -> value +. 273.15 
  | Fahrenheit, Celsius -> (value -. 32.) *. (5. /. 9.) 
  | Fahrenheit, Kelvin  -> (value -. 32.) *. (5. /. 9.) +. 273.15 
  | Kelvin, Celsius     -> value -. 273.15 
  | Kelvin, Fahrenheit  -> (value -. 273.15) *. (9. /. 5.) +. 32. 
  | _, _                   -> invalid_arg "Invalid conversion"

let rec get_user_number () = 
  print_string "Please enter a float number: ";
  try
    read_float ()
  with 
  | Failure _ -> 
    print_endline "Invalid input. Please enter a valid float number (eg. 0., 32., 273.15).";
    get_user_number()

let rec get_conversion_type unit = 
  Printf.printf "Enter a %s unit of measument (celsius, fahrenheit or kelvin): " unit;
  let str = read_line () in 
  match String.lowercase_ascii str with 
  | "celsius" -> Celsius
  | "fahrenheit" -> Fahrenheit
  | "kelvin" -> Kelvin
  | _ -> 
    print_endline "Invalid input. Please enter a valid measument.";
    get_conversion_type unit

let program () = 
  let value = get_user_number () in 
  let source = get_conversion_type "source" in 
  let target = get_conversion_type "target" in 
  let result = convert_value source target value in
  Printf.printf "result: %.2f\n\n" result;;

let rec handle_input () =
  print_string "Enter 'start' to begin the program or type 'quit' to exit: ";
  match read_line () with
  | "start" -> 
    program ();
    handle_input ()
  | "quit" -> print_endline "Exiting program."
  | _ -> handle_input ()

let () = handle_input ()
