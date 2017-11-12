type channel = int
type t = channel
let nb_chan = 4
let of_int x =
  if x < nb_chan then x else failwith "Out of bound channel"
let to_int ch = ch
