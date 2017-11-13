type channel = int
type t = channel
let nb_chan = 4
let of_int x =
  if x < nb_chan then x else failwith "Out of bound channel"
let to_int ch = ch
let to_char c = Char.chr (Char.code 'c' + c)

let c = 0
let d = 1
let e = 2
