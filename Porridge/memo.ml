
module Make (M:Hashtbl.HashedType) : sig

  val make : (M.t -> 'a) -> M.t -> 'a
  val make_rec : ((M.t -> 'a) -> M.t -> 'a) -> M.t -> 'a

end = struct

  module Hashtbl = Hashtbl.Make(M)

  let make f =
    let h = Hashtbl.create 257 in
      fun x ->
        try Hashtbl.find h x with
          | Not_found ->
              let y = f x in
                Hashtbl.add h x y ;
                y

  let make_rec f =
    let h = Hashtbl.create 257 in
    let rec ff x =
      try Hashtbl.find h x with
        | Not_found ->
            let y = f ff x in
              Hashtbl.add h x y ;
            y
    in ff

end

module Fake (M:Hashtbl.HashedType) : sig

  val make : (M.t -> 'a) -> M.t -> 'a
  val make_rec : ((M.t -> 'a) -> M.t -> 'a) -> M.t -> 'a

end = struct

  module Hashtbl = Hashtbl.Make(M)

  let make f = f

  let rec make_rec f x = f (fun x -> make_rec f x) x

end
