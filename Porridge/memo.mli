(** Functor providing memoized function builders,
  * for an arbitrary hashed type. *)
module Make (M : Hashtbl.HashedType) : sig
  val make : (M.t -> 'a) -> M.t -> 'a
  val make_rec : ((M.t -> 'a) -> M.t -> 'a) -> M.t -> 'a
end

module Fake (M : Hashtbl.HashedType) : sig
  val make : (M.t -> 'a) -> M.t -> 'a
  val make_rec : ((M.t -> 'a) -> M.t -> 'a) -> M.t -> 'a
end
