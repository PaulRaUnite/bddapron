(** Boolean/Numerical domain linked to environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

include Domainlevel1.Level1 with type 'a man = 'a Bdddomain0.man
			    and type 'a t0 = 'a Bdddomain0.t


val make_man : 'a Apron.Manager.t -> 'a man
  (** Makes a BDDAPRON manager from an APRON manager, and fills
  options with default values *)

val canonicalize : ?apron:bool -> ?unique:bool -> ?disjoint:bool -> 'a man -> 'a t -> unit
  (** Canonicalize an abstract value by ensuring uniqueness and
      disjointness properties. If [apron] is true, then also
      normalize APRON abstract values. By default: [apron=false,
      unique=disjoint=true]. *)
