(** Boolean/Numerical domain linked to environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a Mtbdddomain0.man
  (** Makes a BDDAPRON manager from an APRON manager.
      If [global=true] (default: [false]), uses a global (persistent)
      BDD cache for the operations [is_leq], [join], [meet] 
      and [exist] (internal).
  *)

include Domainlevel1.Level1 with type 'a man = 'a Mtbdddomain0.man
			    and type 'a t0 = 'a Mtbdddomain0.t

