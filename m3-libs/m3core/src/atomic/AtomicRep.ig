GENERIC INTERFACE AtomicRep (Rep);
TYPE T = RECORD bits: BITS BITSIZE (Rep.T) FOR Rep.T END;
(* T must be a type that is not directly assignable to Rep.T, but can be used
   to hold a value of type Rep.T.  It is preferable that T and Rep.T have the
   same size. *)
END AtomicRep.
