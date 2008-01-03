(* Created on Sun Nov 23 13:03:43 PST 1997 by heydon *)
(* Last modified on Thu Nov 27 11:46:51 PST 1997 by heydon *)

(*
Of the three table implementations we measured that support ordered
iteration, red black trees outperformed both treaps and skip lists.
Moreover, the red black implementation guarantees logarithmic {\it
worst case} performance, while the other two implementations have
only {\it expected} logarithmic performance.

Although the red black implementation is slightly longer and more
complicated than the other implementations, the implementation
described here took only a day to implement and test. Because all
three sorted tables were implemented using Modula-3's generic
interfaces and modules, their implementations can be easily
reused.
*)
