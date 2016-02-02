(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

MODULE UniEncoding

(* Various ways of encoding characters in sequential streams, including
   the Uncode encoding schemes, and others.
*) 

; PROCEDURE EncImage ( Enc : Encoding ) : TEXT  

  = BEGIN 
      CASE Enc 
      OF Encoding . Null => RETURN "Null"
      | Encoding . Internal => RETURN "Internal"
      | Encoding . ISO8859_1 => RETURN "ISO8859_1"
      | Encoding . CM3WC => RETURN "CM3WC"
      | Encoding . UCS2 => RETURN "UCS2"
      | Encoding . UCS2LE => RETURN "UCS2LE"
      | Encoding . UCS2BE => RETURN "UCS2BE"
      | Encoding . UTF8 => RETURN "UTF8"
      | Encoding . UTF16 => RETURN "UTF16"
      | Encoding . UTF16LE => RETURN "UTF16LE"
      | Encoding . UTF16BE => RETURN "UTF16BE"
      | Encoding . UTF32 => RETURN "UTF32"
      | Encoding . UTF32LE => RETURN "UTF32LE"
      | Encoding . UTF32BE => RETURN "UTF32BE"
      END (* CASE *) 
    END EncImage 

; BEGIN 
  END UniEncoding  
. 


