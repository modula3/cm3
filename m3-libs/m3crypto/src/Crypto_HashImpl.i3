(* Copyright 2020,2021 Eric Sessoms / MIT License *)

INTERFACE Crypto_HashImpl;
(* Common implementation shared by basic digest algorithms

It is important to note that most newer algorithms (e.g., SHA-3) no
longer use the 64-byte block size characteristc of the older
algorithms.  Some of the utilities in this module may still be useful
in implementing newer algorithms, but `Crypto_HashImpl.T` will not be
a suitable base class to derive from. *)

IMPORT
  Crypto_Hash AS Hash;


TYPE
  T = Hash.T OBJECT
    (* Base class for 64-byte block-size algorithms *)

    buffer: ARRAY [0..63] OF CHAR;
    used, msglen: CARDINAL
  METHODS
    chunk();
    (* Update hash with one block of data *)
  OVERRIDES
    blockSize := BlockSize;
    updateString := UpdateString
  END;


PROCEDURE BlockSize(hash: T): CARDINAL;
  (* Number of bytes consumed in update *)


PROCEDURE UpdateString(hash: T; READONLY data: ARRAY OF CHAR);
  (* Hash message data *)


PROCEDURE GetWord32be(
    READONLY buffer: ARRAY OF CHAR; start: CARDINAL): INTEGER;
  (* Read 4 bytes as a 32-bit big-endian word *)


PROCEDURE GetWord32le(
    READONLY buffer: ARRAY OF CHAR; start: CARDINAL): INTEGER;
  (* Read 4 bytes as a 32-bit little-endian word *)


PROCEDURE LeftRotate32(word: INTEGER; bits: CARDINAL): INTEGER;
  (* Rotate the lower 32-bits of a word  *)


PROCEDURE Pad(hash: T);
  (* Add a single 1-bit and pad out the hash with zeros *)


PROCEDURE PutWord32be(
    VAR buffer: ARRAY OF CHAR; start: CARDINAL; value: INTEGER);
  (* Write 4 bytes as a 32-bit big-endian word *)


PROCEDURE PutWord32le(
    VAR buffer: ARRAY OF CHAR; start: CARDINAL; value: INTEGER);
  (* Write 4 bytes as a 32-bit little-endian word *)


END Crypto_HashImpl.
