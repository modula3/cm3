(* Copyright 1997-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

INTERFACE FileAttr;

IMPORT File, OSError, Pathname, Time, TokScan, Ustat, Word;

CONST
  Brand = "FileAttr";

TYPE
  T <: Public;

  Public = OBJECT
    fileType := FileType.Unknown;
  METHODS
    init(fileType: FileType;
	 mode := -1;
	 modTime := -1.0d0;
	 size := LAST(CARDINAL)): T;
  END;

(* A "FileAttr.T" represents a set of attributes connected to a file.
   There are various kinds of attributes.  Some may be unsupported on
   certain platforms.  Included in a "FileAttr.T" is a mask indicating
   which of the attributes are valid.

   A "FileAttr.T" can be created from a pathname, from a "File.T", or
   from a POSIX file descriptor.  It can also be created from a few
   selected attributes, using the "init" method.  Once a "FileAttr.T"
   has been created, it should be treated as immutable.

   NOTE: The "FileType" and "AttrType" enumerations must not be reordered.
   Add new items to the end only. *)

  FileType = {
    Unknown,		(* Unknown file type. *)
    File,		(* Regular file. *)
    Directory,		(* Directory. *)
    CharDevice,		(* Character device. *)
    BlockDevice,	(* Block device. *)
    SymLink		(* Symbolic link. *)
  };

  AttrType = {
    FileType,		(* File type -- true for all supported file types. *)
    ModTime,
    Size,
    LinkTarget,		(* Target of a symbolic link. *)
    RDev,		(* Device for a device node. *)
    Owner,
    Group,
    Mode,
    Flags,		(* 4.4bsd flags, a la chflags(2). *)
    LinkCount,          (* Hard link count. *)
    Dev,		(* Device holding the inode. *)
    Inode		(* Inode number. *)
  };
  AttrTypes = SET OF AttrType;

  Attrs = ARRAY FileType OF T;
  SupportInfo = ARRAY FileType OF AttrTypes;

EXCEPTION
  UnknownGroup(TEXT);
  UnknownOwner(TEXT);

CONST
  AllAttrTypes = AttrTypes{ FIRST(AttrType) .. LAST(AttrType) };
  AllButFileType = AllAttrTypes - AttrTypes{ AttrType.FileType };
  AllButModTime = AllAttrTypes - AttrTypes{ AttrType.ModTime };

  Changeable = AttrTypes{ AttrType.ModTime, AttrType.Owner, AttrType.Group,
    AttrType.Mode, AttrType.Flags };
  (* The attributes that we might be able to change. *)

  CheckoutIgnore = AllAttrTypes - AttrTypes{
    AttrType.FileType,
    AttrType.ModTime,
    AttrType.Size,
    AttrType.Mode };
  (* The attributes we don't want to save in the "checkouts" file when
     in checkout mode. *)

  AllSupport = SupportInfo{ AllAttrTypes, .. };

VAR (* CONST *)
  Bogus: T;			(* Not equal to anything else, even itself. *)

  Default: Attrs;		(* Default attributes for new files. *)

  Supported: SupportInfo;	(* What is supported by this end. *)
  Historical: SupportInfo;	(* What was supported in early versions. *)

PROCEDURE FromPathname(path: Pathname.T; follow: BOOLEAN): T
  RAISES {OSError.E};
(* Returns the attributes of the given pathname.  "follow" determines
   whether symbolic links should be followed. *)

PROCEDURE FromFile(file: File.T): T
  RAISES {OSError.E};
(* Returns the attributes of an open file. *)

PROCEDURE FromFD(fd: INTEGER): T
  RAISES {OSError.E};
(* Returns the attributes of an open POSIX file descriptor. *)

PROCEDURE FromStat(READONLY stat: Ustat.struct_stat): T;
(* Returns the attributes corresponding to the given status information. *)

PROCEDURE ForCheckout(rcsAttr: T; umask := -1): T;
(* Calculates the proper attributes for a checkout-mode file, based on
   the attributes of the RCS file and the umask.  If "umask" is
   defaulted, the process's umask is used. *)

PROCEDURE MakeNode(fa: T; path: Pathname.T)
  RAISES {OSError.E};

PROCEDURE HardLink(path, target: Pathname.T): T
  RAISES {OSError.E};
(* Creates a hard link named "path" which refers to the existing file
   "target".  Clears the file flags first, if necessary, so that the
   operation will succeed.  The flags are left in the cleared state.
   Returns a set of attributes which can be used to restore the flags
   to their original state. *)

PROCEDURE Install(fa: T; to: Pathname.T; from: Pathname.T := NIL): BOOLEAN
  RAISES {OSError.E};
(* Moves the file or node named "from" to the location specified by "to",
   and gives it the attributes specified by "fa".  "from" and "to" must
   reside in the same filesystem.  The exception is raised only if the
   file couldn't be moved into place.  It may be impossible to set some
   of the attributes to their specified values, but that does not cause
   an exception to be raised.

   If "from" is "NIL", then the attributes of "to" are changed in place.

   The return value indicates whether anything at all was done.  If the
   file is being handled in place, and its attributes were already OK,
   then "FALSE" is returned.  In all other cases, "TRUE" is returned. *)

PROCEDURE Delete(path: Pathname.T)
  RAISES {OSError.E};
(* Makes a valiant effort to remove the specified file or node, changing
   its attributes first if necessary.  The exception is raised if the
   file or node exists, but could not be removed. *)

PROCEDURE Decode(t: TEXT): T
  RAISES {TokScan.Error, UnknownGroup, UnknownOwner};
(* Converts a printable representation into file attributes. *)

(* The "Unknown" exceptions are raised the first time each unknown
   owner or group is encountered.  The exception argument is the
   text representation of the owner or group.  On subsequent calls,
   owners and groups already recorded as unknown are simply treated
   as being not present in the attributes.  Thus these exceptions
   can be used for logging warning messages about unknown owners
   and groups. *)

PROCEDURE Encode(fa: T;
                 READONLY support := AllSupport;
		 ignore := AttrTypes{}): TEXT;
(* Produces a printable representation of the given file attributes,
   possibly limited to selected attributes as specified in "support"
   and "ignore". *)

PROCEDURE IsSupported(fa: T; READONLY support: SupportInfo): BOOLEAN;
(* Determines whether the file whose attributes are given is supported,
   according to the information given in "support". *)

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Compares two sets of file attributes for equality.  Only the
   fields that are valid in both "a" and "b" are compared.  A set of
   attributes with unknown file type ("FileType.Unknown") is unequal
   to anything, including itself. *)

PROCEDURE Merge(fa, from: T): T;
(* Returns a new set of file attributes created by starting with "fa"
   and merging in any attributes that are present in "from" but not
   present in "fa". *)

PROCEDURE MergeDefault(fa: T): T;
(* Equivalent to "Merge(fa, Default[fa.fileType])". *)

PROCEDURE Override(fa, from: T; mask := AllAttrTypes): T;
(* Returns a new set of file attributes created by overriding selected
   attributes of "fa" with values from "from".  The overridden
   attributes are those that are both valid in "from" and present in
   "mask". *)

PROCEDURE Umask(fa: T; umask := -1): T;
(* Returns a new set of attributes copied from "fa", and modified as
   determined by "umask".  If it is defaulted, then the program's
   umask setting is used. *)

PROCEDURE MaskOut(fa: T; mask: AttrTypes): T;
(* Returns a new set of file attributes identical to "fa", except that
   any attributes specified in "mask" are eliminated. *)

PROCEDURE DecodeAttrTypes(t: TEXT): AttrTypes
  RAISES {TokScan.Error};
(* Converts a printable representation into "AttrTypes". *)

PROCEDURE EncodeAttrTypes(at: AttrTypes): TEXT;
(* Produces a printable representation of the given "AttrTypes". *)

PROCEDURE GetMask(fa: T): AttrTypes;
(* Returns the set of attribute types that are specified. *)

PROCEDURE GetModTime(fa: T): Time.T;
(* Returns the modification time from "fa".  It is a checked runtime
   error if the modification time is not valid. *)

PROCEDURE GetSize(fa: T): CARDINAL;
(* Returns the size from "fa".  It is a checked runtime error if the
   size is not valid. *)

PROCEDURE GetMode(fa: T): Word.T;
(* Returns the mode from "fa".  It is a checked runtime error if the
   mode is not valid. *)

PROCEDURE GetLinkTarget(fa: T): Pathname.T;
(* Returns the symbolic link target from "fa".  It is a checked
   runtime error if the link target is not valid. *)

PROCEDURE GetLinkCount(fa: T): CARDINAL;
(* Returns the hard link count from "fa".  It is a checked runtime
   error if the link count is not valid. *)

PROCEDURE SetSize(fa: T; size: CARDINAL);
(* Sets the size of "fa". *)

END FileAttr.
