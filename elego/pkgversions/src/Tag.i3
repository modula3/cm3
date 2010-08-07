(*---------------------------------------------------------------------------*)
INTERFACE Tag;

IMPORT Version, TextSeq;

CONST Brand = "Tag Module version 0.1";

(*---------------------------------------------------------------------------*)
TYPE 
  Kind = {Undefined, Merge, Devel, Alpha, Beta, Gamma, Change, Feature, Fix, 
          Release, Latest};
  Attribute = {MergeMarker, BranchStart, Branch, Stable};
  Attributes = SET OF Attribute;

  T = BRANDED "Tag Object Type version 0.1" OBJECT
    t_seq      : TextSeq.T;
    t_kind     : Kind;
    t_pkgname  : TEXT; (* name of the package the tag belongs to *)
    t_version  : Version.T; (* contains major, minor, patchlevel *)
    t_mversion : Version.T; (* merged into version *)
    t_attr     : Attributes;
    t_orig     : TEXT; (* original text form if initialized from text *)
    t_change   : TEXT; (* name of a change/feature/fix tag *)
    t_location : TEXT; (* name of (DCVS) location suffix *)
  METHODS
    init() : T := Init;
    initFromTag(tag : T) : T := InitFromTag;
    initFromText(tag : TEXT) : T := InitFromText;
    initFromElements(k : Kind; pkgname : TEXT; major, minor, pl : CARDINAL;
                     stable := FALSE; change : TEXT := NIL;
                     branch := FALSE; suffix : TEXT := NIL;
                     start := FALSE) : T := InitFromElements;
    originalText() : TEXT := OriginalText;
    denotation(checkIt := TRUE) : TEXT := Denotation;
    base(level := 5) : TEXT := Base;
    okay() : BOOLEAN := Okay;
    predefined() : BOOLEAN := Predefined;
    isStableBranchTag() : BOOLEAN := IsStableBranchTag;
    isChangeBranchTag() : BOOLEAN := IsChangeBranchTag;
    isChangeFeatureOrFixTag() : BOOLEAN := IsChangeFeatureOrFixTag;
    kind() : Kind := GetKind;
    kindAsText() : TEXT := GetKindAsText;
    pkgName() : TEXT := PackageName;
    getChangeName() : TEXT := GetChangeName;
    getLocationSuffix() : TEXT := GetLocationSuffix;
    mergeDestVersion() : Version.T := MergeDestVersion;
    version() : Version.T := GetVersion;
    versionAsText() : TEXT := VersionAsText;
    setNextMajorVersion() := SetNextMajorVersion;
    setNextMinorVersion() := SetNextMinorVersion;
    setNextPatchLevel() := SetNextPatchLevel;
    equal(tag : T) : BOOLEAN := Equal;
    compare(tag : T) : [-1..1] := Compare;
    debugInfo() : TEXT := DebugInfo;
  END;

(*---------------------------------------------------------------------------*)
VAR
  Head : T; (* CONST *)
  (* predefined constant for head of trunk revisisons *)
  Tip  : T; (* CONST *)
  (* predefined constant for head of branch revisisons *)
  LastTip : T;
  (* tag of the latest release branch in a package *)

(*---------------------------------------------------------------------------*)
PROCEDURE CheckedName(t : TEXT) : TEXT;
  (* Return a tag component with all suspicious characters replaced by `='. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Decompose(tag : TEXT) : TextSeq.T;
  (* Decompose the tag into its elements (separated by `_').
     Translate any `=' characters found into `_'. 
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Compose(seq : TextSeq.T) : TEXT;
  (* Build a tag from a sequence of elements (add `_' characters). 
     Translate any `_' characters inside the elements into `='. 
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE KindFromText(t : TEXT) : Kind;
  (* Convert a TEXT to a Kind. *)

(*---------------------------------------------------------------------------*)
PROCEDURE KindToText(k : Kind) : TEXT;
  (* Convert a Kind to a TEXT. *)

(*---------------------------------------------------------------------------*)
PROCEDURE New(tag : TEXT) : T;
  (* Construct a new tag from its textual representation. *)

(*---------------------------------------------------------------------------*)
PROCEDURE NewCopy(tag : T) : T;
  (* Construct a new tag from an existing tag *)

(*---------------------------------------------------------------------------*)
PROCEDURE Construct(k : Kind; pkgname : TEXT; 
                    major, minor, pl : CARDINAL;
                    stable := FALSE; change : TEXT := NIL;
                    branch := FALSE; suffix : TEXT := NIL;
                    start := FALSE) : T;
  (* Construct a new tag from the given elements. *)

(*---------------------------------------------------------------------------*)
PROCEDURE NewStableBranch(tag : T) : T;
  (* Return a new stable branch tag if `tag' is a release tag;
     if not, the result is undefined. *)
     
(*---------------------------------------------------------------------------*)
PROCEDURE NewBranchStartTag(tag : T) : T;
  (* Return a new branch start tag for a given tag from branch `tag'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE NewBranch(tag : T; kind : Kind := Kind.Undefined) : T;
  (* Return a new change branch tag if `tag' is a release tag;
     if not, the result is undefined. *)
     
(*---------------------------------------------------------------------------*)
PROCEDURE NewBranchStart(tag : T; kind : Kind := Kind.Undefined) : T;
  (* Return a new change branch start tag for a given tag from branch `tag'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE NewMergeTag(tag : T; mversion : Version.T := NIL) : T;
  (* Return a new merge tag for a given release branch `tag'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T) : T;
  (* Set everything to `undefined'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE InitFromTag(self : T; tag : T) : T;
  (* Assign `self' from `tag'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE InitFromText(self : T; tag : TEXT) : T;
  (* Convert initial values from TEXT representation. *)

(*---------------------------------------------------------------------------*)
PROCEDURE InitFromElements(self : T; k : Kind; pkgname : TEXT; 
                           major, minor, pl : CARDINAL;
                           stable := FALSE; change : TEXT := NIL;
                           branch := FALSE; suffix : TEXT := NIL;
                           start := FALSE) : T;
  (* Construc a tag from the given elements. *)

(*---------------------------------------------------------------------------*)
PROCEDURE OriginalText(self : T) : TEXT;
  (* Return the original text the tag was constructed from, if defined,
     else "". *)

(*---------------------------------------------------------------------------*)
PROCEDURE Denotation(self : T; checkIt := TRUE) : TEXT;
  (* Return the textual representation of a tag. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Base(self : T; level := 5) : TEXT;
  (* Return the base of the denotation of a tag, containing `level' elements *)

(*---------------------------------------------------------------------------*)
PROCEDURE Okay(self : T) : BOOLEAN;
  (* <=> Tag `self' is syntactically correct and all elements defined. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Predefined(self : T) : BOOLEAN;
  (* <=> Tag `self' is a predefined tag, viz. Head, Tip or LastTip. *)

(*---------------------------------------------------------------------------*)
PROCEDURE GetKind(self : T) : Kind;
  (* Return the kind of a tag. *)

(*---------------------------------------------------------------------------*)
PROCEDURE GetKindAsText(self : T) : TEXT;
  (* Return the textual representation of the kind of a tag. *)

(*---------------------------------------------------------------------------*)
PROCEDURE IsStableBranchTag(self : T) : BOOLEAN;
  (* <=> Tag `self' is of the form <ident>_<ident>_<nat>_<nat>_stable. *)

(*---------------------------------------------------------------------------*)
PROCEDURE IsChangeBranchTag(self : T) : BOOLEAN;
  (* <=> Tag `self' is of the form
     [change|feature|fix]_<ident>_<ident>_<nat>_<nat>_head. *)

(*---------------------------------------------------------------------------*)
PROCEDURE IsChangeFeatureOrFixTag(self : T) : BOOLEAN;
  (* <=> Tag `self' is of the form
     [change|feature|fix]_.*. *)

(*---------------------------------------------------------------------------*)
PROCEDURE PackageName(self : T) : TEXT;
  (* Return the package name of tag `self'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE GetChangeName(self : T) : TEXT;
  (* Return the name of change/fix/feature tag `self'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE GetLocationSuffix(self : T) : TEXT;
  (* Return the location suffix of tag `self'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE MergeDestVersion(self : T) : Version.T;
  (* Return the version of the merge destination if tag `self' is a
     merge tag. *)

(*---------------------------------------------------------------------------*)
PROCEDURE GetVersion(self : T) : Version.T;
  (* Return the version of tag `self'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE VersionAsText(self : T) : TEXT;
  (* Return the textual representation of the version of tag `self'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextMajorVersion(self : T);
  (* Increment the tag version to the next major version. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextMinorVersion(self : T);
  (* Increment the tag version to the next minor version. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SetNextPatchLevel(self : T);
  (* Increment the tag version to the next patch level. *)

(*---------------------------------------------------------------------------*)
PROCEDURE LatestTag(list : TextSeq.T; k : Kind) : T;
  (* Return the tag with the highest version of kind `k' from `list'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE ContainedInList(list : TextSeq.T; tag : T) : BOOLEAN;
  (* <=> `tag' is contained in `list' *)

(*---------------------------------------------------------------------------*)
PROCEDURE IsKind(tag : TEXT; k : Kind) : BOOLEAN;

(*---------------------------------------------------------------------------*)
PROCEDURE Equal(self, tag : T) : BOOLEAN;

(*---------------------------------------------------------------------------*)
PROCEDURE Compare(self, tag : T) : [-1..1];
  (* compare respecting the logical structure of the tags *)

(*---------------------------------------------------------------------------*)
PROCEDURE CompareFromText(tag1, tag2 : TEXT) : [-1..1];
  (* as Compare but with additional InitFromText *)

(*---------------------------------------------------------------------------*)
PROCEDURE DebugInfo(self : T) : TEXT;
  (* return information about tag contents *)

END Tag.

