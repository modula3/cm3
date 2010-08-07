(*--------------------------------------------------------------------------*)
INTERFACE Version;

IMPORT Word;

CONST Brand = "Package Versions Module 1.0";

(*--------------------------------------------------------------------------*)
(*
  Versions are objects that contain three main elements: a major number,
  a minor number, and a patch level. Versions have a textual representation
  and a pathname representation. The textual representation is usually
|  
|   major.minor.patchlevel      example:  2.6.1
|
  the pathname represenation is
|
|   major_minor_patchlevel      example:  2_6_1
|
  If there are no further qualifications of a version, it is considered
  to be a `release' version. Other kinds of version may be `alpha', 
  `beta', `gamma', and `devel'. The representation of these kinds adds
  a leading character to the version string, for example
|
|  a4.4.2 = alpha 4.4.2, d3.1.4 = development version 3.1.4
|
  Version ranges consist of two version, the lower bound and the upper
  bound of the range. A version range is denoted by
|
|   version1-version2           example: 2.6.1-2.7.3
|
  Undefined elements of versions or version ranges may be denoted
  by the character `x', for example 2.5.x.
|
  A version is considered to be `defined', if at least the major number
  of the version is defined. A version is said to be `exactDefined', if
  all elements are defined. An undefined version element is denoted by
  the constant `Undefined', which is represented by LAST(CARDINAL).
|
  Versions can be compared for equality, they are completely ordered by
  the relation `less(v1, v2)'. The ordering concerning the numerical
  values is obvious (at least I think so), for versions of different
  kind hold the following rules:
|
|  less(alpha-version, beta-version)
|  less(beta-version, gamma-version)
|  less(gamma-version, release-version)
|
  Of course the relation `less' is transitive.
|
  Extension from Thu May 25 10:47:50 MET DST 2000:

  A new kind of version has been introduced, called `latest' and
  denoted by an `l' in the version number. Usually, all latest
  versions are less than everything else. Several methods of this
  interface contain a parameter `latestOverride' though, which
  defaults to FALSE. If this is set to TRUE, any latest version will
  be greater than any other version, and latest versions will be
  included in all version ranges. Thus they will always be chosen in
  preference to other kinds of versions, be it development, release, or
  anything else.

*)

(*--------------------------------------------------------------------------*)
CONST Undefined = LAST(CARDINAL);

(*--------------------------------------------------------------------------*)
TYPE
  T = OBJECT
    major, minor, patchlevel : CARDINAL;
    kind : CHAR; (* a = alpha, b = beta, g = gamma, r = release,
                    d = devel, l = latest *)
    valid : BOOLEAN;
  METHODS
    init() : T := Init;
    isValid() : BOOLEAN := IsValid;
    fromText(t : TEXT) : T := FromText;
    toText() : TEXT := ToText;
    fromDir(t : TEXT) : T := FromDir;
    toDir() : TEXT := ToDir;
    fromDirOrText(t : TEXT) : T := FromDirOrText;
    equal(v : T) : BOOLEAN := Equal;
    less(v : T; latestOverride := FALSE) : BOOLEAN := Less;
    le(v : T; latestOverride := FALSE) : BOOLEAN := LessOrEqual;
    defined() : BOOLEAN := Defined;
    exactDefined() : BOOLEAN := ExactDefined;
    isDevel() : BOOLEAN := IsDevel;
    isRelease() : BOOLEAN := IsRelease;
    isAlpha() : BOOLEAN := IsAlpha;
    isBeta() : BOOLEAN := IsBeta;
    isGamma() : BOOLEAN := IsGamma;
    isLatest() : BOOLEAN := IsLatest;
    compatible(v : T; latestOverride := FALSE) : BOOLEAN := Compatible;
    nextMajor() := NextMajor;
    nextMinor() := NextMinor;
    nextPatchLevel() := NextPatchLevel;
  END;

  Range = OBJECT
    from, to : T;
  METHODS
    init() : Range := RangeInit;
    contains(v : T; latestOverride := FALSE) : BOOLEAN := Contains;
    fromText(t : TEXT) := RangeFromText;
    toText() : TEXT := RangeToText;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE IsValid(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE Defined(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE ExactDefined(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE FromText(self : T; t : TEXT) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE FromDir(self : T; t : TEXT) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE ToDir(self : T) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE FromDirOrText(self : T; t : TEXT) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE Equal(self : T; v : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsDevel(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsRelease(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsAlpha(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsBeta(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsGamma(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE IsLatest(self : T) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE Compatible(self : T; v : T; latestOverride := FALSE) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE Less(self : T; v : T; latestOverride := FALSE) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE LessOrEqual(self : T; v : T; latestOverride := FALSE) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE NextMajor(self : T);

(*--------------------------------------------------------------------------*)
PROCEDURE NextMinor(self : T);

(*--------------------------------------------------------------------------*)
PROCEDURE NextPatchLevel(self : T);

(*--------------------------------------------------------------------------*)
PROCEDURE RangeInit(self : Range) : Range;

(*--------------------------------------------------------------------------*)
PROCEDURE Contains(self : Range; v : T; latestOverride := FALSE) : BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE RangeFromText(self : Range; t : TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE RangeToText(self : Range) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE Hash(self : T) : Word.T;

END Version.
