(*--------------------------------------------------------------------------*)
INTERFACE VCUtils;

IMPORT TextSeq;
IMPORT Tag, TagSeq, PkgVC;

(*--------------------------------------------------------------------------*)
PROCEDURE DetectPackageRootDir(VAR packageDir : TEXT; VAR pkgName : TEXT);
  (* post: packageDir, pkgName defined *)

(*--------------------------------------------------------------------------*)
PROCEDURE ChangeToPackageRoot(packageDir : TEXT) RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE CommitTypeFromText(t : TEXT) : PkgVC.CommitType RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDir(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T)
  RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutPackage(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T) 
  RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutCollection(pkgVC : PkgVC.T; name : TEXT; tag : Tag.T)
  RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE CheckoutDirect(pkgVC     : PkgVC.T; 
                         prjRoot, 
                         collection, 
                         rev       : TEXT;
                         packages  : TextSeq.T) RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE FindTagnameInList(tagname : TEXT; tags : TagSeq.T;) : Tag.T;

(*--------------------------------------------------------------------------*)
PROCEDURE FindTagInList(tag : Tag.T; tags : TagSeq.T;) : Tag.T;

(*--------------------------------------------------------------------------*)
PROCEDURE TagExists(pkgVC : PkgVC.T; t : Tag.T) : BOOLEAN RAISES {PkgVC.E};

(*--------------------------------------------------------------------------*)
PROCEDURE NewTagFromPattern(pkgVC : PkgVC.T; pat: TEXT) : Tag.T
  RAISES {PkgVC.E};

END VCUtils.
