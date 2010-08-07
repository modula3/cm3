(*--------------------------------------------------------------------------*)
INTERFACE Release;

(*--------------------------------------------------------------------------*)
PROCEDURE ComPactName() : TEXT;
  (* This function returns an unchangable compiled-in version string,
     which is an indication of the release branch this ComPact version
     comes from. Currently there will only be three possible values:
     "Elego ComPact Plus" (full commercial version), "Elego ComPact
     Light" (commercial `light' version), and "Elego ComPact Free"
     (full non-commercial version). This string will also be used in
     the Show() procedure. *)

(*--------------------------------------------------------------------------*)
PROCEDURE VersionToString(): TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE LicenseNumberToString(): TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE KeyCheck(pass: TEXT): BOOLEAN;

(*--------------------------------------------------------------------------*)
PROCEDURE ComPactVersionText(): TEXT;
  (* complete version text: ComPact name, version, license number *)

(*--------------------------------------------------------------------------*)
PROCEDURE Show();

END Release.
