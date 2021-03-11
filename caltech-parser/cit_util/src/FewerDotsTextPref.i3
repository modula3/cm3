INTERFACE FewerDotsTextPref;
FROM TextEquivalence IMPORT Preference;
VAR
  instance: Preference;
TYPE
  T <: Preference;

PROCEDURE Is(<*UNUSED*>self: T; thisBetter, thanThis: TEXT): BOOLEAN;

END FewerDotsTextPref. 
