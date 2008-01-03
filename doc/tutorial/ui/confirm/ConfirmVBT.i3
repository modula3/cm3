INTERFACE ConfirmVBT;

IMPORT Trestle, VBT, HVSplit, ButtonVBT;

TYPE
  T = HVSplit.T OBJECT
      METHODS
        init (msg: TEXT; yaction := DeleteDialog; caction := DeleteDialog):
              T := Init
      END;

PROCEDURE DeleteDialog (v: ButtonVBT.T; READONLY cd: VBT.MouseRec);

PROCEDURE Init (self   : T;
                msg    : TEXT;
                yaction         := DeleteDialog;
                caction         := DeleteDialog  ): T;

(* New(...) is equivalent to NEW(T).init(...) *)
PROCEDURE New (msg: TEXT; yaction := DeleteDialog; caction := DeleteDialog):
  T;

END ConfirmVBT.
