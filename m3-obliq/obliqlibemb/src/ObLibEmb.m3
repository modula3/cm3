(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Thu Aug 10 09:32:23 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Sep 24 15:00:44 1997
 * Update Count    : 33
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.8  1997/10/22 14:21:49  bm
 * fixed typos in the help files.  Changed EmbProxiedObj obliq object to
 * not be protected.
 *
 * Revision 1.7  1997/07/11 17:37:22  bm
 * Potential release version
 *
 * Revision 1.6  1997/03/12 21:39:44  bm
 * Small bug
 *
 * Revision 1.5  1997/03/12 21:34:57  bm
 * Moved sharedobj from coterie
 *
 * Revision 1.4  1996/10/12 23:42:51  bm
 * fixed header
 *
 * Revision 1.3  1996/10/12 23:41:36  bm
 * silenced debug shit.
 *
 * 
 * HISTORY
 *)

MODULE ObLibEmb;

IMPORT ObEmbProxiedObj, ObLoader, ObEmbBundle, ObSharedObj, SynWr;

VAR setupDone := FALSE;

PROCEDURE PackageSetup (wr: SynWr.T): ObLoader.T =
  BEGIN
    SetupPackages();
    RETURN SetupModules(wr);
  END PackageSetup;

PROCEDURE SetupPackages () =
  BEGIN
    IF NOT setupDone THEN
      ObEmbProxiedObj.SetupPackage();
      ObSharedObj.SetupPackage();
      setupDone := TRUE;
    END;
  END SetupPackages;

PROCEDURE SetupModules (wr: SynWr.T): ObLoader.T =
  VAR 
    (* Use the Bundle as a fallback. *)
    loader: ObLoader.T := NEW(ObLoader.BundleT).init(wr, ObEmbBundle.Get());
  BEGIN
    (* Search the dirs first, in case we change the files during
       testing. *)
    loader := ObLoader.NewDefaultDir(wr, "obliqlibemb", NIL, loader);

    ObEmbProxiedObj.SetupModule(loader);
    ObSharedObj.SetupModule(loader);
    RETURN loader;
  END SetupModules;

BEGIN
END ObLibEmb.
