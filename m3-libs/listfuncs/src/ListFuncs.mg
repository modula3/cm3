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
 * Created On      : Wed Jul 19 10:55:27 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Sep  5 19:18:23 1997
 * Update Count    : 14
 * 
 * $Source: /opt/cvs/cm3/m3-libs/listfuncs/src/ListFuncs.mg,v $
 * $Date: 2001-12-01 14:42:18 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1997/10/22 14:20:59  bm
 * added Count function
 *
 * Revision 1.2  1997/06/29 18:27:58  bm
 * Fixed header
 *
 * 
 * HISTORY
 *)

GENERIC MODULE ListFuncs(Elem, List);

PROCEDURE DeleteD(VAR listHead: T; READONLY elem: Elem.T): T =
  VAR list := listHead;
      prev := list;
  BEGIN
    IF list = NIL THEN
      RETURN NIL;
    END;
    IF Elem.Equal(list.head, elem) THEN
      listHead := list.tail;
      list.tail := NIL;
    ELSE
      list := list.tail;
      WHILE list # NIL AND NOT Elem.Equal(list.head, elem) DO
        prev := list;
        list := list.tail;
      END;
      IF list # NIL THEN
        prev.tail := list.tail;
        list.tail := NIL;
      END;
    END;
    RETURN list;
  END DeleteD;

PROCEDURE DeleteAllD(VAR listHead: T; READONLY elem: Elem.T): T =
  VAR temp: List.T;
      list: List.T := listHead;
      ret : List.T := NIL;
  BEGIN
    (* Pull out the ones at the beginning of the list. *)
    WHILE list # NIL AND Elem.Equal(list.head, elem) DO
      temp := ret;
      ret := list;
      list := list.tail;
      ret.tail := temp;
    END;
    listHead := list;
    IF list = NIL THEN
      RETURN ret;
    END;
    
    WHILE list.tail # NIL DO
      IF Elem.Equal(list.tail.head, elem) THEN
        (* move the element to the return list. *)
        temp := ret;
        ret := list.tail;
        list.tail := list.tail.tail;
        ret.tail := temp;
      ELSE
        (* just advance to the next one. *)
        list := list.tail;
      END;
    END;
    RETURN ret;
  END DeleteAllD;

PROCEDURE Count(l: T; READONLY e: Elem.T): INTEGER =
  VAR c: INTEGER := 0;
  BEGIN
    WHILE l # NIL DO
      IF Elem.Equal(l.head, e) THEN INC(c) END;
      l := l.tail;
    END;
    RETURN c;
  END Count; 

VAR cache: T := NIL;

PROCEDURE New(READONLY elem: Elem.T): T =
  VAR new: T;
  BEGIN
    IF cache # NIL THEN
      new := cache;
      cache := cache.tail;
    ELSE
      new := NEW(T);
    END;
    new.head := elem;
    RETURN new;
  END New;

PROCEDURE Free(list: T): T =
  VAR ret := list.tail;
  BEGIN
    list.tail := cache;
    cache := list;
    RETURN ret;
  END Free;

BEGIN
END ListFuncs.
