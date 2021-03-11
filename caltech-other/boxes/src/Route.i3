(* $Id$ *)

INTERFACE Route;
IMPORT GridPointSet, GridPointList;
IMPORT ForbiddenSteps;
IMPORT RouteID;
IMPORT GridPointCube;

TYPE 
  T = OBJECT METHODS
    gridPointList() : GridPointList.T;

    (* attach some outside attribute to the route;
       it is a checked runtime error to add more than one 
       attribute of a given type (name) 
     *)
    addAttribute(attributeName : TEXT; attr : REFANY);
    
    (* get a named attribute; it is a checked runtime error to 
       attempt to get a nonexistent attribute 
     *)
    getAttribute(attributeName : TEXT) : REFANY;

    (* delete a named attribute; it is a checked runtime error to
       attempt to delete a nonexistent attribute 
     *)
    deleteAttribute(attributeName : TEXT);

    (* when we commit the route, we have the option of committing a 
       SMALLER route than we originally intended ... *)
    commitFinishedRoute(actualRouteCommitted : GridPointList.T);

    (* the the cost of a finished route *)
    getCost() : CARDINAL;

    ripUpStuff() : GridPointSet.T;

    (* override the following to provide feedback for ripper-up;
       method should return cost of ripping up a routed tagged by id "for" *)
    (* the cost should be along the lines of the total number of gridpoint
       units spanned by the MST (or something like that) *)
    ripUpCost(for : RouteID.T) : LONGREAL;

    (* override the following to provide a RouteID for the current route *)
    myID() : RouteID.T;
  END;

  Default <: Public;

  Public = T OBJECT METHODS
    (* FIRST compute the route 
       THEN do rip-ups that are requested by that route
       FINALLY commit it with commitFinishedRoute *)
    computeRouteBetweenSets(
              (* route between from and to *)
              fromSet, toSet : GridPointSet.T;  

              (* treating the following as non-obstacles
                 (should include reachable layout in from and to) *)
              nonObstacles : GridPointSet.T;    

              forbiddenSteps : ForbiddenSteps.T := NIL;
              multiplier : REAL;

              READONLY BBox : GridPointCube.T;
              (* bbox w/in which to route *)

              respectBBox : BOOLEAN;
              (* stay inside bbox on THIS route *)
              
              keepOuts : GridPointSet.T := NIL;
              (* the route will stay out of the keepOuts, guaranteed 
                 keepOuts must not overlap fromSet, toSet, or nonObstacles

                 keepOuts need ONLY implement the member() method.

                 if keepOuts is NIL, then it will be treated as the empty
                 set
              *)


              pointsThatCostZero : GridPointSet.T := NIL              
    (* sometimes you want to treat existing points in a net
       (not part of routing sets) as free to step through! *)
    ) RAISES { NotFound };



  END;

CONST Brand = "Gridpoint Router Route";

PROCEDURE SetRipUpFilename(fn : TEXT);

EXCEPTION NotFound((* reason : *) TEXT);
END Route.


