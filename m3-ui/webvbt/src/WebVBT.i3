(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jan 17 14:20:25 PST 1996 by najork                   *)
(*      modified on Fri Jan  5 12:46:53 PST 1996 by mhb                      *)
<* PRAGMA LL *>

(* A "WebVBT" is a VBT class for displaying a page on the Web. *)

INTERFACE WebVBT;

IMPORT HTML, HTMLVBT, TextEditVBT, TextList, PixmapVBT, VBT, Web;

CONST 
  DefaultURL = "http://www.research.digital.com/SRC/home.html";

TYPE
  T <: Public;
  Private <: VBT.T;
  Public = Private OBJECT
             <* LL = VBT.mu *>
             url : TEXT := NIL;
             page: Page := NIL;
           METHODS
             <* LL <= VBT.mu *>
             init (): T;

             <* LL = VBT.mu *>
             fetch (url      : TEXT    := DefaultURL;
                    style    : Style   := Style.Normal;
                    zippers  : BOOLEAN := FALSE;
                    reload   : BOOLEAN := FALSE;
                    server   : Web.T   := NIL;
                    scrollBar: BOOLEAN := TRUE);
             fromText (contents      : TEXT; 
                       contentType   : Web.MIMEType := Web.MIMEType.Text;
                       contentSubType: TEXT         := "html";
                       url           : TEXT         := "text:"; 
                       style         : Style        := Style.Normal;
                       zippers       : BOOLEAN      := FALSE;
                       reload        : BOOLEAN      := FALSE;
                       server        : Web.T        := NIL;
                       scrollBar     : BOOLEAN      := TRUE);

             stop ();
             getLinks(): TextList.T;
             search(pattern: TEXT): BOOLEAN;

             (* the following are intended to be overridden by clients: *)
             ready (remImages: CARDINAL);

             hotlink (link: TEXT; READONLY cd: VBT.MouseRec);
             ismap   (link: TEXT; READONLY cd: VBT.MouseRec);
             isindex (typein: TEXT);
             form    ();
           END;

  Style =
    {Ugly (* text-only, fixed font *),
     NoImages (* multiple fonts, but no images *),
     Normal (* multiple fonts, images fetched before anything displayed *),
     Background                  (* images are fetched and displayed in the
                                    background *)
    };

(* The method "v.init()" initializes "v" as a "WebVBT.T" and displays
   nothing.

   The method "v.fetch(url, style, reload, server)" (eventually) displays
   the contents of "url".  More precisely, "v.fetch" cancels any fetching
   in progress and then forks a thread to actually retrieve the contents of
   "url", passing to "Web.Fetch" the "reload" and "server" arguments.
   After the contents of "url" are fetched (and, in the case of an HTML
   page, also parsed), the following events take place atomically locked by
   "VBT.mu": the "url" and "page" fields are set, the "VBT" displaying the
   "url" (i.e., "page.vbt") is updated, and the "v.ready(0)" method is
   invoked.

   Almost.  When "fetch" is called with "style=Background", the "ready"
   method is called before any images are fetched.  The parameter to
   "ready" is the number of images remaining to be fetched; the "ready"
   method will be called repeatedly, each time that an image is fetched.

   Not quite.  The "style" parameter given to the "fetch" method allows 4
   different styles of fetching: When "style=Ugly", a page is displayed
   with no graphics, using a single font.  This is pretty ugly, but it's
   probably fast.  For all other styles, the page is displayed using
   multiple fonts.  When "style=NoImages", no attempt is made to display
   images; the textual "alt" field is displayed, if one is given in the
   html.  When "style=Normal" (the default), images are fetched (using the
   "reload" and "server" arguments passed to "fetch") and displayed before
   the page is displayed.  Finally, when "style=Background", the page is
   displayed before all the images have been retrieved.  As images are
   retrieved, "v.ready(ct)" is called repeatedly, with "ct" set to the
   number of images left to retrieve.

   The method "v.stop()" is used to cancel any previous call to "v.fetch"
   that has not yet caused "v.ready(0)" to be called.

   The "ready" method is called after a URL fetched by a call to
   "v.fetch(...)" has been fetched and displayed.  As described above, when
   "fetch" is called with "style" equal to "Ugly", "NoImages", or "Normal",
   "v.ready(0)" is simply called.  However, if "fetch" is called with
   "style" equal to "Background", the "ready" method is called repeatedly,
   with a descreasing number.  The "ready" method is not called if the call
   to "fetch" was pre-empted by another call to "fetch" or the call was
   stopped by a call to "v.stop()".  The default "ready" method is a noop.


   The method "v.getLinks" returns a list of URLs contained in "v".  This
   list is "NIL" if "v" is not an HTML page or if fetching "v" is still
   pending.

   The method "v.search(pattern)" returns whether "pattern" appears in the
   text displayed by "v".  The search is case-insensitive.

   The method "v.hotlink(link, cd)" is called whenever the user has clicked
   on a link in an HTML page.  The default "hotlink" method is a noop. *)

TYPE

  Page = Web.Page OBJECT END;

  TextPage = Page OBJECT 
    vbt: TextEditVBT.T
  END;

  HTMLPage = Page OBJECT
     html: HTML.T;
     vbt: HTMLVBT.T;
  END;

  ImagePage = Page OBJECT
     vbt: PixmapVBT.T;
  END;

END WebVBT.

