/* plplotcapi.i contains the common API (API used by the many computer
 * language interfaces for the PLplot library) of PLplot in a form that
 * is readily understood by swig.
 */
/* 
Copyright 2002 Gary Bishop
Copyright 2002 Alan W. Irwin
This file is part of PLplot.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; version 2 of the License.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public License
along with the file; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/* For passing user data, as with X's XtPointer */

typedef void* PLPointer;

/*--------------------------------------------------------------------------*\
 * Complex data types and other good stuff
\*--------------------------------------------------------------------------*/

/* Switches for escape function call. */
/* Some of these are obsolete but are retained in order to process
   old metafiles */

#define PLESC_SET_RGB		1	/* obsolete */
#define PLESC_ALLOC_NCOL	2	/* obsolete */
#define PLESC_SET_LPB		3	/* obsolete */
#define PLESC_EXPOSE		4	/* handle window expose */
#define PLESC_RESIZE		5	/* handle window resize */
#define PLESC_REDRAW		6	/* handle window redraw */
#define PLESC_TEXT		7	/* switch to text screen */
#define PLESC_GRAPH		8	/* switch to graphics screen */
#define PLESC_FILL		9	/* fill polygon */
#define PLESC_DI		10	/* handle DI command */
#define PLESC_FLUSH		11	/* flush output */
#define PLESC_EH		12      /* handle Window events */
#define PLESC_GETC		13	/* get cursor position */
#define PLESC_SWIN		14	/* set window parameters */
#define PLESC_PLFLTBUFFERING	15	/* configure PLFLT buffering */
#define PLESC_XORMOD		16	/* set xor mode */
#define PLESC_SET_COMPRESSION	17	/* AFR: set compression */
#define PLESC_CLEAR		18      /* RL: clear graphics region */
#define PLESC_DASH		19	/* RL: draw dashed line */
#define PLESC_HAS_TEXT		20	/* driver draws text */
#define PLESC_IMAGE		21	/* handle image */
#define PLESC_IMAGEOPS          22      /* plimage related operations */

/* image operations */
#if 0
#define ZEROW2B   1
#define ZEROW2D   2
#define ONEW2B    3
#define ONEW2D    4
#endif

/* definitions for the opt argument in plsurf3d() */

#define DRAW_LINEX 0x01 /* draw lines parallel to the X axis */
#define DRAW_LINEY 0x02 /* draw lines parallel to the Y axis */
#define DRAW_LINEXY 0x03 /* draw lines parallel to both the X and Y axes */
#define MAG_COLOR 0x04 /* draw the mesh with a color dependent of the magnitude */
#define BASE_CONT 0x08 /* draw contour plot at bottom xy plane */
#define TOP_CONT 0x10 /* draw contour plot at top xy plane */
#define SURF_CONT 0x20 /* draw contour plot at surface */
#define DRAW_SIDES 0x40 /* draw sides */
#define FACETED   0x80 /* draw outline for each square that makes up the surface */
#define MESH 0x100 /* draw mesh */

/* Window parameter tags */

#define PLSWIN_DEVICE		1	/* device coordinates */
#define PLSWIN_WORLD		2	/* world coordinates */

/* PLplot Option table & support constants */

/* Option-specific settings */

#define PL_OPT_ENABLED		0x0001	/* Obsolete */
#define PL_OPT_ARG		0x0002	/* Option has an argment */
#define PL_OPT_NODELETE		0x0004	/* Don't delete after processing */
#define PL_OPT_INVISIBLE	0x0008	/* Make invisible */
#define PL_OPT_DISABLED		0x0010	/* Processing is disabled */

/* Option-processing settings -- mutually exclusive */

#define PL_OPT_FUNC		0x0100	/* Call handler function */
#define PL_OPT_BOOL		0x0200	/* Set *var = 1 */
#define PL_OPT_INT		0x0400	/* Set *var = atoi(optarg) */
#define PL_OPT_FLOAT		0x0800	/* Set *var = atof(optarg) */
#define PL_OPT_STRING		0x1000	/* Set var = optarg */

/* Global mode settings */
/* These override per-option settings */

#define PL_PARSE_PARTIAL	0x0000	/* For backward compatibility */
#define PL_PARSE_FULL		0x0001	/* Process fully & exit if error */
#define PL_PARSE_QUIET		0x0002	/* Don't issue messages */
#define PL_PARSE_NODELETE	0x0004	/* Don't delete options after */
					/* processing */
#define PL_PARSE_SHOWALL	0x0008	/* Show invisible options */
#define PL_PARSE_OVERRIDE	0x0010	/* Obsolete */
#define PL_PARSE_NOPROGRAM	0x0020	/* Program name NOT in *argv[0].. */
#define PL_PARSE_NODASH		0x0040	/* Set if leading dash NOT required */
#define PL_PARSE_SKIP		0x0080	/* Skip over unrecognized args */

#define PL_MAXKEY 16

/* Structure for describing the plot window */

#define PL_MAXWINDOWS	64	/* Max number of windows/page tracked */

/* Macro used (in some cases) to ignore value of argument */
/* I don't plan on changing the value so you can hard-code it */

#define PL_NOTSET (-42)


#define PLESPLFLTBUFFERING_ENABLE     1
#define PLESPLFLTBUFFERING_DISABLE    2
#define PLESPLFLTBUFFERING_QUERY      3

/* Non-common API that are included here because they traditionally
 * were part of plmodule.c. */

DOC(plarrows, "Plot an arrow.")
void
plarrows(PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk, PLFLT *ArrayCk, PLINT n,
         PLFLT scale, PLFLT dx, PLFLT dy) ;

DOC(plsxwin, "Set inferior X window.")
void
plsxwin(PLINT window_id);

/* Complete list of common API (has "c_" suffix version defined in plplot.h) */

DOC(pl_setcontlabelformat, "Set the format of the contour labels.")
void
pl_setcontlabelformat(PLINT lexp, PLINT sigdig);

DOC(pl_setcontlabelparam, "Set offset and spacing of contour labels.")
void
pl_setcontlabelparam(PLFLT offset, PLFLT size, PLFLT spacing, PLINT active);

DOC(pladv, "Advance to subpage \"page\", or to the next one if \"page\" = 0.")
void
pladv(PLINT page);

DOC(plaxes,"This functions similarly to plbox() except that the origin of the axes is placed at the user-specified point (x0, y0).")
void
plaxes(PLFLT x0, PLFLT y0, const char *xopt, PLFLT xtick, PLINT nxsub,
	 const char *yopt, PLFLT ytick, PLINT nysub);

DOC(plbin,"Plot a histogram using x to store data values and y to store frequencies.")
void
plbin(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLINT center);

DOC(plbop, "Start new page.  Should only be used with pleop().")
void
plbop(void);

DOC(plbox, "Draw a box around the current viewport.")
void
plbox(const char *xopt, PLFLT xtick, PLINT nxsub,
	const char *yopt, PLFLT ytick, PLINT nysub);

DOC(plbox3, "This is the 3-d analogue of plbox().")
void
plbox3(const char *xopt, const char *xlabel, PLFLT xtick, PLINT nsubx,
	 const char *yopt, const char *ylabel, PLFLT ytick, PLINT nsuby,
	 const char *zopt, const char *zlabel, PLFLT ztick, PLINT nsubz);

DOC(plcalc_world, "Calculate world coordinates and subpage from relative device coordinates.")
void
plcalc_world(PLFLT rx, PLFLT ry, PLFLT *OUTPUT, PLFLT *OUTPUT, PLINT *OUTPUT);

DOC(plclear, "Clear current subpage.")
void
plclear(void);

DOC(plcol0, "Set color, map 0.  Argument is integer between 0 and 15.")
void
plcol0(PLINT icol0);

DOC(plcol1, "Set color, map 1.  Argument is a float between 0. and 1.")
void
plcol1(PLFLT col1);

DOC(plcont, "Draw a contour plot.")
void
plcont(PLFLT **Matrix, PLINT nx, PLINT ny, PLINT kx, PLINT lx,
	 PLINT ky, PLINT ly, PLFLT *Array, PLINT n,
	 pltr_func pltr,
	 PLPointer PYOBJECT_DATA);

DOC(plcpstrm, "Copy state parameters from the reference stream to the current stream.")
void
plcpstrm(PLINT iplsr, PLINT flags);

DOC(plend, "End a plotting session for all open streams.")
void
plend(void);

DOC(plend1, "End a plotting session for the current stream only.")
void
plend1(void);

DOC(plenv, "Simple interface for defining viewport and window.")
void
plenv(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
	PLINT just, PLINT axis);

DOC(pleop, "End current page.  Should only be used with plbop().")
void
pleop(void);

DOC(plerrx, "Plot horizontal error bars (xmin(i),y(i)) to (xmax(i),y(i)).")
void
plerrx(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk);

DOC(plerry, "Plot vertical error bars (x,ymin(i)) to (x(i),ymax(i)).")
void
plerry(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk);

DOC(plfamadv, "Advance to the next family file on the next new page.")
void
plfamadv(void);

DOC(plfill, "Pattern fills the polygon bounded by the input points.")
void
plfill(PLINT n, PLFLT *Array, PLFLT *ArrayCk);

DOC(plfill3, "Pattern fills the 3d polygon bounded by the input points.")
void
plfill3(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk);

DOC(plflush, "Flush the output stream.  Use sparingly, if at all.")
void
plflush(void);

DOC(plfont, "Set the global font flag to 'ifont'.")
void
plfont(PLINT ifont);

DOC(plfontld, "Load specified font set.")
void
plfontld(PLINT fnt);

DOC(plgchr, "Get character default height and current (scaled) height.")
void 
plgchr(PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgcol0, "Get 8 bit RGB values for given color from color map 0.")
void
plgcol0(PLINT icol0, PLINT *OUTPUT, PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgcolbg, "Get 8-bit RGB background color.")
void
plgcolbg(PLINT *OUTPUT, PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgcompression, "Get the current compression setting.")
void
plgcompression(PLINT *OUTPUT);

DOC(plgdev, "Get the current device (keyword) name.")
void
plgdev(char *OUTPUT);

DOC(plgdidev, "Retrieve current window into device space.")
void
plgdidev(PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgdiori, "Get plot orientation.")
void
plgdiori(PLFLT *OUTPUT);

DOC(plgdiplt, "Retrieve current window into plot space.")
void
plgdiplt(PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgfam, "Get family file parameters.")
void
plgfam(PLINT *OUTPUT, PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgfnam, "Get the (current) output file name.")
void
plgfnam(char *OUTPUT);

DOC(plglevel, "Get the (current) run level.")
void
plglevel(PLINT *OUTPUT);

DOC(plgpage, "Get output device parameters.")
void
plgpage(PLFLT *OUTPUT, PLFLT *OUTPUT,
	  PLINT *OUTPUT, PLINT *OUTPUT, PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgra, "Switch to graphics screen.")
void
plgra(void);

DOC(plgspa, "Get subpage boundaries in absolute coordinates.")
void
plgspa(PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgstrm, "Get current stream number.")
void
plgstrm(PLINT *OUTPUT);

DOC(plgver, "Get current library version number.")
void
plgver(char *OUTPUT);

DOC(plgvpd, "Get viewport boundaries in normalized device coordinates.")
void
plgvpd(PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgvpw, "Get viewport boundaries in world coordinates.")
void
plgvpw(PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plgxax, "Get x axis labeling parameters.")
void
plgxax(PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgyax, "Get y axis labeling parameters.")
void
plgyax(PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plgzax, "Get z axis labeling parameters.")
void
plgzax(PLINT *OUTPUT, PLINT *OUTPUT);

DOC(plhist, "Draw histogram.")
void
plhist(PLINT n, PLFLT *Array, PLFLT datmin, PLFLT datmax,
	 PLINT nbin, PLINT oldwin);

DOC(plhls, "Set current color (map 0) by hue, lightness, and saturation.")
void
plhls(PLFLT h, PLFLT l, PLFLT s);

DOC(plinit, "Initialize PLplot, using preset or default options.")
void
plinit(void);

DOC(pljoin, "Draw a line segment from (x1, y1) to (x2, y2).")
void
pljoin(PLFLT x1, PLFLT y1, PLFLT x2, PLFLT y2);

DOC(pllab, "Label graphs.")
void
pllab(const char *xlabel, const char *ylabel, const char *tlabel);

DOC(pllightsource, "Set position of the light source.")
void
pllightsource(PLFLT x, PLFLT y, PLFLT z);

DOC(plline, "Draw line segments connecting a series of points.")
void
plline(PLINT n, PLFLT *Array, PLFLT *ArrayCk);

DOC(plline3, "Draw a line in 3 space.")
void
plline3(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk);

DOC(pllsty, "Set line style.")
void
pllsty(PLINT lin);

DOC(plmesh, "Plot a 3-d mesh representation of z[x][y].")
void
plmesh(PLFLT *ArrayX, PLFLT *ArrayY, PLFLT **MatrixCk,
       PLINT nx, PLINT ny, PLINT opt);

DOC(plmeshc, "Plot a 3-d contoured mesh representation of the function z[x][y].")
void
plmeshc(PLFLT *ArrayX, PLFLT *ArrayY, PLFLT **MatrixCk,
	 PLINT nx, PLINT ny, PLINT opt, PLFLT *Array, PLINT n);

DOC(plmkstrm, "Create a new stream and makes it the default.")
void
plmkstrm(PLINT *OUTPUT);

DOC(plmtex, "Print \"text\" at specified position relative to viewport.")
void
plmtex(const char *side, PLFLT disp, PLFLT pos, PLFLT just,
	 const char *text);

DOC(plot3d, "Plot a 3-d representation of the function z[x][y].")
void
plot3d(PLFLT *ArrayX, PLFLT *ArrayY, PLFLT **MatrixCk,
	 PLINT nx, PLINT ny, PLINT opt, PLINT side);

DOC(plot3dc, "Plot a 3-d contoured representation of the function z[x][y].")
void
plot3dc(PLFLT *ArrayX, PLFLT *ArrayY, PLFLT **MatrixCk,
	 PLINT nx, PLINT ny, PLINT opt, PLFLT *Array, PLINT n);

DOC(plsurf3d, "Plot a 3-d contoured surface representation of the function z[x][y].")
void
plsurf3d(PLFLT *ArrayX, PLFLT *ArrayY, PLFLT **MatrixCk,
	 PLINT nx, PLINT ny, PLINT opt, PLFLT *Array, PLINT n);

DOC(plpat, "Set fill pattern directly.")
void
plpat(PLINT n, PLINT *Array, PLINT *ArrayCk);

DOC(plpoin, "Plot array y against x for n points using ASCII code \"code\".")
void
plpoin(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLINT code);

DOC(plpoin3, "Draw a series of points in 3 space.")
void
plpoin3(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk, PLINT code);

DOC(plpoly3, "Draw a polygon in 3 space. ")
void
plpoly3(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLFLT *ArrayCk, PLINT *ArrayCkMinus1,
	    PLINT flag);

DOC(plprec, "Set the floating point precision (in number of places) in numeric labels.")
void
plprec(PLINT setp, PLINT prec);

DOC(plpsty, "Set fill pattern, using one of the predefined patterns.")
void
plpsty(PLINT patt);

DOC(plptex, "Print \"text\" at world cooordinate (x,y).")
void
plptex(PLFLT x, PLFLT y, PLFLT dx, PLFLT dy, PLFLT just, const char *text);

DOC(plreplot, "Replay contents of plot buffer to current device/file.")
void
plreplot(void);

DOC(plschr, "Set character height.")
void
plschr(PLFLT def, PLFLT scale);

DOC(plscmap0, "Set color map 0 colors by 8 bit RGB values.")
void
plscmap0(PLINT *Array, PLINT *ArrayCk, PLINT *ArrayCk, PLINT n);

DOC(plscmap0n, "Set number of colors in cmap 0.")
void
plscmap0n(PLINT ncol0);

DOC(plscmap1, "Set color map 1 colors by 8 bit RGB values.")
void
plscmap1(PLINT *Array, PLINT *ArrayCk, PLINT *ArrayCk, PLINT n);

DOC(plscmap1l, "Set color map 1 colors using a piece-wise linear relationship between intensity [0,1] (cmap 1 index) and position in HLS or RGB color space.")
void
plscmap1l(PLINT itype, PLINT n, PLFLT *Array,
	    PLFLT *ArrayCk, PLFLT *ArrayCk, PLFLT *ArrayCk, PLINT *ArrayCkMinus1);

DOC(plscmap1n, "Set number of colors in cmap 1.")
void
plscmap1n(PLINT ncol1);

DOC(plscol0, "Set 8-bit RGB value in cmap 0.")
void
plscol0(PLINT icol0, PLINT r, PLINT g, PLINT b);

DOC(plscolbg, "Set the background color using 8-bit RGB value.")
void
plscolbg(PLINT r, PLINT g, PLINT b);

DOC(plscolor, "Globally turn color output on/off.")
void
plscolor(PLINT color);

DOC(plscompression, "Set the compression level.")
void
plscompression(PLINT compression);

DOC(plsdev, "Set the device (keyword) name.")
void
plsdev(const char *devname);

DOC(plsdidev, "Set window into device space using margin, aspect ratio, and justification.")
void
plsdidev(PLFLT mar, PLFLT aspect, PLFLT jx, PLFLT jy);

DOC(plsdimap, "Set up transformation from metafile coordinates.")
void
plsdimap(PLINT dimxmin, PLINT dimxmax, PLINT dimymin, PLINT dimymax,
	   PLFLT dimxpmm, PLFLT dimypmm);

DOC(plsdiori, "Set plot orientation, specifying rotation in units of pi/2.")
void
plsdiori(PLFLT rot);

DOC(plsdiplt, "Set window into plot space.")
void
plsdiplt(PLFLT xmin, PLFLT ymin, PLFLT xmax, PLFLT ymax);

DOC(plsdiplz, "Set window into plot space incrementally (zoom).")
void
plsdiplz(PLFLT xmin, PLFLT ymin, PLFLT xmax, PLFLT ymax);

DOC(plsesc, "Set the escape character for text strings.")
void
plsesc(char esc);

DOC(plsetopt, "Process input strings, treating them as an option and argument pair. The first is for the external API, the second the work routine declared here for backward compatibilty.")
PLINT
plsetopt(char *opt, char *optarg);

DOC(plsfam, "Set family file parameters.")
void
plsfam(PLINT fam, PLINT num, PLINT bmax);

DOC(plsfnam, "Set the output file name.")
void
plsfnam(const char *fnam);

DOC(plshades, "Shade regions with continuous range of colours.")
void 
plshades( PLFLT **Matrix, PLINT nx, PLINT ny, defined_func df,
	  PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax,
	  PLFLT *Array, PLINT n, PLINT fill_width,
	  PLINT cont_color, PLINT cont_width,
	  fill_func ff, PLINT rectangular,
	  pltr_func pltr,
	  PLPointer PYOBJECT_DATA);

DOC(plshade, "Shade region with discrete colour, pattern fill.")
void 
plshade(PLFLT **Matrix, PLINT nx, PLINT ny, defined_func df,
	  PLFLT left, PLFLT right, PLFLT bottom, PLFLT top,
	  PLFLT shade_min, PLFLT shade_max,
	  PLINT sh_cmap, PLFLT sh_color, PLINT sh_width,
	  PLINT min_color, PLINT min_width,
	  PLINT max_color, PLINT max_width,
	  fill_func ff, PLINT rectangular,
	  pltr_func pltr,
	  PLPointer PYOBJECT_DATA);

DOC(plsmaj, "Set up lengths of major tick marks.")
void
plsmaj(PLFLT def, PLFLT scale);

DOC(plsmin, "Set up lengths of minor tick marks.")
void
plsmin(PLFLT def, PLFLT scale);

DOC(plsori, "Set orientation.  Must be done before calling plinit.")
void
plsori(PLINT ori);

DOC(plspage, "Set output device parameters.  Usually ignored by the driver.")
void
plspage(PLFLT xp, PLFLT yp, PLINT xleng, PLINT yleng,
	  PLINT xoff, PLINT yoff);

DOC(plspause, "Set the pause (on end-of-page) status.")
void
plspause(PLINT pause);

DOC(plsstrm, "Set stream number.")
void
plsstrm(PLINT strm);

DOC(plssub, "Set the number of subwindows in x and y.")
void
plssub(PLINT nx, PLINT ny);

DOC(plssym, "Set symbol height.")
void
plssym(PLFLT def, PLFLT scale);

DOC(plstar, "Initialize PLplot, passing in the windows/page settings.")
void
plstar(PLINT nx, PLINT ny);

DOC(plstart, "Initialize PLplot, passing the device name and windows/page settings.")
void
plstart(const char *devname, PLINT nx, PLINT ny);

DOC(plstripa, "Add a point to a stripchart. ")
void
plstripa(PLINT id, PLINT pen, PLFLT x, PLFLT y);

DOC(plstripc, "Create 1d stripchart.")
void
plstripc(PLINT *OUTPUT, char *xspec, char *yspec,
	PLFLT xmin, PLFLT xmax, PLFLT xjump, PLFLT ymin, PLFLT ymax,
	PLFLT xlpos, PLFLT ylpos,
	PLINT y_ascl, PLINT acc,
	PLINT colbox, PLINT collab,
	PLINT *Array, PLINT *ArrayCk, char *legline[4],
	char *labx, char *laby, char *labtop);

DOC(plstripd, "Deletes and releases memory used by a stripchart. ")
void
plstripd(PLINT id);

DOC(plstyl, "Set up a new line style.")
void
plstyl(PLINT n, PLINT *Array, PLINT *ArrayCk);

DOC(plsvpa, "Set the edges of the viewport to the specified absolute coordinates.")
void
plsvpa(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax);

DOC(plsxax, "Set x axis labeling parameters.")
void
plsxax(PLINT digmax, PLINT digits);

DOC(plsyax, "Set y axis labeling parameters.")
void
plsyax(PLINT digmax, PLINT digits);

DOC(plsym, "Plot array y against x for n points using Hershey symbol \"code\"")
void
plsym(PLINT n, PLFLT *Array, PLFLT *ArrayCk, PLINT code);

DOC(plszax, "Set z axis labeling parameters")
void
plszax(PLINT digmax, PLINT digits);

DOC(pltext, "Switch to text screen.")
void
pltext(void);

DOC(plvasp, "Sets the edges of the viewport with the given aspect ratio, leaving room for labels.")
void
plvasp(PLFLT aspect);

DOC(plvpas, "Create the largest viewport of the specified aspect ratio that fits within the specified normalized subpage coordinates.")
void
plvpas(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax, PLFLT aspect);

DOC(plvpor, "Create a viewport with the specified normalized subpage coordinates.")
void
plvpor(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax);

DOC(plvsta, "Define a \"standard\" viewport with seven character heights for the left margin and four character heights everywhere else.")
void
plvsta(void);

DOC(plw3d, "Set up a window for three-dimensional plotting.")
void
plw3d(PLFLT basex, PLFLT basey, PLFLT height, PLFLT xmin0,
	PLFLT xmax0, PLFLT ymin0, PLFLT ymax0, PLFLT zmin0,
	PLFLT zmax0, PLFLT alt, PLFLT az);

DOC(plwid, "Set pen width.")
void
plwid(PLINT width);

DOC(plwind, "Set up world coordinates of the viewport boundaries (2d plots).")
void
plwind(PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax);

DOC(plxormod, "Set xor mode; mode = 1-enter, 0-leave, status = 0 if not interactive device.")
void
plxormod(PLINT mode, PLINT *OUTPUT);

# if 0

/* Deprecated functions that are in common API, but we don't want to
 * propagate them to the python API. */

DOC(plrgb, "Set line color by red, green, blue values in range from  0. to 1.")
void
plrgb(PLFLT r, PLFLT g, PLFLT b);

DOC(plrgb1, "Set line color by 8 bit RGB values.")
void
plrgb1(PLINT r, PLINT g, PLINT b);

void 
plshade1(PLFLT *Matrix, PLINT nx, PLINT ny, defined_func df,
	 PLFLT left, PLFLT right, PLFLT bottom, PLFLT top,
	 PLFLT shade_min, PLFLT shade_max,
	 PLINT sh_cmap, PLFLT sh_color, PLINT sh_width,
	 PLINT min_color, PLINT min_width,
	 PLINT max_color, PLINT max_width,
	 fill_func ff, PLINT rectangular,
	 pltr_func pltr,
	 PLPointer PYOBJECT_DATA);

#endif

/*--------------------------------------------------------------------------*\
 *		Functions for use from C or C++ only
 *  N.B. If you want these in python, they should be officially put in 
 *  the common API for all front-ends to the PLplot library with "c_" suffix,
 *  DocBook xml documentation in the api.xml chapter, etc. 
\*--------------------------------------------------------------------------*/

#if 0

/* Draws a contour plot using the function evaluator f2eval and data stored
 * by way of the f2eval_data pointer.  This allows arbitrary organizations
 * of 2d array data to be used. 
 */
void
plfcont(f2eval_func f2eval,
	PLPointer PYOBJECT_DATA,
	PLINT nx, PLINT ny, PLINT kx, PLINT lx,
	PLINT ky, PLINT ly, PLFLT *clevel, PLINT nlevel,
	pltr_func pltr,
	PLPointer PYOBJECT_DATA);
/* plot continental outline in world coordinates */

void
plmap( void (*mapform)(PLINT, PLFLT *, PLFLT *), char *type,
         PLFLT minlong, PLFLT maxlong, PLFLT minlat, PLFLT maxlat );

/* Plot the latitudes and longitudes on the background. */

void 
plmeridians( void (*mapform)(PLINT, PLFLT *, PLFLT *), 
               PLFLT dlong, PLFLT dlat,
               PLFLT minlong, PLFLT maxlong, PLFLT minlat, PLFLT maxlat );

void 
plfshade(f2eval_func,
	 PLPointer PYOBJECT_DATA,
	 c2eval_func,
	 PLPointer c2eval_data,
	 PLINT nx, PLINT ny, 
	 PLFLT left, PLFLT right, PLFLT bottom, PLFLT top,
	 PLFLT shade_min, PLFLT shade_max,
	 PLINT sh_cmap, PLFLT sh_color, PLINT sh_width,
	 PLINT min_color, PLINT min_width,
	 PLINT max_color, PLINT max_width,
	 fill_func, PLINT rectangular,
	 pltr_func,
	 PLPointer PYOBJECT_DATA);

/* Converts input values from relative device coordinates to relative plot */
/* coordinates. */

void
pldid2pc(PLFLT *INOUT, PLFLT *INOUT, PLFLT *INOUT, PLFLT *INOUT);

/* Converts input values from relative plot coordinates to relative */
/* device coordinates. */

void
pldip2dc(PLFLT *INOUT, PLFLT *INOUT, PLFLT *INOUT, PLFLT *INOUT);

/* plots a 2d image (or a matrix too large for plshade() ). */

void
plimage( PLFLT **Matrix, PLINT nx, PLINT ny, 
	 PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax, PLFLT zmin, PLFLT zmax,
	 PLFLT Dxmin, PLFLT Dxmax, PLFLT Dymin, PLFLT Dymax);

/* Returns a list of file-oriented device names and their menu strings */
void
plgFileDevs(char ***p_menustr, char ***p_devname, PLINT *p_ndev);

/* Returns a list of all device names and their menu strings */

void
plgDevs(char ***p_menustr, char ***p_devname, PLINT *p_ndev);

/* Set the function pointer for the keyboard event handler */

void
plsKeyEH(void (*KeyEH) (PLGraphicsIn *, void *, PLINT *), void *KeyEH_data);

/* Set the function pointer for the (mouse) button event handler */

void
plsButtonEH(void (*ButtonEH) (PLGraphicsIn *, void *, PLINT *),
	    void *ButtonEH_data);
#endif
/* Set the variables to be used for storing error info */

#if 0
void
plsError(PLINT *errcode, char *errmsg);
#endif

/* Sets an optional user exit handler. */
#if 0
void
plsexit(PLINT (*handler) (char *));
	/* Transformation routines */
#endif

#if 0
/* Just like pltr2() but uses pointer arithmetic to get coordinates from */
/* 2d grid tables.  */

void
pltr2p(PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty, PLPointer pltr_data);
/* Identity transformation for plots from Fortran. */

void
pltr0f(PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty, void *pltr_data);

/* Does linear interpolation from doubly dimensioned coord arrays */
/* (row dominant, i.e. Fortran ordering). */

void
pltr2f(PLFLT x, PLFLT y, PLFLT *tx, PLFLT *ty, void *pltr_data);

/* Example linear transformation function for contour plotter. */

void 
xform(PLFLT x, PLFLT y, PLFLT * OUTPUT, PLFLT * OUTPUT);
	/* Function evaluators */
/* Does a lookup from a 2d function array.  Array is of type (PLFLT **), */
/* and is column dominant (normal C ordering). */

PLFLT
plf2eval2(PLINT ix, PLINT iy, PLPointer plf2eval_data);

/* Does a lookup from a 2d function array.  Array is of type (PLFLT *), */
/* and is column dominant (normal C ordering). */

PLFLT
plf2eval(PLINT ix, PLINT iy, PLPointer plf2eval_data);

/* Does a lookup from a 2d function array.  Array is of type (PLFLT *), */
/* and is row dominant (Fortran ordering). */

PLFLT
plf2evalr(PLINT ix, PLINT iy, PLPointer plf2eval_data);
#endif
	/* Command line parsing utilities */

/* Clear internal option table info structure. */

void
plClearOpts(void);

/* Reset internal option table info structure. */

void
plResetOpts(void);

/* Merge user option table into internal info structure. */
#if 0
PLINT
plMergeOpts(PLOptionTable *options, char *name, char **notes);
#endif
/* Set the strings used in usage and syntax messages. */

void
plSetUsage(char *program_string, char *usage_string);

#if 0
/* This is wrapped by common API plsetopt so ignore. */
PLINT
plSetOpt(char *opt, char *optarg);
#endif

/* Process options list using current options info. */
PLINT
plParseOpts(PLINT *p_argc, char **argv, PLINT mode);

/* Print usage & syntax message. */

void
plOptUsage(void);

	/* Miscellaneous */
#if 0
/* Set the output file pointer */

void
plgfile(FILE **p_file);

/* Get the output file pointer */

void
plsfile(FILE *file);

/* Get the escape character for text strings. */

void
plgesc(char *p_esc);

/* Front-end to driver escape function. */

void
pl_cmd(PLINT op, void *ptr);

/* Return full pathname for given file if executable */

PLINT 
plFindName(char *p);

/* Looks for the specified executable file according to usual search path. */

char *
plFindCommand(char *fn);

/* Gets search name for file by concatenating the dir, subdir, and file */
/* name, allocating memory as needed.  */

void
plGetName(char *dir, char *subdir, char *filename, char **filespec);

/* Prompts human to input an integer in response to given message. */

PLINT
plGetInt(char *s);

/* Prompts human to input a float in response to given message. */

PLFLT
plGetFlt(char *s);

	/* Nice way to allocate space for a vectored 2d grid */

/* Allocates a block of memory for use as a 2-d grid of PLFLT's.  */

void
plAlloc2dGrid(PLFLT ***f, PLINT nx, PLINT ny);

/* Frees a block of memory allocated with plAlloc2dGrid(). */

void
plFree2dGrid(PLFLT **f, PLINT nx, PLINT ny);

/* Find the maximum and minimum of a 2d matrix allocated with plAllc2dGrid(). */

void
plMinMax2dGrid(PLFLT **f, PLINT nx, PLINT ny, PLFLT *fmax, PLFLT *fmin);

#endif

/* Functions for converting between HLS and RGB color space */

void
plHLS_RGB(PLFLT h, PLFLT l, PLFLT s, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

void
plRGB_HLS(PLFLT r, PLFLT g, PLFLT b, PLFLT *OUTPUT, PLFLT *OUTPUT, PLFLT *OUTPUT);

DOC(plGetCursor, "Wait for graphics input event and translate to world coordinates")
PLINT
plGetCursor(PLGraphicsIn *gin);

/* Translates relative device coordinates to world coordinates.  */
/* Use plcalc_world instead of plTranslateCursor. */
#if 0
int
plTranslateCursor(PLGraphicsIn *gin);
#endif
