/* Copyright 1995 Digital Equipment Corporation. */
/* Distributed only by permission. */

/* Lectern: a user interface for viewing documents stored as images */
/* C optimizations for LGM module */
/* Almost all functions herein are direct analogs of functions in LGM.m3 */

/* Last modified on Tue Feb 14 11:28:31 PST 1995 by birrell  */
/*      modified on Tue Nov 15 18:31:53 PST 1994 by wobber   */

#define PIXWORDBITS   sizeof(int)*8

int bitsIn[256];

void ScaleInit()
{
    int i;
    int done = 1;

    bitsIn[0] = 0;
    while (done < 256) {
        for (i=0;i<done;i++) bitsIn[done+i] = bitsIn[i] + 1;
        done *= 2;
    }
}

/* corresponds to LGM.FastScaleBy2 */

void By2(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i = 0;
    int       j = 0;
    unsigned  source = *pixels;

    while (i < width) {
        if (source == 0) {
            i += PIXWORDBITS/2 - j;
            source = *++pixels;
            j = 0;
        } else {
            line[i++] += bitsIn[source & 3];
            if (++j == PIXWORDBITS/2) {
                source = *++pixels;
                j = 0;
            } else
                source >>= 2;
        }
    }
}

/* corresponds to LGM.FastScaleBy3 */

void By3(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i = 0;
    int       j = 0;
    int       grabBit = 1;
    unsigned  source = *pixels;
    unsigned  temp;

    while (i < width) {
        if (j == PIXWORDBITS/3) {
            temp = source;
            source = *++pixels;
            switch (grabBit) {
            case 0:
                line[i++] += bitsIn[source & 7];
                grabBit = 1;
                j = 1;
                source >>= 3;
                break;
            case 1:
                line[i++] += bitsIn[temp] + (source & 1);
                source >>= 1;
                grabBit = 2;
                j = 0;
                break;
            case 2:
                line[i++] += temp + bitsIn[source & 3];
                source >>= 2;
                grabBit = 0;
                j = 0;
                break;
            }
        } else if (source == 0) {
            i += PIXWORDBITS/3 - j;
            j = PIXWORDBITS/3;
        } else {
            line[i++] += bitsIn[source & 7];
            j++;
            source >>= 3;
        } 
    }  
}

/* corresponds to LGM.FastScaleBy4 */

void By4(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i = 0;
    int       j = 0;
    int       source = *pixels;

    while (i < width) {
        if (source == 0) {
            i += PIXWORDBITS/4 - j;
            source = *++pixels;
            j = 0;
        } else {
            line[i++] += bitsIn[source & 15];
            if (++j == PIXWORDBITS/4) {
                source = *++pixels;
                j = 0;
            } else
                source >>= 4;
        }
    }
}

/* corresponds to LGM.FastScaleBy48 */

void By48(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i,k;
    int       j = PIXWORDBITS/8;
    int       source = *pixels;

    for (i=0;i<width;i++) {
        k = 6;
        while (k > 0) {
            if (source == 0) {
                if (j <= k) {
                    k -= j;
                    j = PIXWORDBITS/8;
                    source = *++pixels;
                } else {
                    j -= k;
                    break;
                }
            } else {
                k--;
                line[i] += bitsIn[source & 255];
                if (--j == 0) {
                    source = *++pixels;
                    j = PIXWORDBITS/8;
                } else
                    source >>= 8;
	    }
        }
    }
}

/* corresponds to LGM.FastScaleByN */

void ByN(line, pixels, width, n)
long     *line;
unsigned *pixels;
int       width;
int       n;
{
    int       i, k;
    int       j = 0;
    unsigned       source = *pixels;

    for (i=0;i<width;i++) {
        for (k=0;k<n;k++) {
            if (source & 1) line[i] += 1;
            source >>= 1;
            if (++j == PIXWORDBITS) {
                source = *++pixels;
                j = 0;
            }
        }
    }
}

/* corresponds to LGM.FastScaleBy2R */

void By2R(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i;
    int       j = PIXWORDBITS;
    unsigned  source = *pixels;

    for (i=0;i<width;i++) {
        j -= 2;
        line[i] += bitsIn[(source >> j) & 3];
        if (j == 0) {
            source = *++pixels;
            j = PIXWORDBITS;
        };
    }
}

/* corresponds to LGM.FastScaleBy4R */

void By4R(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i;
    int       j = PIXWORDBITS;
    unsigned  source = *pixels;

    for (i=0;i<width;i++) {
        j -= 4;
        line[i] += bitsIn[(source >> j) & 15];
        if (j == 0) {
            source = *++pixels;
            j = PIXWORDBITS;
        };
    }
}

/* corresponds to LGM.FastScaleBy48R */

void By48R(line, pixels, width)
long     *line;
unsigned *pixels;
int       width;
{
    int       i,k;
    int       j = PIXWORDBITS;
    unsigned  source = *pixels;

    for (i=0;i<width;i++) {
        for (k=0;k<6;k++) {
            j -= 8;
            line[i] += bitsIn[(source >> j) & 255];
            if (j == 0) {
                source = *++pixels;
                j = PIXWORDBITS;
            }
        }
    }
}

/* corresponds to LGM.FastScaleByNR */

void ByNR(line, pixels, width, n)
long     *line;
unsigned *pixels;
int       width;
int       n;
{
    int       i, k;
    int       j = 0;
    int       source = *pixels;

    for (i=0;i<width;i++) {
        for (k=0;k<n;k++) {
            if (source < 0) line[i] += 1;
            source <<= 1;
            if (++j == PIXWORDBITS) {
                source = *++pixels;
                j = 0;
            }
        }
    }
}

void FastMap(line, nline, map)
long     *line;
int       nline;
long     *map;
{
    long      *lim = line + nline;

    while (line < lim)
         *line++ = map[*line];
}


/* accumulate totals for scaling a line of RGB pixels */

void ScaleRGB(line, nline, unscaled, scale)
long             *line;
int               nline;
unsigned char    *unscaled;
int               scale;
{
    long      *lim = line + nline;
    int        i;

    while (line < lim) {
        unsigned int     total0 = 0;
        unsigned int     total1 = 0;
        unsigned int     total2 = 0;
        for (i=0; i < scale; i++) {
            total0 += *unscaled++;
            total1 += *unscaled++;
            total2 += *unscaled++;
	  };
        *line++ += total0;
        *line++ += total1;
        *line++ += total2;
      }
  }


/* map accumulated total to [0..255], for RGB scaling */

void FastMapToChar(line, nline, map, destChars)
long     *line;
int       nline;
char     *map;
char     *destChars;
{
    long      *lim = line + nline;

    while (line < lim)
         *destChars++ = map[*line++];
}

void FastPack(src, dest, n)
long    *src;
char    *dest;
int      n;
{
    while (n-- > 0) {
        *dest++ = (src[0] << 6) +  (src[1] << 4) + (src[2] << 2) + src[3];
        src += 4;
    }
}

/* corresponds to LGM.FinishPRun, this and prev point at th[h], prev[h] */

int FinishPRun(this, prev, lim)
char    *this;
char    *prev;
int      lim;
{
    char    c = *this;
    char   *start = this;
    char   *plim = this + lim;

    for (;;) {
        if (c != 0) {
            while (*++this == *++prev)
                if (this == plim) break;
            return (this - start);
        }
        if (++this == plim) return (this - start);
        c = *this;
        if (c != *++prev)
            if (c == 0) {
                while (*++this == 0)
                    if (this == plim) break;
                return -(this - start);
            } else
                return (this - start);
    }
}

/* corresponds to LGM.FinishPRun, this points at th[h] */

int FinishZRun(this, lim)
char    *this;
int      lim;
{
    char   *start = this;
    char   *plim = this + lim;

    while (*++this == 0)
        if (this == plim) break;
    return (this - start);
}
