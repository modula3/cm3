c
c     $Id$
c
      subroutine muld(a, b, p, ar, ac, bc)
      implicit none
c
c     Modula-3/C array indexing M(COLS,ROWS)
c
      integer ar, ac, bc
      double precision a(ac,ar), b(bc,ac), p(bc,ar)

      integer row, col, term
      double precision elem

      do 200 row = 1, ar
         do 150 col = 1, bc
            p(col,row) = 0.0
            do 100 term = 1, ac
               p(col,row) = p(col,row) + a(term,row) * b(col,term)
 100        continue
 150     continue
 200  continue
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

