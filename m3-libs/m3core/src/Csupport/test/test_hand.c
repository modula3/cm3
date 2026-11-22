#include "hand.c"

WORD_T
__fastcall
test_set_member(WORD_T elt, WORD_T* set)
/* never used by current backend */
{
  WORD_T word = elt / SET_GRAIN;
  WORD_T bit  = elt % SET_GRAIN;
  return (set[word] & (1UL << bit)) != 0;
}

void
__fastcall
test_set_singleton(WORD_T a, WORD_T* s)
/* never used by current backend */
{
  WORD_T a_word = a / SET_GRAIN;
  WORD_T a_bit  = a % SET_GRAIN;
  s[a_word] |= (((WORD_T)1) << a_bit);
}

int main()
{
  size_t a[] = {3};
  size_t b[] = {1};
  size_t c[] = {2};
  size_t* sets[] = {a,b,c};
  const char* names[] = {"a", "b", "c"};
  unsigned i, j;
  unsigned n = sizeof(size_t) * 8;
  
  for (i = 0; i < 3; ++i)
  {
    for (j = 0; j < 3; ++j)
    {
      size_t x = sets[i][0];
      size_t y = sets[j][0];
      
      int lt = set_lt(n, &y, &x);
      int le = set_le(n, &y, &x);
      int gt = set_gt(n, &y, &x);
      int ge = set_ge(n, &y, &x);

      /* reverse the parameters */
      int xlt = set_lt(n, &x, &y);
      int xle = set_le(n, &x, &y);
      int xgt = set_gt(n, &x, &y);
      int xge = set_ge(n, &x, &y);

      assert(xlt == gt);
      assert(xgt == lt);
      assert(xle == ge);
      assert(xge == le);

      if (i == j)
      {
        assert(lt == 0);
        assert(xlt == 0);
        assert(gt == 0);
        assert(xgt == 0);
        assert(le == 1);
        assert(xle == 1);
        assert(ge == 1);
        assert(xge == 1);
      }
      else
      {
        printf("%s < %s %d\n", names[i], names[j], lt);
        printf("%s <= %s %d\n", names[i], names[j], le);
        printf("%s > %s %d\n", names[i], names[j], gt);
        printf("%s >= %s %d\n", names[i], names[j], ge);

        /* if all of j's bits are set in i, then j is le, aka i is ge */
      
        if ((x & y) == y)
        {
          assert(ge);
          assert(xle);
        }
        
        /* if all of i's bits are set in j, then i is le */

        if ((y & x) == x)
        {
          assert(le);
          assert(xge);
        }
          
        /* If they share no bits, they are none of lt, le, gt, ge */

        if ((x & y) == 0)
        {
          assert(!lt);
          assert(!le);
          assert(!gt);
          assert(!ge);
        }
        
        /* If they share bits, but not all of i's: */

        if ((x & y) != 0 && (x & y) != x)
        {
          assert(gt);
          assert(ge);
          assert(xlt);
          assert(xle);
        }

        /* If they share bits, but not all of j's: */

        if ((x & y) != 0 && (x & y) != y)
        {
          assert(lt);
          assert(le);
          assert(xgt);
          assert(xge);
        }
  
      }
    }
 }

  return 0;
 }
