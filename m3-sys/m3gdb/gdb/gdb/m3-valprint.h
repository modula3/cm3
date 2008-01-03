/* See whether field field_name of object at inferior address ref, with inferior
   typecell address tc_addr is the "buf" field of an object of type 
   TextLiteral.T.  If so, return a trumped-up type that is right for printing
   the buf field, having computed its element count and element type from the
   object.  This type is good until the next call on this function.  If not,
   return zero.  
*/ 

#if !defined (M3_VALPRINT_H)
#define M3_VALPRINT_H 1
 
#include "defs.h"
#include "gdbtypes.h"

extern bool /* Yes, it's that field of that type. */ 
m3_check_TextLiteral_buf 
  ( CORE_ADDR ref, 
    CORE_ADDR tc_addr, 
    char * field_name, 
    int * bitsize,
    int * bitpos, /* of the field, relative to ref */  
    struct type ** field_type 
  );  

extern int m3_val_print2 (struct type *, const gdb_byte *, int, int, 
                          struct ui_file *, int, int, int);

extern int m3_val_print (struct type *, const gdb_byte *, int, CORE_ADDR,
			 struct ui_file *, int, int, int,
			 enum val_prettyprint);

#endif /* !defined (M3_VALPRINT_H) */

/* End of file m3-valprint.h */ 
