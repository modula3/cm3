/* Support for printing Modula-3 values for GDB, the GNU debugger.
   Copyright 1994, Digitial Equipement Corporation */

#include "defs.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "value.h"
#include "demangle.h"
#include "valprint.h"
#include "language.h"

#include "m3-lang.h"


LONGEST
m3_unpack_ord (valaddr, bitpos, bitsize, sign_extend)
     char *valaddr;
     int bitpos;
     int bitsize;
     int sign_extend;
{
  LONGEST res, mask; 

  if (bitpos % 8 + bitsize <= 8) {		/* char access */
    valaddr += bitpos / 8;
    bitpos = bitpos % 8;
    res = *((char *)valaddr); }
  else if (bitpos % 16 + bitsize <= 16) {	/* short access */
    short *v = (short *)valaddr;
    v += bitpos / 16;
    bitpos = bitpos % 16;
    res = *v; }
  else if (bitpos % 32 + bitsize <= 32) {	/* int access */
    int *v = (int *)valaddr;
    v += bitpos / 32;
    bitpos = bitpos % 32;
    res = *v; }
  else if (bitpos % 64 + bitsize <= TARGET_LONG_BIT) {	/* long access */
    long *v = (long *)valaddr;
    v += bitpos / 64;
    bitpos = bitpos % 64;
    res = *v; }
  else {
    error ("wrong bitsize in m3_unpack_ord: %d", bitsize); }

  mask = ((unsigned long)(~0L)) >> (sizeof (res) * HOST_CHAR_BIT - bitsize);
  res  = res >> bitpos;
  res  = res & mask;

  if ((sign_extend)
     && (bitsize > 0)
     && (bitsize < sizeof(res) * HOST_CHAR_BIT)
     && (res & (1L << (bitsize-1)))) {
    res = res | ((~0L) << bitsize);
  }

  return res;
}

LONGEST
m3_unpack_int2 (val)
     value_ptr val;
{
  LONGEST lower, upper;
  struct type * type = VALUE_TYPE (val);

  m3_ordinal_bounds (type, &lower, &upper);
  return m3_unpack_ord (VALUE_CONTENTS (val),  0,
			TYPE_M3_SIZE(type), (lower < 0));
}


CORE_ADDR
m3_unpack_pointer (valaddr, bitpos)
     char *valaddr;
     int bitpos;
{
  return (CORE_ADDR) m3_unpack_ord (valaddr, bitpos, TARGET_PTR_BIT, 0);
}

CORE_ADDR
m3_unpack_pointer2 (val)
     value_ptr val;
{
  return *(CORE_ADDR*) VALUE_CONTENTS (val);
}


double 
m3_unpack_float2 (val)
     value_ptr val;
{
  double res;
  int size = TYPE_LENGTH (VALUE_TYPE (val));

  if (size == 4) {
    res = *(float *) VALUE_CONTENTS (val); }
  else {
    res = *(double*) VALUE_CONTENTS (val); }
  return res;
}


static void 
m3_print_scalar (valaddr, bitpos, bitsize, stream, format, sign_extend)
     char *valaddr;
     int bitpos, bitsize;
     FILE *stream;
     int format;
     int sign_extend;
{
  LONGEST v = m3_unpack_ord (valaddr, bitpos, bitsize, sign_extend);

  switch (format) {
    case 'x':
      if (v == 0)
	fputs_filtered ("NIL", stream);
      else
	fprintf_filtered (stream, "16_%lx", v); break;
    case 'o': fprintf_filtered (stream, "8_%lo", v);  break;
    case 'd':
    default:  fprintf_filtered (stream, "%ld", v);    break; }
}

static 
m3_print_object_1 (valaddr, tc_addr, stream, format)
     char *valaddr;
     CORE_ADDR tc_addr;
     FILE *stream;
     int format;
{
  char name [100];
  struct type *this_obj;
  struct symbol *this_obj_sym;
  int i, data_offset; 

  if (tc_addr == 0) {
    return; }

  this_obj = find_m3_type_from_tc (tc_addr);
  if (TYPE_CODE (this_obj) == TYPE_CODE_M3_ROOT
      || TYPE_CODE (this_obj) == TYPE_CODE_M3_UN_ROOT) {
    return; }

  m3_print_object_1 (valaddr, tc_address_to_parent_tc_address (tc_addr),
		     stream, format);

  data_offset = tc_address_to_dataOffset (tc_addr);

  if (TYPE_M3_OBJ_NFIELDS (this_obj) > 0) {
    fputs_filtered ("(* ", stream);
    m3_print_type(this_obj, 0, stream, 0, 0);
    fputs_filtered (" *) ", stream);
  }

  for (i = 0; i < TYPE_M3_OBJ_NFIELDS (this_obj); i++) {
    if (i != 0) {
      fputs_filtered ("; ", stream);
      wrap_here ("    "); }
    fputs_filtered (TYPE_M3_OBJ_FIELD_NAME (this_obj, i), stream);
    fputs_filtered (" = ", stream);
    m3_val_print2 (TYPE_M3_OBJ_FIELD_TYPE (this_obj, i), 
		   valaddr, 
		   data_offset * TARGET_CHAR_BIT + 
		      TYPE_M3_OBJ_FIELD_BITPOS (this_obj, i),
		   TYPE_M3_OBJ_FIELD_BITSIZE (this_obj, i),
		   stream, format, 0, 0); }
  if (i > 0) {
    fputs_filtered ("; ", stream);
    wrap_here ("    "); }
}

void m3_read_object_fields_bits (ref, type, tc_addr_res, bits)
     CORE_ADDR ref;
     struct type *type;
     CORE_ADDR *tc_addr_res;
     char **bits;
{
  int typecode;
  CORE_ADDR typecode_addr, tc_addr;
  int dataSize, dataOffset;

  if (ref == 0) {
    if (tc_addr_res != 0) { *tc_addr_res = 0; }
    *bits = 0;
    return;
  }

  tc_addr = find_m3_heap_tc_addr (ref);
  dataSize = tc_address_to_dataSize (tc_addr);
  *bits = malloc (dataSize);
  target_read_memory (ref, *bits, dataSize);
  if (tc_addr_res != 0) { *tc_addr_res = tc_addr; }
}

static void
m3_print_object (valaddr, bitpos, type, stream, format)
     char *valaddr;
     int bitpos;
     struct type *type;
     FILE *stream;
     int format;
{
  char *bits;
  CORE_ADDR ref, tc_addr;

  ref = m3_unpack_pointer (valaddr, bitpos);
  m3_read_object_fields_bits (ref, type, &tc_addr, &bits);

  if (bits == 0) {
    fputs_filtered ("NIL", stream);
    return;
  }

  fputs_filtered ("OBJECT ", stream);
  m3_print_object_1 (bits, tc_addr, stream, format);
  fputs_filtered (" END", stream);
}


/* Print data of type TYPE located at VALADDR (within GDB), which came from
   the inferior at address ADDRESS, onto stdio stream STREAM according to
   FORMAT (a letter or 0 for natural format).  The data at VALADDR is in
   target byte order.
   
   If the data are a string pointer, returns the number of string characters
   printed.
   
   If DEREF_REF is nonzero, then dereference references, otherwise just print
   them like pointers.
   
   The PRETTY parameter controls prettyprinting.  */

static int 
compare (valaddr, bitpos1, bitpos2, bitsize)
     char *valaddr;
     int bitpos1, bitpos2, bitsize;
{
  if ((bitpos1 % 8) != 0 || (bitpos2 % 8) != 0 || (bitsize % 8 != 0)) {
    /* this comparaisons are too hard for now */
    return 0; }
  return memcmp (valaddr + bitpos1/8, valaddr + bitpos2/8, bitsize/8) == 0;
}

extern unsigned int repeat_count_threshold;

int
m3_val_print2 (type, valaddr, bitpos, bitsize, stream, format, deref_ref, toplevel)
     struct type *type;
     char *valaddr;
     int bitpos;
     int bitsize;
     FILE *stream;
     int format;
     int deref_ref;
     int toplevel;
{
  register unsigned int i = 0;		/* Number of characters printed */
  unsigned len;
  struct type *elttype;
  unsigned eltlen;
  LONGEST val;
  unsigned char c;
  CORE_ADDR addr;
  int things_printed = 0;
  int reps, j;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_M3_ARRAY: {
      struct type *index = TYPE_M3_ARRAY_INDEX (type);
      struct type *elt   = TYPE_M3_ARRAY_ELEM (type);
      LONGEST lower, upper, n;
      
      fputs_filtered ("[", stream);

      m3_ordinal_bounds (index, &lower, &upper);
      n = upper - lower + 1;

      for (i = things_printed = 0; i < n && things_printed < print_max; i++) {
	if (i != 0) {
	  fputs_filtered (", ", stream);
	  wrap_here ("    "); }

	m3_val_print2 (elt, valaddr, 
		       bitpos + i * TYPE_M3_SIZE (elt), TYPE_M3_SIZE (elt),
		       stream, format, 0, 0);
        things_printed++;
	for (j = i + 1, reps = 1; 
	     j < n &&  compare (valaddr, bitpos + i * TYPE_M3_SIZE (elt),
				bitpos + j * TYPE_M3_SIZE (elt),
				TYPE_M3_SIZE (elt));
	     j++, reps++);
	if (reps > repeat_count_threshold) {
	  fprintf_filtered (stream, " <repeats %d times>", reps);
	  i += reps - 1;
	  things_printed += repeat_count_threshold; }}
	    
      if (i < n) {
	fputs_filtered ("...", stream); }

      fputs_filtered ("]", stream);
      break; }
      
    case TYPE_CODE_M3_OPEN_ARRAY: {
      struct type *elt_type = TYPE_M3_OPEN_ARRAY_ELEM (type);
      CORE_ADDR    elems    = m3_unpack_pointer (valaddr, bitpos);
      int          eltsize  = 1;
      int          nelems;

      nelems = m3_unpack_ord (valaddr + TARGET_PTR_BIT/HOST_CHAR_BIT,
				       bitpos, TARGET_LONG_BIT, 0);

      if (bitpos % HOST_CHAR_BIT != 0) {
	error ("improperly aligned open array"); }

      valaddr += (bitpos / HOST_CHAR_BIT);
      bitpos = 0;

      { struct type *e = elt_type;
	char *nelem_addr = valaddr
	                    + (TARGET_PTR_BIT + TARGET_LONG_BIT)/HOST_CHAR_BIT;
	while (TYPE_CODE (e) == TYPE_CODE_M3_OPEN_ARRAY) {
	  eltsize = eltsize * m3_unpack_ord (nelem_addr, 0, TARGET_LONG_BIT,0);
	  nelem_addr += TARGET_LONG_BIT / HOST_CHAR_BIT;
	  e = TYPE_M3_OPEN_ARRAY_ELEM (e); }
	eltsize = eltsize * TYPE_M3_SIZE (e); }
      if (eltsize % 8 != 0) {
	error ("another improper alignment"); }
      eltsize = eltsize / 8;

      fputs_filtered ("[", stream);
      if (TYPE_CODE (elt_type) == TYPE_CODE_M3_OPEN_ARRAY) {
	for (i = things_printed = 0; 
	     i < nelems && things_printed < print_max; i++) {
	  if (i > 0) {
	    fputs_filtered (",", stream); 
	    wrap_here ("    "); }
	  *(long*)(valaddr + TARGET_PTR_BIT / HOST_CHAR_BIT) = elems + i * eltsize;
	  m3_val_print2 (elt_type, valaddr + TARGET_PTR_BIT / HOST_CHAR_BIT, 
			 0, TYPE_M3_SIZE (elt_type), 
			 stream, format, 0, 0);
	  things_printed++; }}
      else {
	char *a = alloca (eltsize);
	char *previous = alloca (eltsize);
	reps = 0;
	for (i = things_printed = 0; 
	     i < nelems && things_printed < print_max; i++) {
	  target_read_memory (elems, a, eltsize);
	  if (reps > 0 && memcmp (a, previous, eltsize) == 0) {
	    reps++; }
	  else {
	    if (reps > 1) {
	      if (reps > repeat_count_threshold) {
		fprintf_filtered (stream, " <repeats %d times>", reps); }
	      else {
		for (j = 0; j < reps - 1 && things_printed < print_max; j++) {
		  if (things_printed) {
		    fputs_filtered (",", stream); 
		    wrap_here ("    "); }
		  m3_val_print2 (elt_type, previous, 
				 0, TYPE_M3_SIZE (elt_type), 
				 stream, format, 0, 0);
		  things_printed++; }}
	      things_printed += reps; }
	    if (things_printed < print_max) {
	      if (things_printed) {
		fputs_filtered (",", stream); 
		wrap_here ("    "); }
	      m3_val_print2 (elt_type, a, 
			     0, TYPE_M3_SIZE (elt_type), 
			     stream, format, 0, 0);
	      things_printed++; }
	    reps = 1;
	    memcpy (previous, a, eltsize); }
	  elems += eltsize; }
	if (reps > 1) {
	  if (reps > repeat_count_threshold) {
	    fprintf_filtered (stream, " <repeats %d times>", reps); 
	    things_printed += reps - 1; }
	  else {
	    for (j = 0; j < reps - 1 && things_printed < print_max; j++) {
	      if (things_printed) {
		fputs_filtered (",", stream); 
		wrap_here ("    ");  }
	      m3_val_print2 (elt_type, previous, 
			     0, TYPE_M3_SIZE (elt_type),
			     stream, format, 0, 0);
	      things_printed++; }}}}
      if (things_printed < nelems) {
	fputs_filtered ("...", stream); }
      fputs_filtered ("]", stream);
      break; }

    case TYPE_CODE_M3_PACKED: {
      m3_val_print2 (TYPE_M3_PACKED_TARGET (type), valaddr,
		     bitpos, TYPE_M3_SIZE (type),
		     stream, format, 0, 0);
      break; }
      
    case TYPE_CODE_M3_ENUM: {
      LONGEST lower, upper, val;
      m3_ordinal_bounds (type, &lower, &upper);
      val = m3_unpack_ord (valaddr, bitpos, bitsize, (lower < 0));
      if ((lower <= val) && (val <= upper)) {
        fputs_filtered (TYPE_M3_ENUM_VALNAME (type, val), stream);
      } else {
	fprintf_filtered (stream, "<enum value %ld out of range [%ld..%ld]>",
			  val, lower, upper);
      };
      break; }
      
    case TYPE_CODE_M3_INDIRECT: {
      CORE_ADDR target_addr = m3_unpack_pointer (valaddr, 0);
      struct type *target = TYPE_M3_INDIRECT_TARGET (type);
      int target_size = TYPE_LENGTH (target);
      char *target_val = alloca (target_size); 
      
      target_read_memory (target_addr, target_val, target_size);
      m3_val_print2 (target, target_val, 
		     0, TYPE_M3_SIZE (target), 
		     stream, format, deref_ref, toplevel);
      break; }
      
    case TYPE_CODE_M3_PROC: {
      m3_print_scalar (valaddr, bitpos, bitsize, stream,
		       format ? format : 'x', 0);
      break; }
      
    case TYPE_CODE_M3_RECORD: {
      fputs_filtered ("RECORD ", stream);
      for (i = 0; i < TYPE_M3_REC_NFIELDS (type); i++) {
	if (TYPE_M3_REC_FIELD_NAME (type, i)[0] != '_') {
	  fputs_filtered (TYPE_M3_REC_FIELD_NAME (type, i), stream);
	  fputs_filtered (" = ", stream);
	  m3_val_print2 (TYPE_M3_REC_FIELD_TYPE (type, i), valaddr,
			 bitpos + TYPE_M3_REC_FIELD_BITPOS (type, i),
			 TYPE_M3_SIZE (TYPE_M3_REC_FIELD_TYPE (type, i)),
			 stream, format, 0, 0);
	  fputs_filtered ("; ", stream);
	  wrap_here ("    ");  }}
      fputs_filtered (" END", stream);
      break; }
      
    case TYPE_CODE_M3_SET: {
      int n = 0;
      int j;
      LONGEST lower, upper;
      struct type *target = TYPE_M3_SET_TARGET (type);
      int nelems = TYPE_NFIELDS (target);
      int en = (TYPE_CODE (target) == TYPE_CODE_M3_ENUM);
      int ch = (TYPE_CODE (target) == TYPE_CODE_M3_CHAR);
      int chs = (TYPE_CODE (target) == TYPE_CODE_M3_SUBRANGE)
	&& (TYPE_CODE (TYPE_M3_SUBRANGE_TARGET (target)) == TYPE_CODE_M3_CHAR);

      m3_ordinal_bounds (target, &lower, &upper);
      fputs_filtered ("{", stream);
      
      for (i = 0; i < TYPE_LENGTH (type) / sizeof (long); i++) {
	val = m3_unpack_ord (valaddr, bitpos, TARGET_LONG_BIT, 0);
	for (j = 0; j < TARGET_LONG_BIT; j++) {
	  LONGEST ord = i * TARGET_LONG_BIT + j + lower;
	  if ((val & 1 << j) && (ord <= upper)) {
	    if (n > 0) {
	      fputs_filtered (", ", stream); }
	    if (en) {
	      fputs_filtered (TYPE_FIELD_NAME (target, ord), stream); }
	    else if (ch) {
	      fprintf_filtered (stream, "'%c'", ord); }
	    else if (chs) {
	      fprintf_filtered (stream, "'%c'", ord); }
	    else {
	      fprintf_filtered (stream, "%ld", ord); }
	    n++; }}
	valaddr += sizeof (long); }
      
      fputs_filtered ("}", stream);
      
      break; }
      
    case TYPE_CODE_M3_SUBRANGE: {
      LONGEST lower, upper, val;
      struct type *target = TYPE_M3_SUBRANGE_TARGET (type);
      m3_ordinal_bounds (type, &lower, &upper);
      val = m3_unpack_ord (valaddr, bitpos, bitsize, (lower < 0));
      if ((val < lower) || (upper < val)) {
        fprintf_filtered(stream,"<subrange value %ld out of range [%ld..%ld]>",
			 val, lower, upper);
      } else if (TYPE_CODE (target) == TYPE_CODE_M3_ENUM) {
        fputs_filtered (TYPE_M3_ENUM_VALNAME (target, val), stream);
      } else {
	m3_print_scalar (valaddr, bitpos, bitsize, stream, format, (lower <0));
      }
      break; }

    case TYPE_CODE_M3_ADDRESS:
      m3_print_scalar (valaddr, bitpos, bitsize, stream, 
		       format ? format : 'x', 0);
      break;

    case TYPE_CODE_M3_BOOLEAN:
      if (m3_unpack_ord (valaddr, bitpos, bitsize, 0)) {
	fputs_filtered ("TRUE", stream); }
      else {
	fputs_filtered ("FALSE", stream); }
      break;

    case TYPE_CODE_M3_CHAR:
      m3_printchar (m3_unpack_ord (valaddr, bitpos, 8, 0), stream);
      break;

    case TYPE_CODE_M3_INTEGER:
    case TYPE_CODE_M3_CARDINAL:
    case TYPE_CODE_M3_NULL:
    case TYPE_CODE_M3_VOID:
      m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 1);
      break;

    case TYPE_CODE_M3_ROOT:
    case TYPE_CODE_M3_UN_ROOT:
    case TYPE_CODE_M3_OBJECT: {
      if (deref_ref && !format) {
	m3_print_object (valaddr, bitpos, type, stream, format); }
      else {
	m3_print_scalar (valaddr, bitpos, bitsize, stream, 
			 format ? format : 'x', 0); }
      break; }

    case TYPE_CODE_M3_REFANY: {
      m3_print_scalar (valaddr, bitpos, bitsize, stream, 
		       format ? format : 'x', 0);
      break; }

    case TYPE_CODE_M3_POINTER: {
      struct type *target = TYPE_M3_POINTER_TARGET (type);
      if (TYPE_CODE (target) == TYPE_CODE_M3_OPEN_ARRAY
	  && TYPE_CODE (TYPE_M3_OPEN_ARRAY_ELEM (target)) == TYPE_CODE_M3_CHAR) {
	CORE_ADDR chars_addr;
	CORE_ADDR text_value;
	text_value = m3_unpack_pointer (valaddr, bitpos);
	if (text_value == 0) {
	  fputs_filtered ("NIL", stream); }
	else {
	  target_read_memory (text_value, &chars_addr, TYPE_M3_SIZE (type));
	  val_print_string (chars_addr, 0, stream); }}
      else
	m3_print_scalar (valaddr, bitpos, bitsize, stream, 
			 format ? format : 'x', 0);

      break; }

    case TYPE_CODE_FLT: {
      if (format) {
	m3_print_scalar (valaddr, bitpos, bitsize, stream, format, 0); }
      else {
	if (bitpos % 8 != 0) {
	  error ("improperly aligned floating point value"); }
	print_floating (valaddr + bitpos / 8, type, stream); }
      break; }

    case TYPE_CODE_STRING:
      fputs_filtered ("\"", stream);
      fputs_filtered (valaddr, stream);
      fputs_filtered ("\"", stream);
      break;

    default: 
     return 1; }

  fflush (stream);
  return (0);
}

int
m3_val_print (type, valaddr, address, stream, format, deref_ref, recurse,
	     pretty)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
{
  if (m3_val_print2 (type, valaddr, 0, TYPE_M3_SIZE (type),
		     stream, format, deref_ref, 1)) {
    /* like the value of registers */
    c_val_print (type, valaddr, address, stream, format, deref_ref,
		 recurse, pretty); }
}

int
m3_value_print (val, stream, format, pretty)
     value_ptr val;
     GDB_FILE *stream;
     int format;
     enum val_prettyprint pretty;
{
  struct type *type = VALUE_TYPE (val);

  /* on top-level, prefix pointer value with (type) if it's not a text */
    if (TYPE_CODE (type) == TYPE_CODE_M3_POINTER) {
      struct type *target = TYPE_M3_POINTER_TARGET (type);
      if (TYPE_CODE (target) != TYPE_CODE_M3_OPEN_ARRAY
	  || TYPE_CODE (TYPE_M3_OPEN_ARRAY_ELEM (target)) != TYPE_CODE_M3_CHAR) {
	fprintf_filtered (stream, "(* ");
	m3_print_type (type, "", stream, 0, 0);
	fprintf_filtered (stream, " *) ");
      }
    }

  return (val_print (type, VALUE_CONTENTS (val),
		     VALUE_ADDRESS (val), stream, format, 1, 0, pretty));
}
