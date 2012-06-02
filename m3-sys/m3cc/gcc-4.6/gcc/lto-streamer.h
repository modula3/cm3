/* Modula-3: modified */

/* Data structures and declarations used for reading and writing
   GIMPLE to a file stream.

   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Doug Kwan <dougkwan@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_LTO_STREAMER_H
#define GCC_LTO_STREAMER_H

#include "plugin-api.h"
#include "tree.h"
#include "gimple.h"
#include "target.h"
#include "cgraph.h"
#include "vec.h"
#include "vecprim.h"
#include "alloc-pool.h"
#include "gcov-io.h"

EXTERN_C_START

typedef unsigned HOST_WIDE_INT bitpack_word_t;
DEF_VEC_I(bitpack_word_t);
DEF_VEC_ALLOC_I(bitpack_word_t, heap);

/* Tags representing the various IL objects written to the bytecode file
   (GIMPLE statements, basic blocks, EH regions, tree nodes, etc).

   NOTE, when adding new LTO tags, also update lto_tag_name.  */
enum LTO_tags
{
  LTO_null = 0,

  /* Reserve enough entries to fit all the tree and gimple codes handled
     by the streamer.  This guarantees that:

     1- Given a tree code C:
     		enum LTO_tags tag == C + 1

     2- Given a gimple code C:
		enum LTO_tags tag == C + NUM_TREE_CODES + 1

     Conversely, to map between LTO tags and tree/gimple codes, the
     reverse operation must be applied.  */
  LTO_bb0 = 1 + NUM_TREE_CODES + LAST_AND_UNUSED_GIMPLE_CODE,
  LTO_bb1,

  /* EH region holding the previous statement.  */
  LTO_eh_region,

  /* An MD or NORMAL builtin.  Only the code and class are streamed out.  */
  LTO_builtin_decl,

  /* Function body.  */
  LTO_function,

  /* EH table.  */
  LTO_eh_table,

  /* EH region types.  These mirror enum eh_region_type.  */
  LTO_ert_cleanup,
  LTO_ert_try,
  LTO_ert_allowed_exceptions,
  LTO_ert_must_not_throw,

  /* EH landing pad.  */
  LTO_eh_landing_pad,

  /* EH try/catch node.  */
  LTO_eh_catch,

  /* Special for global streamer. Reference to previously-streamed node.  */
  LTO_tree_pickle_reference,

  /* References to indexable tree nodes.  These objects are stored in
     tables that are written separately from the function bodies that
     reference them.  This way they can be instantiated even when the
     referencing functions aren't (e.g., during WPA) and it also allows
     functions to be copied from one file to another without having
     to unpickle the body first (the references are location
     independent).

     NOTE, do not regroup these values as the grouping is exposed
     in the range checks done in lto_input_tree.  */
  LTO_field_decl_ref,			/* Do not change.  */
  LTO_function_decl_ref,
  LTO_label_decl_ref,
  LTO_namespace_decl_ref,
  LTO_result_decl_ref,
  LTO_ssa_name_ref,
  LTO_type_decl_ref,
  LTO_type_ref,
  LTO_const_decl_ref,
  LTO_imported_decl_ref,
  LTO_translation_unit_decl_ref,
  LTO_global_decl_ref,			/* Do not change.  */

  /* This tag must always be last.  */
  LTO_NUM_TAGS
};


/* Set of section types that are in an LTO file.  This list will grow
   as the number of IPA passes grows since each IPA pass will need its
   own section type to store its summary information.

   When adding a new section type, you must also extend the
   LTO_SECTION_NAME array in lto-section-in.c.  */
enum lto_section_type
{
  LTO_section_decls = 0,
  LTO_section_function_body,
  LTO_section_static_initializer,
  LTO_section_cgraph,
  LTO_section_varpool,
  LTO_section_refs,
  LTO_section_jump_functions,
  LTO_section_ipa_pure_const,
  LTO_section_ipa_reference,
  LTO_section_symtab,
  LTO_section_opts,
  LTO_section_cgraph_opt_sum,
  LTO_N_SECTION_TYPES		/* Must be last.  */
};

/* Indices to the various function, type and symbol streams. */
typedef enum
{
  LTO_DECL_STREAM_TYPE = 0,		/* Must be first. */
  LTO_DECL_STREAM_FIELD_DECL,
  LTO_DECL_STREAM_FN_DECL,
  LTO_DECL_STREAM_VAR_DECL,
  LTO_DECL_STREAM_TYPE_DECL,
  LTO_DECL_STREAM_NAMESPACE_DECL,
  LTO_DECL_STREAM_LABEL_DECL,
  LTO_N_DECL_STREAMS
} lto_decl_stream_e_t;

typedef enum ld_plugin_symbol_resolution ld_plugin_symbol_resolution_t;
DEF_VEC_I(ld_plugin_symbol_resolution_t);
DEF_VEC_ALLOC_I(ld_plugin_symbol_resolution_t, heap);

/* Return a char pointer to the start of a data stream for an lto pass
   or function.  The first parameter is the file data that contains
   the information.  The second parameter is the type of information
   to be obtained.  The third parameter is the name of the function
   and is only used when finding a function body; otherwise it is
   NULL.  The fourth parameter is the length of the data returned.  */
typedef const char* (lto_get_section_data_f) (struct lto_file_decl_data *,
					      enum lto_section_type,
					      const char *,
					      size_t *);

/* Return the data found from the above call.  The first three
   parameters are the same as above.  The fourth parameter is the data
   itself and the fifth is the lenght of the data. */
typedef void (lto_free_section_data_f) (struct lto_file_decl_data *,
					enum lto_section_type,
					const char *,
					const char *,
					size_t);

/* Cache of pickled nodes.  Used to avoid writing the same node more
   than once.  The first time a tree node is streamed out, it is
   entered in this cache.  Subsequent references to the same node are
   resolved by looking it up in this cache.

   This is used in two ways:

   - On the writing side, the first time T is added to STREAMER_CACHE,
     a new reference index is created for T and T is emitted on the
     stream.  If T needs to be emitted again to the stream, instead of
     pickling it again, the reference index is emitted.

   - On the reading side, the first time T is read from the stream, it
     is reconstructed in memory and a new reference index created for
     T.  The reconstructed T is inserted in some array so that when
     the reference index for T is found in the input stream, it can be
     used to look up into the array to get the reconstructed T.  */
struct lto_streamer_cache_d
{
  /* The mapping between tree nodes and slots into the nodes array.  */
  htab_t node_map;

  /* Node map to store entries into.  */
  alloc_pool node_map_entries;

  /* Next available slot in the nodes and offsets arrays.  */
  unsigned next_slot;

  /* The nodes pickled so far.  */
  VEC(tree,heap) *nodes;

  /* Offset into the stream where the nodes have been written.  */
  VEC(unsigned,heap) *offsets;
};


/* Structure used as buffer for reading an LTO file.  */
struct lto_input_block
{
  const char *data;
  unsigned int p;
  unsigned int len;
};

#define LTO_INIT_INPUT_BLOCK(BASE,D,P,L)   \
  do {                                     \
    BASE.data = D;                         \
    BASE.p = P;                            \
    BASE.len = L;                          \
  } while (0)

#define LTO_INIT_INPUT_BLOCK_PTR(BASE,D,P,L) \
  do {                                       \
    BASE->data = D;                          \
    BASE->p = P;                             \
    BASE->len = L;                           \
  } while (0)


/* The is the first part of the record for a function or constructor
   in the .o file.  */
struct lto_header
{
  int16_t major_version;
  int16_t minor_version;
  enum lto_section_type section_type;
};

/* The header for a function body.  */
struct lto_function_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Number of labels with names.  */
  int32_t num_named_labels;

  /* Number of labels without names.  */
  int32_t num_unnamed_labels;

  /* Size compressed or 0 if not compressed.  */
  int32_t compressed_size;

  /* Size of names for named labels.  */
  int32_t named_label_size;

  /* Size of the cfg.  */
  int32_t cfg_size;

  /* Size of main gimple body of function.  */
  int32_t main_size;

  /* Size of the string table.  */
  int32_t string_size;
};


/* Structure describing a symbol section.  */
struct lto_decl_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Size of region for decl state. */
  int32_t decl_state_size;

  /* Number of nodes in globals stream.  */
  int32_t num_nodes;

  /* Size of region for expressions, decls, types, etc. */
  int32_t main_size;

  /* Size of the string table.  */
  int32_t string_size;
};


/* Statistics gathered during LTO, WPA and LTRANS.  */
struct lto_stats_d
{
  unsigned HOST_WIDE_INT num_input_cgraph_nodes;
  unsigned HOST_WIDE_INT num_output_cgraph_nodes;
  unsigned HOST_WIDE_INT num_input_files;
  unsigned HOST_WIDE_INT num_output_files;
  unsigned HOST_WIDE_INT num_cgraph_partitions;
  unsigned HOST_WIDE_INT section_size[LTO_N_SECTION_TYPES];
  unsigned HOST_WIDE_INT num_function_bodies;
  unsigned HOST_WIDE_INT num_trees[NUM_TREE_CODES];
  unsigned HOST_WIDE_INT num_output_il_bytes;
  unsigned HOST_WIDE_INT num_compressed_il_bytes;
  unsigned HOST_WIDE_INT num_input_il_bytes;
  unsigned HOST_WIDE_INT num_uncompressed_il_bytes;
};

/* Encoder data structure used to stream callgraph nodes.  */
struct lto_cgraph_encoder_d
{
  /* Map nodes to reference number. */
  struct pointer_map_t *map;

  /* Map reference number to node. */
  VEC(cgraph_node_ptr,heap) *nodes;

  /* Map of nodes where we want to output body.  */
  struct pointer_set_t *body;
};

typedef struct lto_cgraph_encoder_d *lto_cgraph_encoder_t;

/* Return number of encoded nodes in ENCODER.  */

static inline int
lto_cgraph_encoder_size (lto_cgraph_encoder_t encoder)
{
  return VEC_length (cgraph_node_ptr, encoder->nodes);
}


/* Encoder data structure used to stream callgraph nodes.  */
struct lto_varpool_encoder_d
{
  /* Map nodes to reference number. */
  struct pointer_map_t *map;

  /* Map reference number to node. */
  VEC(varpool_node_ptr,heap) *nodes;

  /* Map of nodes where we want to output initializer.  */
  struct pointer_set_t *initializer;
};
typedef struct lto_varpool_encoder_d *lto_varpool_encoder_t;

/* Return number of encoded nodes in ENCODER.  */

static inline int
lto_varpool_encoder_size (lto_varpool_encoder_t encoder)
{
  return VEC_length (varpool_node_ptr, encoder->nodes);
}

/* Mapping from indices to trees.  */
struct GTY(()) lto_tree_ref_table
{
  /* Array of referenced trees . */
  tree * GTY((length ("%h.size"))) trees;

  /* Size of array. */
  unsigned int size;
};


/* Mapping between trees and slots in an array.  */
struct lto_decl_slot
{
  tree t;
  int slot_num;
};


/* The lto_tree_ref_encoder struct is used to encode trees into indices. */

struct lto_tree_ref_encoder
{
  htab_t tree_hash_table;	/* Maps pointers to indices. */
  unsigned int next_index;	/* Next available index. */
  VEC(tree,heap) *trees;	/* Maps indices to pointers. */
};

/* The structure that holds all of the vectors of global types,
   decls and cgraph nodes used in the serialization of this file.  */
struct lto_out_decl_state
{
  /* The buffers contain the sets of decls of various kinds and types we have
     seen so far and the indexes assigned to them.  */
  struct lto_tree_ref_encoder streams[LTO_N_DECL_STREAMS];

  /* Encoder for cgraph nodes.  */
  lto_cgraph_encoder_t cgraph_node_encoder;

  /* Encoder for varpool nodes.  */
  lto_varpool_encoder_t varpool_node_encoder;

  /* If this out-decl state belongs to a function, fn_decl points to that
     function.  Otherwise, it is NULL. */
  tree fn_decl;
};

typedef struct lto_out_decl_state *lto_out_decl_state_ptr;

DEF_VEC_P(lto_out_decl_state_ptr);
DEF_VEC_ALLOC_P(lto_out_decl_state_ptr, heap);

/* One of these is allocated for each object file that being compiled
   by lto.  This structure contains the tables that are needed by the
   serialized functions and ipa passes to connect themselves to the
   global types and decls as they are reconstituted.  */
struct GTY(()) lto_file_decl_data
{
  /* Table of cgraph nodes present in this file.  */
  lto_cgraph_encoder_t GTY((skip)) cgraph_node_encoder;

  /* Table of varpool nodes present in this file.  */
  lto_varpool_encoder_t GTY((skip)) varpool_node_encoder;

  /* The .o file that these offsets relate to.  */
  const char *GTY((skip)) file_name;

  /* Hash table maps lto-related section names to location in file.  */
  htab_t GTY((skip)) section_hash_table;

  /* Hash new name of renamed global declaration to its original name.  */
  htab_t GTY((skip)) renaming_hash_table;

  /* Linked list used temporarily in reader */
  struct lto_file_decl_data *next;

  /* Sub ID for merged objects. */
  unsigned id;

  /* Symbol resolutions for this file */
  VEC(ld_plugin_symbol_resolution_t,heap) * GTY((skip)) resolutions;
};

typedef struct lto_file_decl_data *lto_file_decl_data_ptr;

struct lto_char_ptr_base
{
  char *ptr;
};

/* An incore byte stream to buffer the various parts of the function.
   The entire structure should be zeroed when created.  The record
   consists of a set of blocks.  The first sizeof (ptr) bytes are used
   as a chain, and the rest store the bytes to be written.  */
struct lto_output_stream
{
  /* The pointer to the first block in the stream.  */
  struct lto_char_ptr_base * first_block;

  /* The pointer to the last and current block in the stream.  */
  struct lto_char_ptr_base * current_block;

  /* The pointer to where the next char should be written.  */
  char * current_pointer;

  /* The number of characters left in the current block.  */
  unsigned int left_in_block;

  /* The block size of the last block allocated.  */
  unsigned int block_size;

  /* The total number of characters written.  */
  unsigned int total_size;
};

/* The is the first part of the record in an LTO file for many of the
   IPA passes.  */
struct lto_simple_header
{
  /* The header for all types of sections. */
  struct lto_header lto_header;

  /* Size of main gimple body of function.  */
  int32_t main_size;

  /* Size of main stream when compressed.  */
  int32_t compressed_size;
};

/* A simple output block.  This can be used for simple IPA passes that
   do not need more than one stream.  */
struct lto_simple_output_block
{
  enum lto_section_type section_type;
  struct lto_out_decl_state *decl_state;

  /* The stream that the main tree codes are written to.  */
  struct lto_output_stream *main_stream;
};

/* Data structure holding all the data and descriptors used when writing
   an LTO file.  */
struct output_block
{
  enum lto_section_type section_type;
  struct lto_out_decl_state *decl_state;

  /* The stream that the main tree codes are written to.  */
  struct lto_output_stream *main_stream;

  /* The stream that contains the string table.  */
  struct lto_output_stream *string_stream;

  /* The stream that contains the cfg.  */
  struct lto_output_stream *cfg_stream;

  /* The hash table that contains the set of strings we have seen so
     far and the indexes assigned to them.  */
  htab_t string_hash_table;

  /* The current cgraph_node that we are currently serializing.  Null
     if we are serializing something else.  */
  struct cgraph_node *cgraph_node;

  /* These are the last file and line that were seen in the stream.
     If the current node differs from these, it needs to insert
     something into the stream and fix these up.  */
  const char *current_file;
  int current_line;
  int current_col;

  /* True if writing globals and types.  */
  bool global;

  /* Cache of nodes written in this section.  */
  struct lto_streamer_cache_d *writer_cache;
};


/* Data and descriptors used when reading from an LTO file.  */
struct data_in
{
  /* The global decls and types.  */
  struct lto_file_decl_data *file_data;

  /* All of the labels.  */
  tree *labels;

  /* The string table.  */
  const char *strings;

  /* The length of the string table.  */
  unsigned int strings_len;

  /* Number of named labels.  Used to find the index of unnamed labels
     since they share space with the named labels.  */
  unsigned int num_named_labels;

  /* Number of unnamed labels.  */
  unsigned int num_unnamed_labels;

  const char *current_file;
  int current_line;
  int current_col;

  /* Maps each reference number to the resolution done by the linker. */
  VEC(ld_plugin_symbol_resolution_t,heap) *globals_resolution;

  /* Cache of pickled nodes.  */
  struct lto_streamer_cache_d *reader_cache;
};

/* In lto-symtab.c.  */
extern void lto_symtab_merge_decls (void);
extern void lto_symtab_merge_cgraph_nodes (void);
extern tree lto_symtab_prevailing_decl (tree decl);
extern enum ld_plugin_symbol_resolution lto_symtab_get_resolution (tree decl);
extern void lto_symtab_free (void);

EXTERN_C_END

#endif /* GCC_LTO_STREAMER_H  */
