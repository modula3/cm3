struct lang_identifier
{
  struct tree_identifier ignore;
};

typedef enum
{
  T_word_8,  T_int_8,
  T_word_16, T_int_16,
  T_word_32, T_int_32,
  T_word_64, T_int_64,
  T_reel, T_lreel, T_xreel,
  T_addr, T_struct, T_void,
  T_word, T_int,
  T_LAST
}
m3_type;

enum m3_tree_index
{
  /* Types. */
  M3TI_ADDR,
  M3TI_WORD,
  M3TI_INT,
  M3TI_REEL,
  M3TI_LREEL,
  M3TI_XREEL,
  M3TI_INT_8,
  M3TI_INT_16,
  M3TI_INT_32,
  M3TI_INT_64,
  M3TI_WORD_8,
  M3TI_WORD_16,
  M3TI_WORD_32,
  M3TI_WORD_64,
  M3TI_VOID,

  /* Values. */
  M3TI_ZERO,
  M3TI_ONE,
  M3TI_NULL,

  /* Procedures. */
  M3TI_MEMCPY,
  M3TI_MEMMOVE,
  M3TI_MEMSET,
  M3TI_DIV,
  M3TI_MOD,
  M3TI_SET_UNION,
  M3TI_SET_DIFF,
  M3TI_SET_INTER,
  M3TI_SET_SDIFF,
  M3TI_SET_EQ,
  M3TI_SET_NE,
  M3TI_SET_GT,
  M3TI_SET_GE,
  M3TI_SET_LT,
  M3TI_SET_LE,
  M3TI_SET_MEMBER,
  M3TI_SET_RANGE,
  M3TI_SET_SING,
  M3TI_FAULT_PROC,
  M3TI_FAULT_HANDLER,

  /* Miscellaneous. */
  M3TI_GLOBAL_VARS,
  M3TI_DEBUG_FIELDS,
  M3TI_CURRENT_BLOCK,
  M3TI_CURRENT_RECORD_TYPE,
  M3TI_CURRENT_RECORD_VALS,
  M3TI_CURRENT_SEGMENT,
  M3TI_FAULT_INTF,
  M3TI_PENDING_BLOCKS,

  M3TI_MAX
};

extern tree m3_global_trees[M3TI_MAX];

#define t_addr		m3_global_trees[M3TI_ADDR]
#define t_word		m3_global_trees[M3TI_WORD]
#define t_int		m3_global_trees[M3TI_INT]
#define t_reel		m3_global_trees[M3TI_REEL]
#define t_lreel		m3_global_trees[M3TI_LREEL]
#define t_xreel		m3_global_trees[M3TI_XREEL]
#define t_int_8		m3_global_trees[M3TI_INT_8]
#define t_int_16	m3_global_trees[M3TI_INT_16]
#define t_int_32	m3_global_trees[M3TI_INT_32]
#define t_int_64	m3_global_trees[M3TI_INT_64]
#define t_word_8	m3_global_trees[M3TI_WORD_8]
#define t_word_16	m3_global_trees[M3TI_WORD_16]
#define t_word_32	m3_global_trees[M3TI_WORD_32]
#define t_word_64	m3_global_trees[M3TI_WORD_64]
#define t_void		m3_global_trees[M3TI_VOID]

#define v_zero		m3_global_trees[M3TI_ZERO]
#define v_one		m3_global_trees[M3TI_ONE]
#define v_null		m3_global_trees[M3TI_NULL]

#define memcpy_proc	m3_global_trees[M3TI_MEMCPY]
#define memmove_proc	m3_global_trees[M3TI_MEMMOVE]
#define memset_proc	m3_global_trees[M3TI_MEMSET]
#define div_proc	m3_global_trees[M3TI_DIV]
#define mod_proc	m3_global_trees[M3TI_MOD]
#define set_union_proc	m3_global_trees[M3TI_SET_UNION]
#define set_diff_proc	m3_global_trees[M3TI_SET_DIFF]
#define set_inter_proc	m3_global_trees[M3TI_SET_INTER]
#define set_sdiff_proc	m3_global_trees[M3TI_SET_SDIFF]
#define set_eq_proc	m3_global_trees[M3TI_SET_EQ]
#define set_ne_proc	m3_global_trees[M3TI_SET_NE]
#define set_gt_proc	m3_global_trees[M3TI_SET_GT]
#define set_ge_proc	m3_global_trees[M3TI_SET_GE]
#define set_lt_proc	m3_global_trees[M3TI_SET_LT]
#define set_le_proc	m3_global_trees[M3TI_SET_LE]
#define set_member_proc	m3_global_trees[M3TI_SET_MEMBER]
#define set_range_proc	m3_global_trees[M3TI_SET_RANGE]
#define set_sing_proc	m3_global_trees[M3TI_SET_SING]
#define fault_proc	m3_global_trees[M3TI_FAULT_PROC]
#define fault_handler	m3_global_trees[M3TI_FAULT_HANDLER]

#define global_vars	m3_global_trees[M3TI_GLOBAL_VARS]
#define debug_fields	m3_global_trees[M3TI_DEBUG_FIELDS]
#define current_block	m3_global_trees[M3TI_CURRENT_BLOCK]
#define current_record_type	m3_global_trees[M3TI_CURRENT_RECORD_TYPE]
#define current_record_vals	m3_global_trees[M3TI_CURRENT_RECORD_VALS]
#define current_segment	m3_global_trees[M3TI_CURRENT_SEGMENT]
#define fault_intf	m3_global_trees[M3TI_FAULT_INTF]
#define pending_blocks	m3_global_trees[M3TI_PENDING_BLOCKS]

/* Functions defined in m3cg/decl.c */
extern tree m3_build_type PARAMS ((m3_type, int, int));
extern void m3_declare_runtime_functions PARAMS ((void));
extern void m3_init_decl_processing PARAMS ((void));

/* Functions defined in m3cg/tree.c */
extern tree m3_build1 PARAMS ((enum tree_code, tree, tree));
extern tree m3_build2 PARAMS ((enum tree_code, tree, tree, tree));
extern tree m3_build3 PARAMS ((enum tree_code, tree, tree, tree, tree));
extern tree m3_build_int PARAMS ((int));
extern tree m3_build_real PARAMS ((const char *, tree));
extern tree m3_cast PARAMS ((tree, tree));
extern tree m3_do_extract PARAMS ((tree, tree, tree, tree, int));
extern tree m3_do_fixed_extract PARAMS ((tree, int, int, tree, int));
extern tree m3_do_fixed_insert PARAMS ((tree, tree, int, int, tree));
extern tree m3_do_insert PARAMS ((tree, tree, tree, tree, tree));
extern tree m3_do_rotate PARAMS ((tree, tree, int, tree));
extern tree m3_do_shift PARAMS ((tree, tree, int, tree));
extern int m3_is_small_cardinal PARAMS ((tree, HOST_WIDE_INT *));
