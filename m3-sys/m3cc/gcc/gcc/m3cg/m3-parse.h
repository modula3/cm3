typedef char* PSTR;
typedef const char* PCSTR;
typedef signed char SCHAR;
typedef unsigned char UCHAR;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef unsigned HOST_WIDE_INT UWIDE;
typedef HOST_WIDE_INT WIDE;

typedef enum
{
  /* 00 */ T_word_8,
  /* 01 */ T_int_8,
  /* 02 */ T_word_16,
  /* 03 */ T_int_16,
  /* 04 */ T_word_32,
  /* 05 */ T_int_32,
  /* 06 */ T_word_64,
  /* 07 */ T_int_64,
  /* 08 */ T_reel,
  /* 09 */ T_lreel,
  /* 0A */ T_xreel,
  /* 0B */ T_addr,
  /* 0C */ T_struct,
  /* 0D */ T_void,
  /* 0E */ T_word,
  /* 0F */ T_int,
  /* 10 */ T_longword,
  /* 11 */ T_longint,
  /* 12 */ T_LAST
} m3_type;


typedef struct _m3buf_t {
  char buf[256];
} m3buf_t;

#if !GCC45
static bool m3_mark_addressable (tree exp);
#endif
static tree m3_type_for_size (UINT precision, int unsignedp);
static tree m3_type_for_mode (enum machine_mode, int unsignedp);
static tree m3_unsigned_type (tree type_node);
static tree m3_signed_type (tree type_node);
static tree m3_signed_or_unsigned_type (bool unsignedp, tree type);
static alias_set_type m3_get_alias_set (tree);

/* Functions to keep track of the current scope */
static tree pushdecl (tree decl);

/* Langhooks.  */
static tree builtin_function (PCSTR name,
                              tree type,
                              enum built_in_function function_code,
                              enum built_in_class clas);
static tree getdecls (void);
static int global_bindings_p (void);
#if !GCC45
static void insert_block (tree block);
#endif

#if GCC42
static void
m3_expand_function (tree fndecl);
#endif

static tree m3_push_type_decl (tree type, tree name);
static void m3_write_globals (void);

static tree builtin_function (PCSTR name,
                              tree type,
                              enum built_in_function function_code,
                              enum built_in_class clas);

static PCSTR trace_name (PCSTR* inout_name);
static PSTR trace_upper_hex (PSTR format);
static void trace_int (PCSTR name, WIDE val);
static void trace_typeid (PCSTR name, ULONG val);
static void trace_string (PCSTR name, PCSTR result, long length);
static void trace_type (PCSTR name, m3_type type);
static void trace_type_tree (PCSTR name, tree type);
static void trace_float (PCSTR name, UINT kind, long Longs[2]);
static void trace_boolean (PCSTR name, bool val);
static void trace_var (PCSTR name, tree var);
static void trace_proc (PCSTR, tree p);

static WIDE get_int (void);
static UWIDE get_uint (void);
static ULONG get_typeid (void);
static PCSTR scan_string (long length);
static tree scan_calling_convention (void);
static m3_type scan_type (void);
static tree scan_mtype (m3_type* T);
static UINT scan_sign (void);
static tree scan_float (UINT *out_Kind, long Longs[2]);
static bool scan_boolean (void);
static tree scan_var (enum tree_code code);
static tree scan_proc (void);
static tree scan_label (void);

static bool IsHostBigEndian (void);
static varray_type varray_extend (varray_type va, size_t n);

static void format_tag_v (m3buf_t* buf, char kind, ULONG type_id, PCSTR fmt, va_list args);
static void debug_tag (char kind, ULONG type_id, PCSTR fmt, ...);
static void dump_record_type (tree record_type);
static void debug_field_name_length (PCSTR name, size_t length);
static void debug_field_name (PCSTR name);
static void debug_field_id (ULONG type_id);
static void debug_field_fmt_v (ULONG type_id, PCSTR fmt, va_list args);
static void debug_field_fmt (ULONG type_id, PCSTR fmt, ...);
static void debug_struct (void);
static void one_field (UWIDE offset, UWIDE size, tree type, tree *out_f, tree *out_v);
static void one_gap (UWIDE next_offset);

static void m3_gap (UWIDE next_offset);
static void m3_field (PCSTR name, size_t name_length, tree type, UWIDE offset,
                      UWIDE size, tree* out_f, tree* out_v);

static void add_stmt (tree t);
static tree fix_name (PCSTR name, size_t length, ULONG type_id);
static tree declare_temp (tree type);
static tree proc_addr (tree p);
static void m3_start_call (void);
static void m3_pop_param (tree t);
static struct language_function* m3_language_function (void);
static void m3_volatilize_decl (tree decl);
static void m3_volatilize_current_function (void);
static void m3_call_direct (tree p, tree return_type);
static void m3_call_indirect (tree return_type, tree calling_convention);
static void m3_swap (void);
static tree m3_deduce_field_reference (PCSTR caller, tree value, UWIDE offset,
                                       tree field_treetype, m3_type field_m3type);
static bool m3_type_match (tree t1, tree t2);
static bool m3_type_mismatch (tree t1, tree t2);
static void m3_load_1 (tree v, UWIDE offset, tree src_t, m3_type src_T,
                       tree dst_t, m3_type dst_T, bool volatil);
static void m3_load (tree v, UWIDE offset, tree src_t, m3_type src_T,
                     tree dst_t, m3_type dst_T);
static void m3_store_1 (tree v, UWIDE offset, tree src_t, m3_type src_T,
                        tree dst_t, m3_type dst_T, bool volatil);
static void m3_store (tree v, UWIDE offset, tree src_t, m3_type src_T,
                      tree dst_t, m3_type dst_T);
static void m3_store_volatile (tree v, UWIDE offset, tree src_t, m3_type src_T,
                               tree dst_t, m3_type dst_T);
static void setop (tree p, WIDE n, int q);
static void setop2 (tree p, int q);

static PCSTR mode_to_string (enum machine_mode mode);

static void declare_fault_proc (void);
static void emit_fault_proc (void);
static tree generate_fault (int code);

static void m3_gimplify_function (tree fndecl);

static void m3_declare_record_common (void);
static void m3_declare_pointer_common (PCSTR caller, ULONG my_id, ULONG target_id);
static void m3cg_if_compare (tree type, tree label, enum tree_code o);
static void m3cg_compare (tree src_t, tree dst_t, enum tree_code op);
static void m3_minmax (tree type, int min);
static tree m3cg_set_member_ref (tree type, tree* out_bit_in_word);
static void m3cg_set_compare (UWIDE n, tree type, tree proc);
static tree m3_do_fixed_extract (tree x, WIDE m, WIDE n, tree type);
static void m3cg_fetch_and_op (tree type1, tree type2, enum built_in_function fncode);

static void m3_breakpoint(void);
static void m3_parse_file (int);

static UINT m3_init_options (UINT argc, PCSTR* argv);
static int m3_handle_option (size_t code, PCSTR arg, int value);
static bool m3_post_options (PCSTR* pfilename);
static bool m3_init (void);

static tree m3_build1 (enum tree_code code, tree tipe, tree op0);
static tree m3_build2 (enum tree_code code, tree tipe, tree op0, tree op1);
static tree m3_build3 (enum tree_code code, tree tipe, tree op0, tree op1, tree op2);
static tree m3_cast (tree type, tree op0);
static tree m3_convert (tree type, tree op0);
static tree m3_build_pointer_type (tree a);
static tree m3_build_type_id (m3_type type, UWIDE size, UWIDE align, ULONG type_id);
static tree m3_build_type (m3_type type, UWIDE size, UWIDE align);
static tree m3_do_insert (tree x, tree y, tree i, tree n, tree orig_type);
static tree left_shift (tree t, int i);
static tree m3_do_fixed_insert (tree x, tree y, WIDE i, WIDE n, tree type);
static tree m3_do_extract (tree x, tree i, tree n, tree type);
static tree m3_do_rotate (enum tree_code code, tree orig_type, tree val, tree cnt);
static tree m3_do_shift (enum tree_code code, tree orig_type, tree val, tree count);

/*======================================================= OPTION HANDLING ===*/

static int option_trace_all;
