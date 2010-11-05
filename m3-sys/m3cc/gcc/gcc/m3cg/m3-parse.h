// INTEGER(x) m3_trace_int (#x, x);
// UNSIGNED_INTEGER(x) m3_trace_int (#x, x;
// STRING(x, length) m3_trace_string (#x, x, length);
// CALLING_CONVENTION(x) m3_trace_calling_convention (#x, x);
// TYPE(x) m3_trace_type (#x, x);
// MTYPE(x) m3_trace_mtype (#x, x);
// MTYPE2(x, y) m3_trace_mtype (#x, #y, x, y);
// SIGN(x) m3_trace_sign (#x, x)
// BYTESIZE(x)  m3_trace_int (#x, x);
// FLOAT(x, fkind) m3_trace_float (#x, x, fkind);
// BOOLEAN(x) m3_trace_boolean (#x, x);
// VAR(x) m3_trace_var (#x, x);
// RETURN_VAR(x, code) m3_trace_var (#x, x);
// PROC(x) m3_trace_proc (#x, x);
// LABEL(x) m3_race_label (#x, x);
// TYPEID(x) m3_trace_typeid (#x, x);

static PCSTR m3_trace_name (PCSTR* inout_name);
static PSTR m3_trace_upper_hex (PSTR format);
static WIDE m3_trace_int (PCSTR name, WIDE val);
static WIDE get_int (void);
static UWIDE get_uint (void);
static ULONG get_typeid (PCSTR name);
static PCSTR scan_string (PCSTR name, long length);
static tree scan_calling_convention (void);
static m3_type scan_type (PCSTR name);
static tree scan_mtype (PCSTR name, m3_type* T);
static UINT scan_sign (void);
static int IsHostBigEndian (void);
static tree scan_float (UINT *out_Kind);
static bool scan_boolean (PCSTR name);
static varray_type varray_extend (varray_type va, size_t n);
static tree scan_var (enum tree_code code, PCSTR name);
static tree scan_proc (void);
static tree scan_label (void);
static void format_tag_v (m3buf_t* buf, char kind, ULONG type_id, PCSTR fmt, va_list args);
static void debug_tag (char kind, ULONG type_id, PCSTR fmt, ...);
static void dump_record_type (tree record_type);
static void debug_field_name_length (PCSTR name, size_t length);
static void debug_field_name (PCSTR name);
static void debug_field_id (ULONG type_id);
static void debug_field_fmt_v (ULONG type_id, PCSTR fmt, va_list args);
static void debug_field_fmt (ULONG type_id, PCSTR fmt, ...);
static void debug_struct (void);
static void one_gap (UWIDE offset);
static void one_field (UWIDE offset, UWIDE size, tree type, tree *out_f, tree *out_v);
static void one_gap (UWIDE next_offset);
static void m3_field (PCSTR name, size_t name_length, tree type, UWIDE offset,
                      UWIDE size, tree*, tree*);
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
static void m3_gimplify_function (tree fndecl);
static void emit_fault_proc (void);
static tree generate_fault (int code);
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
