M3CG (BEGIN_UNIT, UNSIGNED_INTEGER (n))
M3CG (END_UNIT, NOTHING)
M3CG (IMPORT_UNIT, STRING (name, name_length))
M3CG (EXPORT_UNIT, STRING (name, name_length))
M3CG (SET_SOURCE_FILE, STRING (name, name_length))
M3CG (SET_SOURCE_LINE, UNSIGNED_INTEGER (i))
M3CG (DECLARE_TYPENAME,
      TYPEID (my_id)
      STRING (fullname, fullname_length))
M3CG (DECLARE_ARRAY,
      TYPEID (my_id)
      TYPEID (index_id)
      TYPEID (elts_id)
      BITSIZE (size)
      M3CG_EXTRA_FIELDS (tree t;))
M3CG (DECLARE_OPEN_ARRAY,
      TYPEID (my_id)
      TYPEID (elts_id)
      BITSIZE (size)
      M3CG_EXTRA_FIELDS (tree t;))
M3CG (DECLARE_ENUM,
      TYPEID (my_id)
      UNSIGNED_INTEGER (n_elts)
      BITSIZE (size))
M3CG (DECLARE_ENUM_ELT,
      STRING (name, name_length))
M3CG (DECLARE_PACKED,
      TYPEID (my_id)
      BITSIZE (size)
      TYPEID (target_id))
M3CG (DECLARE_RECORD,
      TYPEID (my_id)
      BITSIZE (size)
      UNSIGNED_INTEGER (n_fields)
      M3CG_EXTRA_FIELDS (tree t;))
M3CG (DECLARE_FIELD,
      STRING (name, name_length)
      BITOFFSET (offset)
      BITSIZE (size)
      TYPEID (my_id))
M3CG (DECLARE_SET,
      TYPEID (my_id)
      TYPEID (domain_id)
      BITSIZE (size))
M3CG (DECLARE_SUBRANGE,
      TYPEID (my_id)
      TYPEID (domain_id)
      INTEGER (min)
      INTEGER (max)
      BITSIZE (size))
M3CG (DECLARE_POINTER,
      TYPEID (my_id)
      TYPEID (target_id)
      STRING (brand, brand_length)
      BOOLEAN (traced))
M3CG (DECLARE_INDIRECT,
      TYPEID (my_id)
      TYPEID (target_id))
M3CG (DECLARE_PROCTYPE,
      TYPEID (my_id)
      UNSIGNED_INTEGER (n_formals)
      TYPEID (result_id)
      INTEGER (n_raises) /* can be -1 */
      CALLING_CONVENTION (calling_convention))
M3CG (DECLARE_FORMAL,
      STRING (name, name_length)
      TYPEID (my_id))
M3CG (DECLARE_RAISES,
      STRING (name, name_length))
M3CG (DECLARE_OBJECT,
      TYPEID (my_id)
      TYPEID (super_id)
      STRING (brand, brand_length)
      BOOLEAN (traced)
      UNSIGNED_INTEGER (n_fields)
      UNSIGNED_INTEGER (n_methods)
      BITSIZE (field_size)
      M3CG_EXTRA_FIELDS (tree t;))
M3CG (DECLARE_METHOD,
      STRING (name, name_length)
      TYPEID (my_id))
M3CG (DECLARE_OPAQUE,
      TYPEID (my_id)
      TYPEID (super_id))
M3CG (REVEAL_OPAQUE,
      TYPEID (lhs)
      TYPEID (rhs))
M3CG (DECLARE_EXCEPTION,
      STRING (name, name_length)
      TYPEID (type_id)
      BOOLEAN (raise_proc)
      VAR (base)
      UNSIGNED_INTEGER (offset))
M3CG (SET_RUNTIME_PROC,
      STRING (name, name_length)
      PROC (p))
M3CG (IMPORT_GLOBAL,
      STRING (name, name_length)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      TYPEID (type_id)
      RETURN_VAR (var, VAR_DECL))
M3CG (DECLARE_SEGMENT,
      STRING (name, name_length)
      TYPEID (type_id)
      BOOLEAN (is_const)
      RETURN_VAR (var, VAR_DECL))
M3CG (BIND_SEGMENT,
      VAR (var)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      BOOLEAN (exported)
      BOOLEAN (initialized))
M3CG (DECLARE_GLOBAL,
      STRING (name, name_length)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      TYPEID (type_id)
      BOOLEAN (exported)
      BOOLEAN (initialized)
      RETURN_VAR (var, VAR_DECL))
M3CG (DECLARE_CONSTANT,
      STRING (name, name_length)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      TYPEID (type_id)
      BOOLEAN (exported)
      BOOLEAN (initialized)
      RETURN_VAR (var, VAR_DECL))
M3CG (DECLARE_LOCAL,
      STRING (name, name_length)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      TYPEID (type_id)
      BOOLEAN (in_memory)
      BOOLEAN (up_level)
      FREQUENCY (frequency)
      RETURN_VAR (var, VAR_DECL))
M3CG (DECLARE_PARAM,
      STRING (name, name_length)
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      TYPEID (type_id)
      BOOLEAN (in_memory)
      BOOLEAN (up_level)
      FREQUENCY (frequency)
      RETURN_VAR (var, PARM_DECL))
M3CG (DECLARE_TEMP,
      BYTESIZE (size)
      ALIGNMENT (align)
      TYPE (type)
      BOOLEAN (in_memory)
      RETURN_VAR (var, VAR_DECL))
M3CG (FREE_TEMP,
      VAR (var))
M3CG (BEGIN_INIT,
      VAR (var))
M3CG (END_INIT,
      VAR (var))
M3CG (INIT_INT,
      BYTEOFFSET (offset)
      INTEGER (value)
      MTYPE (type))
M3CG (INIT_PROC,
      BYTEOFFSET (offset)
      PROC (proc))
M3CG (INIT_LABEL,
      BYTEOFFSET (offset)
      LABEL (label))
M3CG (INIT_VAR,
      BYTEOFFSET (offset)
      VAR (var)
      INTEGER (b))
M3CG (INIT_OFFSET,
      BYTEOFFSET (offset)
      VAR (var))
M3CG (INIT_CHARS,
      BYTEOFFSET (offset)
      STRING (s, length))
M3CG (INIT_FLOAT,
      BYTEOFFSET (offset)
      FLOAT (val, fkind))
M3CG (IMPORT_PROCEDURE,
      STRING (name, name_length)
      UNSIGNED_INTEGER (n_params)
      MTYPE (return_type)
      CALLING_CONVENTION (calling_convention)
      PROC (p))
M3CG (DECLARE_PROCEDURE,
      STRING (name, name_length)
      UNSIGNED_INTEGER (n_params)
      MTYPE (return_type)
      LEVEL (lev)
      CALLING_CONVENTION (calling_convention)
      BOOLEAN (exported)
      PROC (parent)
      PROC (p))
M3CG (BEGIN_PROCEDURE,
      PROC (p))
M3CG (END_PROCEDURE,
      PROC (p))
M3CG (BEGIN_BLOCK, NOTHING)
M3CG (END_BLOCK, NOTHING)
M3CG (NOTE_PROCEDURE_ORIGIN,
      PROC (p))
M3CG (SET_LABEL,
      LABEL (label)
      BOOLEAN (barrier))
M3CG (JUMP,
      LABEL (label))
M3CG (IF_TRUE,
      TYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_FALSE,
      TYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_EQ,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_NE,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_GT,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_GE,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_LT,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (IF_LE,
      MTYPE (type)
      LABEL (label)
      FREQUENCY (frequency))
M3CG (CASE_JUMP,
      MTYPE (type)
      UNSIGNED_INTEGER (n)
      M3CG_EXTRA_FIELDS (
        std::vector<tree> labels;
        virtual void read_extended();))
M3CG (EXIT_PROC, MTYPE2 (type, T))
M3CG (LOAD,
      VAR (var)
      BYTEOFFSET (offset)
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T))
M3CG (LOAD_ADDRESS,
      VAR (var)
      UNSIGNED_INTEGER (offset))
M3CG (LOAD_INDIRECT,
      INTEGER (offset) /* can be < 0? */
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T))
M3CG (STORE,
      VAR (var)
      BYTEOFFSET (offset)
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T))
M3CG (STORE_INDIRECT,
      INTEGER (offset) /* can be < 0? */
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T))
M3CG (LOAD_NIL, NOTHING)
M3CG (LOAD_INTEGER,
      MTYPE (type)
      INTEGER (n))
M3CG (LOAD_FLOAT,
      MTYPE (type)
      FLOAT (f, fkind))
M3CG (EQ,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (NE,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (GT,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (GE,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (LT,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (LE,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (ADD, MTYPE (type))
M3CG (SUBTRACT, MTYPE (type))
M3CG (MULTIPLY, MTYPE (type))
M3CG (DIVIDE, MTYPE (type))
M3CG (NEGATE, MTYPE (type))
M3CG (ABS, MTYPE (type))
M3CG (MAX, MTYPE (type))
M3CG (MIN, MTYPE (type))
M3CG (ROUND,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (TRUNC,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (FLOOR,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (CEILING,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (CVT_FLOAT,
      MTYPE (src_t)
      MTYPE (dst_t))
M3CG (DIV,
      MTYPE (type)
      SIGN (a)
      SIGN (b))
M3CG (MOD,
      MTYPE (type)
      SIGN (a)
      SIGN (b))
M3CG (SET_UNION, BYTESIZE (n))
M3CG (SET_DIFFERENCE, BYTESIZE (n))
M3CG (SET_INTERSECTION, BYTESIZE (n))
M3CG (SET_SYM_DIFFERENCE, BYTESIZE (n))
M3CG (SET_MEMBER,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_EQ,
      UNSIGNED_INTEGER (n)
      MTYPE (type))
M3CG (SET_NE,
      UNSIGNED_INTEGER (n)
      MTYPE (type))
M3CG (SET_LT,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_LE,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_GT,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_GE,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_RANGE,
      BYTESIZE (n)
      MTYPE (type))
M3CG (SET_SINGLETON,
      BYTESIZE (n)
      MTYPE (type))
M3CG (NOT, MTYPE (type))
M3CG (AND, MTYPE (type))
M3CG (OR, MTYPE (type))
M3CG (XOR, MTYPE (type))
M3CG (SHIFT, MTYPE (type))
M3CG (SHIFT_LEFT, MTYPE (type))
M3CG (SHIFT_RIGHT, MTYPE (type))
M3CG (ROTATE, MTYPE (type))
M3CG (ROTATE_LEFT, MTYPE (type))
M3CG (ROTATE_RIGHT, MTYPE (type))
M3CG (WIDEN, BOOLEAN (sign))
M3CG (CHOP, NOTHING)
M3CG (EXTRACT,
      MTYPE (type)
      BOOLEAN (sign_extend))
M3CG (EXTRACT_N,
      MTYPE (type)
      BOOLEAN (sign_extend)
      UNSIGNED_INTEGER (count))
M3CG (EXTRACT_MN,
      MTYPE (type)
      BOOLEAN (sign_extend)
      UNSIGNED_INTEGER (offset)
      UNSIGNED_INTEGER (count))
M3CG (INSERT, MTYPE (type))
M3CG (INSERT_N,
      MTYPE (type)
      UNSIGNED_INTEGER (count))
M3CG (INSERT_MN,
      MTYPE (type)
      UNSIGNED_INTEGER (offset)
      UNSIGNED_INTEGER (count))
M3CG (SWAP,
      MTYPE (type1)
      MTYPE (type2))
M3CG (POP, MTYPE (type))
M3CG (COPY_N,
      MTYPE (count_type)
      MTYPE (mem_type)
      BOOLEAN (overlap))
M3CG (COPY,
      UNSIGNED_INTEGER (n)
      MTYPE (type)
      BOOLEAN (overlap))
M3CG (ZERO_N,
      MTYPE (count_type)
      MTYPE (mem_type))
M3CG (ZERO,
      UNSIGNED_INTEGER (n)
      MTYPE (mem_type))
M3CG (LOOPHOLE,
      MTYPE2 (type1, Type1)
      MTYPE2 (type2, Type2))
M3CG (ABORT, INTEGER (code))
M3CG (CHECK_NIL, INTEGER (code))
M3CG (CHECK_LO,
      MTYPE (type)
      INTEGER (a)
      INTEGER (code))
M3CG (CHECK_HI,
      MTYPE (type)
      INTEGER (a)
      INTEGER (code))
M3CG (CHECK_RANGE,
      MTYPE (type)
      INTEGER (a)
      INTEGER (b)
      INTEGER (code))
M3CG (CHECK_INDEX,
      MTYPE (type)
      INTEGER (code))
M3CG (CHECK_EQ,
      MTYPE (type)
      INTEGER (code))
M3CG (ADD_OFFSET, UNSIGNED_INTEGER (n))
M3CG (INDEX_ADDRESS,
      MTYPE (type)
      INTEGER (bytes))
M3CG (START_CALL_DIRECT,
      PROC (p)
      UNSIGNED_INTEGER (level)
      MTYPE (type))
M3CG (CALL_DIRECT,
      PROC (p)
      MTYPE (type))
M3CG (START_CALL_INDIRECT,
      MTYPE (type)
      CALLING_CONVENTION (calling_convention))
M3CG (CALL_INDIRECT,
      MTYPE (type)
      CALLING_CONVENTION (calling_convention))
M3CG (POP_PARAM, MTYPE (type))
M3CG (POP_STRUCT,
      TYPEID (my_id)
      BYTESIZE (size)
      ALIGNMENT (align))
M3CG (POP_STATIC_LINK, NOTHING)
M3CG (LOAD_PROCEDURE, PROC (p))
M3CG (LOAD_STATIC_LINK, PROC (p))
M3CG (COMMENT, STRING (comment, comment_length))
M3CG (STORE_ORDERED,
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T)
      UNSIGNED_INTEGER (order))
M3CG (LOAD_ORDERED,
      MTYPE2 (src_t, src_T)
      MTYPE2 (dst_t, dst_T)
      UNSIGNED_INTEGER (order))
M3CG (EXCHANGE,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (COMPARE_EXCHANGE,
      MTYPE (type1)
      MTYPE (type2)
      MTYPE (return_type)
      UNSIGNED_INTEGER (success)
      UNSIGNED_INTEGER (failure))
M3CG (FENCE, UNSIGNED_INTEGER (order))
M3CG (FETCH_AND_ADD,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (FETCH_AND_SUB,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (FETCH_AND_OR,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (FETCH_AND_AND,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (FETCH_AND_XOR,
      MTYPE (type1)
      MTYPE (type2)
      UNSIGNED_INTEGER (order))
M3CG (WIDECHAR_SIZE,
      UNSIGNED_INTEGER(bitsize))
