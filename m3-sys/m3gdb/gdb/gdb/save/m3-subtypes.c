/* This is incomplete, having realized it is probably not needed in anything
   like its full generality.  It's sitting around in case some of it turns
   out needed after all. */ 

/* Call this only if left_type is known to be an ordinal type. */ 
static enum subtype_rel
m3_ordinal_subtype_relation ( 
    struct type * left_type, 
    struct type * left_base_type, 
    struct type * right_type
  ) 

  { enum type_code left_base_code; 
    enum type_code right_base_code; 
    struct type * right_base_type;
    LONGEST left_lower; 
    LONGEST left_upper; 
    LONGEST right_lower; 
    LONGEST right_upper; 

    switch ( TYPE_CODE ( right_type ) ) 
      { case TYPE_CODE_M3_SUBRANGE : 
          right_base_type = TYPE_M3_SUBRANGE_TARGET ( right_type ); 
        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          right_base_type = right_type;
        default : 
          return subtype_norel; 
      } 
    left_base_code = TYPE_CODE ( left_base_type ); 
    right_base_code = TYPE_CODE ( right_base_type ); 
    if ( left_base_code != right_base_code ) 
      { return subtype_norel; } 
    if ( left_base_code == TYPE_CODE_M3_ENUM ) 
      { if ( ! m3_type_fields_equal ( left_base_type, right_base_type ) ) 
          { return subtype_norel; } 
      } 
    /* Here, we know both base types are equal. */   
    if ( left_type == left_base_type && right_type == right_base_type ) 
      { return subtype_equal; } 
    /* Here, at least one is a subrange. */ 
    m3_ordinal_bounds ( left_type , & left_lower, & left_upper );  
    m3_ordinal_bounds ( right_type , & right_lower, & right_upper );  
    if ( left_lower == right_lower && left_upper == right_upper ) 
      /* Bounds are equal. */ 
      { if ( left_type != left_base_type && right_type != right_base_type ) 
          /* Both are subranges. */ 
          { return subtype_equal; } 
        else 
          { return subtype_both; }
      }  
    if ( left_lower <= right_lower && left_upper >= right_upper ) 
      { return subtype_super; } /* Left is supertype. */ 
    if ( left_lower >= right_lower && left_upper <= right_upper ) 
      { return subtype_sub; } /* Left is subtype. */ 
    return subtype_norel; 
  } /* m3_ordinal_subtype_relation */ 

static enum subtype_rel 
m3_subtype_relation ( struct type * left, struct type * right ) 

  { struct type * left_direct; 
    struct type * right_direct; 
    enum type_code left_code; 
    enum type_code right_code; 
    enum subtype_relation child_rel; 
    LONGEST lower; 
    LONGEST upper; 

    if ( left == NULL || right == NULL ) { return subtype_norel; } 
    left_direct = m3_direct_type ( left ); 
    right_direct = m3_direct_type ( right ); 

    if ( left_direct == right_direct ) { return subtype_equal; } 
    if ( m3_types_equal ( left_direct, right_direct ) ) { return subtype_equal; }
    left_code = TYPE_CODE ( left_direct ); 
    right_code = TYPE_CODE ( right_direct ); 
    /* Swap operands so that if the type codes differ, left will not be the 
       supertype only. */ 
    if m3_type_code_tier ( left_code ) > m3_type_code_tier ( right_code ) 
      { return reverse_subtype_relation 
                 ( m3_subtype_relation ( right_direct, left_direct ) ); 
      } 

    switch ( left_code ) 
      { case TYPE_CODE_M3_PACKED :
          child_rel 
            = m3_subtype_relation 
                ( TYPE_M3_PACKED_TARGET ( left_direct ) , right_direct ); 
          if ( child_rel == subtype_rel_equal 
               && right_code != TYPE_CODE_M3_PACKED 
             ) 
            { return subtype_both; } 
          else { return child_rel; } 

        case TYPE_CODE_M3_SUBRANGE : 
          return m3_ordinal_subtype_relation 
                   ( left_type, 
                     TYPE_M3_SUBRANGE_TARGET ( left_type ), 
                     lower, upper, right_type 
                   ); 

        case TYPE_CODE_M3_BOOLEAN : 
        case TYPE_CODE_M3_CHAR : 
        case TYPE_CODE_M3_WIDECHAR : 
        case TYPE_CODE_M3_INTEGER : 
        case TYPE_CODE_M3_CARDINAL : 
        case TYPE_CODE_M3_ENUM : 
          return m3_ordinal_subtype_relation 
                   ( left_type, left_type, lower, upper, right_type ); 

        case TYPE_CODE_M3_OPEN_ARRAY : 
        case TYPE_CODE_M3_REFANY : 
        case TYPE_CODE_M3_TRANSIENT_REFANY : 
        case TYPE_CODE_M3_ADDRESS : 
        case TYPE_CODE_M3_PROC : 
        case TYPE_CODE_M3_PROC_CLOSURE :
          return 1;  
        case TYPE_CODE_M3_TEXT : 
        case TYPE_CODE_M3_ARRAY : 
          return 2; 
        case TYPE_CODE_M3_ROOT : 
        case TYPE_CODE_M3_TRANSIENT_ROOT : 
        case TYPE_CODE_M3_UN_ROOT : 
        case TYPE_CODE_M3_POINTER : 
          return 3; 
        case TYPE_CODE_M3_MUTEX : 
          return 4; 
        case TYPE_CODE_M3_OBJECT : 
          return 5; 
        case TYPE_CODE_M3_NULL : 
          return 6; 
        case TYPE_CODE_M3_SET : 
        case TYPE_CODE_M3_OPAQUE : 
        case TYPE_CODE_M3_RECORD : 
        case TYPE_CODE_M3_METHOD : 
        case TYPE_CODE_M3_VOID : 
        default : { return false; } 
      } /* switch ( TYPE_CODE ( right_direct ) ) */ 

  switch ( TYPE_CODE ( left_direct ) ) 
    { case TYPE_CODE_M3_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_OPEN_ARRAY : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_ENUM : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_PACKED :
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SET : 
        return m3_type_fields_equal ( left_direct, right_direct ); 
      case TYPE_CODE_M3_SUBRANGE : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        return 
          TYPE_M3_SUBRANGE_MIN ( left ) == TYPE_M3_SUBRANGE_MIN ( right )  
          && TYPE_M3_SUBRANGE_MAX ( left ) == TYPE_M3_SUBRANGE_MAX ( right );
      case TYPE_CODE_M3_POINTER : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        if ( TYPE_M3_POINTER_TRACED ( left ) != TYPE_M3_POINTER_TRACED ( left ) )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left ) 
             != TYPE_M3_POINTER_BRANDED ( left ) 
           )
          { return false; } 
        if ( TYPE_M3_POINTER_BRANDED ( left ) ) 
          { return TYPE_M3_POINTER_BRAND ( left ) 
                   == TYPE_M3_POINTER_BRAND ( left );
          } 
        return true;  
      case TYPE_CODE_M3_OPAQUE : 
        return m3_type_fields_equal ( left_direct, right_direct ); 

      case TYPE_CODE_M3_RECORD : 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 0; i < TYPE_NFIELDS ( left ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left, i ) 
                 != TYPE_FIELD_NAME ( right, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITPOS ( left, i ) 
                 != TYPE_M3_REC_FIELD_BITPOS ( right, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_REC_FIELD_BITSIZE ( left, i ) 
                 != TYPE_M3_REC_FIELD_BITSIZE ( right, i ) 
               ) 
              { return false; }
          } 
        return true; 
      case TYPE_CODE_M3_OBJECT : 
        if ( TYPE_M3_OBJ_NMETHODS ( left ) != TYPE_M3_OBJ_NMETHODS ( left ) )
          { return false; } 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 1; i < TYPE_NFIELDS ( left ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left, i ) 
                 != TYPE_FIELD_NAME ( right, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_OBJ_FIELD_BITPOS ( left, i ) 
                 != TYPE_M3_OBJ_FIELD_BITPOS ( right, i ) 
               ) 
              { return false; }
            if ( TYPE_M3_OBJ_FIELD_BITSIZE ( left, i ) 
                 != TYPE_M3_OBJ_FIELD_BITSIZE ( right, i ) 
               ) 
              { return false; }
          } 
        if ( TYPE_M3_OBJ_TRACED ( left ) != TYPE_M3_OBJ_TRACED ( left ) )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left ) 
             != TYPE_M3_OBJ_BRANDED ( left ) 
           )
          { return false; } 
        if ( TYPE_M3_OBJ_BRANDED ( left ) ) 
          { return TYPE_M3_OBJ_BRAND ( left ) 
                   == TYPE_M3_OBJ_BRAND ( left );
          } 
        return true;  

      case TYPE_CODE_M3_PROC : 
      case TYPE_CODE_M3_METHOD : 
        if ( TYPE_M3_PROC_NRAISES ( left ) != TYPE_M3_PROC_NRAISES ( left ) )
          { return false; } 
        if ( ! m3_type_fields_equal ( left_direct, right_direct ) ) 
          { return false; } 
        for ( i = 1; i < TYPE_NFIELDS ( left ); i ++ ) 
          { if ( TYPE_FIELD_NAME ( left, i ) 
                 != TYPE_FIELD_NAME ( right, i ) 
               ) 
              { return false; }
          } 
        return true; 

      case TYPE_CODE_M3_ADDRESS : 
      case TYPE_CODE_M3_BOOLEAN : 
      case TYPE_CODE_M3_CHAR : 
      case TYPE_CODE_M3_WIDECHAR : 
      case TYPE_CODE_M3_INTEGER : 
      case TYPE_CODE_M3_CARDINAL : 
      case TYPE_CODE_M3_REFANY : 
      case TYPE_CODE_M3_TRANSIENT_REFANY : 
      case TYPE_CODE_M3_ROOT : 
      case TYPE_CODE_M3_TRANSIENT_ROOT : 
      case TYPE_CODE_M3_UN_ROOT : 
      case TYPE_CODE_M3_MUTEX : 
      case TYPE_CODE_M3_TEXT : 
      case TYPE_CODE_M3_NULL : 
      case TYPE_CODE_M3_VOID : 
        /* There is only one type with each of these code. */ 
        return TYPE_CODE ( left_direct ) == TYPE_CODE ( right_direct ); 
      default : { return false; } 
    } /* switch */ 
  } /* m3_subtype_relation */ 

