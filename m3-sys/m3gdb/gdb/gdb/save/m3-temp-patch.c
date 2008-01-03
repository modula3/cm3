/* For each block in bv that is for a nested procedures, Patch the 
   BLOCK_SUPERBLOCK link to point to the block for the containing
   procedure.     
 */ 

/* name is a string that might be the demangled name of a nested procedure.
   If so, it will be of the form: 
     "<Module>.<Proc>.<InnerProc>.<DeeperProc> . ... . <DeepestProc>",
   where "..." is a metaellipsis. 
   Skip "<Module>.<Proc>" and as many occurences of ".<DeeperProc>" as
   will still leave a ".<DeepestProc>", returning the length of everything 
   skipped.  Return 0 if name is not of this form, with at least one
   inner proc. 
*/
int nested_prefix_len ( char * name) 
    { char * current; 
  
    current = strchr (name, '.'); 
    if (current == 0) return 0;
    current++; 
    current = strchr (current, '.'); 
    if (current == 0) return 0;
    while ( (i = strchr (current + 1 , '.') ) > current) { current = i }; 
    return current - name;
  } 

void
m3_patch_nested_procs 
  ( struct blockvector *bv ) 

  { struct dictionary *dict; 
    struct dict_iterator *iter; 
    struct block *block_ptr;
    int block_no;
    struct symbol *sym; 
    char * name; 
    int prefix_len; 
    char * prefix_copy; 

    /* Fill a dictionary with symbols of functions in the blockvector. */ 
    dict = dict_create_hashed_exandable ( ); 
    for (block_no = FIRST_LOCAL_BLOCK; block_no < BLOCKVECTOR_NBLOCKS (bv); 
         block_no++
        )  
      { block_ptr = BLOCKVECTOR_BLOCK (bv, block_no); 
        if (block_ptr) 
          { sym = BLOCK_FUNCTION (block_ptr); 
            if (sym) { dict_add_symbol (dict, sym); } 
          } 
      } 

    /* Go through the blockvector again, looking for nested functions. */
    for (block_no = FIRST_LOCAL_BLOCK; block_no < BLOCKVECTOR_NBLOCKS (bv); 
         block_no++
        )  
      { block_ptr = BLOCKVECTOR_BLOCK (bv, block_no); 
        if (block_ptr) 
        { sym = BLOCK_FUNCTION (block_ptr);
          if (sym)  
            { name = SYMBOL_SEARCH_NAME (sym); 
              prefix_len = nested_prefix_len (name);  
              if (prefix_len > 0) 
                { prefix_copy = alloca (prefix_len + 1); 
                  memcpy (prefix_copy, name, prefix_len);
                  prefix_copy[prefix_len] = '\0';
                  sym = dict_iter_name_first (dict, prefix_copy, iter);  
                  if (sym) 
                    { BLOCK_SUPERBLOCK (block_ptr) = SYMBOL_BLOCK (sym); } 
                } 
            } 
        } 
      } 
    dict_free (dict); 
  } 
