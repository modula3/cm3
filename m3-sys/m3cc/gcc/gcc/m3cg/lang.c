#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "m3-parse.h"
#include "m3-tree.h"
#include "toplev.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "flags.h"
#include "ggc.h"

static void m3_finish PARAMS ((void));
static const char *m3_init PARAMS ((const char *));

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "SRC Modula-3"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT m3_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH m3_finish
#undef LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION m3_decode_option

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

static const char *
m3_init (filename)
     const char *filename;
{
  m3_init_parse (filename);
  m3_init_decl_processing ();
  return filename;
}

static void
m3_finish ()
{
  m3_finish_parse ();
}
