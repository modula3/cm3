/* Modula-3: modified */

/* Support for GCC plugin mechanism.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file contains the support for GCC plugin mechanism based on the
   APIs described in doc/plugin.texi.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin.h"
#include "timevar.h"
#include "ggc.h"

EXTERN_C_START

#define GCC_PLUGIN_STRINGIFY0(X) #X
#define GCC_PLUGIN_STRINGIFY1(X) GCC_PLUGIN_STRINGIFY0 (X)

/* Event names as strings.  Keep in sync with enum plugin_event.  */
static const char *plugin_event_name_init[] =
{
# define DEFEVENT(NAME) GCC_PLUGIN_STRINGIFY1 (NAME),
# include "plugin.def"
# undef DEFEVENT
};

/* A printf format large enough for the largest event above.  */
#define FMT_FOR_PLUGIN_EVENT "%-32s"

const char **plugin_event_name = plugin_event_name_init;

/* A hash table to map event names to the position of the names in the
   plugin_event_name table.  */
static htab_t event_tab;

/* Keep track of the limit of allocated events and space ready for
   allocating events.  */
static int event_last = PLUGIN_EVENT_FIRST_DYNAMIC;
static int event_horizon = PLUGIN_EVENT_FIRST_DYNAMIC;

/* Hash table for the plugin_name_args objects created during command-line
   parsing.  */
static htab_t plugin_name_args_tab = NULL;

/* List node for keeping track of plugin-registered callback.  */
struct callback_info
{
  const char *plugin_name;   /* Name of plugin that registers the callback.  */
  plugin_callback_func func; /* Callback to be called.  */
  void *user_data;           /* plugin-specified data.  */
  struct callback_info *next;
};

/* An array of lists of 'callback_info' objects indexed by the event id.  */
static struct callback_info *plugin_callbacks_init[PLUGIN_EVENT_FIRST_DYNAMIC];
static struct callback_info **plugin_callbacks = plugin_callbacks_init;

/* For invoke_plugin_callbacks(), see plugin.h.  */
bool flag_plugin_added = false;

/* Helper function for the hash table that compares the base_name of the
   existing entry (S1) with the given string (S2).  */

static int
htab_str_eq (const void *s1, const void *s2)
{
  const struct plugin_name_args *plugin = (const struct plugin_name_args *) s1;
  return !strcmp (plugin->base_name, (const char *) s2);
}


/* Given a plugin's full-path name FULL_NAME, e.g. /pass/to/NAME.so,
   return NAME.  */

static char *
get_plugin_base_name (const char *full_name)
{
  /* First get the base name part of the full-path name, i.e. NAME.so.  */
  char *base_name = xstrdup (lbasename (full_name));

  /* Then get rid of '.so' part of the name.  */
  strip_off_ending (base_name, strlen (base_name));

  return base_name;
}


/* Create a plugin_name_args object for the given plugin and insert it
   to the hash table. This function is called when
   -fplugin=/path/to/NAME.so or -fplugin=NAME option is processed.  */

void
add_new_plugin (const char* plugin_name)
{
}


/* Parse the -fplugin-arg-<name>-<key>[=<value>] option and create a
   'plugin_argument' object for the parsed key-value pair. ARG is
   the <name>-<key>[=<value>] part of the option.  */

void
parse_plugin_arg_opt (const char *arg)
{
}

/* Register additional plugin information. NAME is the name passed to
   plugin_init. INFO is the information that should be registered. */

static void
register_plugin_info (const char* name, struct plugin_info *info)
{
}

/* Helper function for the event hash table that compares the name of an
   existing entry (E1) with the given string (S2).  */

static int
htab_event_eq (const void *e1, const void *s2)
{
  const char *s1= *(const char * const *) e1;
  return !strcmp (s1, (const char *) s2);
}

void
initialize_plugins (void)
{
}

/* Release memory used by one plugin. */

static int
finalize_one_plugin (void **slot, void * ARG_UNUSED (info))
{
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  XDELETE (plugin);
  return 1;
}

/* Free memory allocated by the plugin system. */

void
finalize_plugins (void)
{
}

/* Used to pass options to htab_traverse callbacks. */

struct print_options
{
  FILE *file;
  const char *indent;
};

/* Print the version of one plugin. */

static int
print_version_one_plugin (void **slot, void *data)
{
  struct print_options *opt = (struct print_options *) data;
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  const char *version = plugin->version ? plugin->version : "Unknown version.";

  fprintf (opt->file, " %s%s: %s\n", opt->indent, plugin->base_name, version);
  return 1;
}

/* Print the version of each plugin. */

void
print_plugins_versions (FILE *file, const char *indent)
{
}

void
print_plugins_help (FILE *file, const char *indent)
{
}


/* Return true if plugins have been loaded.  */

bool
plugins_active_p (void)
{
  return false;
}


/* Dump to FILE the names and associated events for all the active
   plugins.  */

DEBUG_FUNCTION void
dump_active_plugins (FILE *file)
{
}


/* Dump active plugins to stderr.  */

DEBUG_FUNCTION void
debug_active_plugins (void)
{
  dump_active_plugins (stderr);
}

/* Give a warning if plugins are present, before an ICE message asking
   to submit a bug report.  */

void
warn_if_plugins (void)
{
  if (plugins_active_p ())
    {
      fnotice (stderr, "*** WARNING *** there are active plugins, do not report"
	       " this as a bug unless you can reproduce it without enabling"
	       " any plugins.\n");
      dump_active_plugins (stderr);
    }

}

/* Likewise, as a callback from the diagnostics code.  */

void
plugins_internal_error_function (diagnostic_context *context ATTRIBUTE_UNUSED,
				 const char *msgid ATTRIBUTE_UNUSED,
				 va_list *ap ATTRIBUTE_UNUSED)
{
  warn_if_plugins ();
}

/* The default version check. Compares every field in VERSION. */

bool
plugin_default_version_check (struct plugin_gcc_version *gcc_version,
			      struct plugin_gcc_version *plugin_version)
{
  if (!gcc_version || !plugin_version)
    return false;

  if (strcmp (gcc_version->basever, plugin_version->basever))
    return false;
  if (strcmp (gcc_version->datestamp, plugin_version->datestamp))
    return false;
  if (strcmp (gcc_version->devphase, plugin_version->devphase))
    return false;
  if (strcmp (gcc_version->revision, plugin_version->revision))
    return false;
  if (strcmp (gcc_version->configuration_arguments,
	      plugin_version->configuration_arguments))
    return false;
  return true;
}


/* Return the current value of event_last, so that plugins which provide
   additional functionality for events for the benefit of high-level plugins
   know how many valid entries plugin_event_name holds.  */

int
get_event_last (void)
{
  return event_last;
}


/* Retrieve the default plugin directory.  The gcc driver should have passed
   it as -iplugindir <dir> to the cc1 program, and it is queriable thru the
   -print-file-name=plugin option to gcc.  */
const char*
default_plugin_dir_name (void)
{
  if (!plugindir_string)
    fatal_error ("-iplugindir <dir> option not passed from the gcc driver");
  return plugindir_string;
}

EXTERN_C_END
