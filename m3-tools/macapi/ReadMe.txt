Description
-----------

This utility be used to generate Modula-3 interfaces from the Pascal version of the 
Apple Universal Interfaces:

  ftp://ftp.apple.com/developer/Development_Kits/UniversalInterfaces3.4.2.img.bin


Help
----

	usage: [<options>] [<files,...>]

	-outdir <path>		output directory, defaults to the current directory
	-indir <path>			input directory (containing the pascal units), this switch
										only applies when using a config file
	-outint <name>		name of the (single) output module, defaults to 'Mac' (exclude extension)
										
	-config <name>		the name of a configuration file. This file lets you specify input files
										and conditional compilation symbols without using the command line.
	-def <symbol> 		define a conditional compilation symbol
	-dump							write out information about what's been parsed when finished
	-overrides <path>	name of a file of override definitions.

Example
-------

The demo directory contains a sample override and configuration file. Try the following command:

	macapi -dump -config config.txt -overrides overrides.txt -indir <source-dir>

where <source-dir> is where you have placed the Apple Universal Interface files.

Limitations
-----------
- doesn't handle variant records (RECORD types with a CASE clause)
- doesn't handle members of an enumeration being refered to in a declaration
- only handles subranges correctly if the left value of the subrange is not a constant expression
  (handles signed integter and identifiers only)
- Doesn't handle inequalities in conditional compilation expressions (this needs to be fixed)
- does not recursively parse include files and doesn't support seperate output modules (this
  will be fixed in a future release)

Notes
-----
- RAVE.p has redeclarations and must be edited to parse
- M3 does not allow an identifier to begin with an underscore, so such idents are renamed a_<ident>
- The Mac interface files have mac line endings, but the utility will write Unix ones, 
  this may confuse reported lines numbers (the interface compiles OK, though)

