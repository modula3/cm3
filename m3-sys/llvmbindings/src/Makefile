
# File: m3-sys/llvmbindings/src/Makefile

# Copyright 2015, Rodney M. Bates.
# rodney.m.bates@acm.org
# Licensed under the Gnu Public License, version 2 or later.

# This Makefile builds C++ code supplied in this package,
# from outside of the llvm source tree.  

# This Makefile needs the following variables:  
# LLVM_INCLUDE_DIR    
# LLVM_BUILD_INCLUDE_DIR    


# This Makefile is "executed" with the target-dependent Modula3 build 
# directory as current directory.  

# The .o files we build here are target-dependent and belong in the build
# directory, but cm3's build system is broken in that case.  So for now, 
# put objects in the src directory.  
CPP_BUILD_DIR=../src
#CPP_BUILD_DIR=. 

CPP_COMPILER = gcc 

CPP_MISC_FLAGS = -c -std=c++11 -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 

all : $(CPP_BUILD_DIR)/M3Extras.o 
all : $(CPP_BUILD_DIR)/M3DIBuilder.o 

$(CPP_BUILD_DIR)/M3Extras.o : ../src/M3Extras.h ../src/M3Extras.cpp
	$(CPP_COMPILER) $(CPP_MISC_FLAGS) -I$(LLVM_INCLUDE_DIR) -I$(LLVM_BUILD_INCLUDE_DIR) -o $(CPP_BUILD_DIR)/M3Extras.o ../src/M3Extras.cpp

$(CPP_BUILD_DIR)/M3DIBuilder.o : ../src/M3DIBuilder.h ../src/M3DIBuilder.cpp
	$(CPP_COMPILER) $(CPP_MISC_FLAGS) -I$(LLVM_INCLUDE_DIR) -I$(LLVM_BUILD_INCLUDE_DIR) -o $(CPP_BUILD_DIR)/M3DIBuilder.o ../src/M3DIBuilder.cpp

clean : 
	rm -f $(CPP_BUILD_DIR)/M3Extras.o
	rm -f $(CPP_BUILD_DIR)/M3DIBuilder.o

# End of m3-sys/llvmbindings/src/Makefile

