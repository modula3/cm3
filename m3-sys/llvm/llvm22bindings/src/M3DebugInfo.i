%module M3DebugInfo

// Bindings for LLVM Debuginfo version 22.0

%pragma(modula3) unsafe="true";

%{
#include "DebugInfo.h"

using namespace llvm;
%}

%insert(m3rawintf) %{
IMPORT Word;
%}

%insert(m3wrapimpl) %{
IMPORT Word;
FROM LLVM IMPORT ModuleRef,BuilderRef,ContextRef,ValueRef,BasicBlockRef,DbgRecordRef;
FROM LLVMTypes IMPORT MetadataRef,int64_t,uint64_t,uint32_t,unsigned;
%}

%insert(m3wrapintf) %{
IMPORT Word;
IMPORT LLVMTypes;
FROM LLVMTypes IMPORT MetadataRef,int64_t,uint64_t,uint32_t,unsigned;
FROM LLVM IMPORT ModuleRef,BuilderRef,ContextRef,ValueRef,BasicBlockRef,DbgRecordRef;

%}

#define LLVM_C_EXTERN_C_BEGIN
#define LLVM_C_EXTERN_C_END
#define LLVM_C_ABI

//define a macro to handle enumeration parms and returns.

%define DoEnum(PARM1)

//need to convert the int to an enumeration
%typemap("m3wraprettype") PARM1 %{PARM1%}
%typemap("m3wrapretvar")  PARM1 %{ret : C.int;%}
%typemap("m3wrapretraw")  PARM1 %{ret%}
%typemap("m3wrapretconv") PARM1 %{VAL(ret,PARM1)%}
%typemap("m3wrapargvar")  PARM1 %{$1tmp : C.int;%}
%typemap("m3wrapinconv")  PARM1 %{$1tmp := ORD($1_name);%}
%typemap("m3wrapargraw")  PARM1 %{$1tmp%}

%typemap("m3rawrettype")  PARM1 %{C.int%}
//this one deletes the comment of which enum it is
//%typemap("m3rawintype")  PARM1 %{C.int%}

%enddef

//define a macro for most of our opaque conversions
%define DoRefs(PARM1,PARM2)

%typemap("m3rawintype")   PARM1 %{ADDRESS%}
%typemap("m3wrapintype")  PARM1 %{PARM2%}
%typemap("m3rawrettype")  PARM1 %{ADDRESS%}
%typemap("m3wraprettype") PARM1 %{PARM2%}

%enddef

//define a macro for conversions with pointers
%define DoPtrs(PARM1,PARM2)

%typemap("m3rawintype")   PARM1 * %{ADDRESS%}
%typemap("m3wrapintype")  PARM1 * %{UNTRACED REF PARM2%}
%typemap("m3rawrettype")  PARM1 * %{ADDRESS%}
%typemap("m3wraprettype") PARM1 * %{UNTRACED REF PARM2%}
%typemap("m3rawinmode")   PARM1 * %{%}
%typemap("m3wrapinmode")  PARM1 * %{%}

%enddef

DoRefs(LLVMMetadataRef,MetadataRef);
DoPtrs(LLVMMetadataRef,MetadataRef);

DoRefs(LLVMModuleRef,ModuleRef);
DoRefs(LLVMDIBuilderRef,BuilderRef);
DoRefs(LLVMContextRef,ContextRef);
DoRefs(LLVMValueRef,ValueRef);
DoRefs(LLVMBasicBlockRef,BasicBlockRef);
DoRefs(LLVMDbgRecordRef,DbgRecordRef);

//enums
DoEnum(LLVMDWARFSourceLanguage);
DoEnum(LLVMDIFlags);
DoEnum(LLVMDWARFEmissionKind);
DoEnum(LLVMDWARFMacinfoRecordType);
DoEnum(LLVMChecksumKind);

// ***  basic typemaps ***
//size_t
//size_t is unsigned and is max for arch I think so is not unsigned_int which
//is 32 bits
%typemap("m3rawintype")     size_t     %{Word.T%}
%typemap("m3wrapintype")    size_t     %{Word.T%}

//size_t *
%typemap("m3rawintype")     size_t *    %{Word.T%}
%typemap("m3wrapintype")    size_t *    %{Word.T%}
%typemap("m3rawinmode")     size_t *    %{VAR%}
%typemap("m3wrapinmode")    size_t *    %{VAR%}

//unsigned *
%typemap("m3rawintype")     unsigned *  %{C.unsigned_int%}
%typemap("m3wrapintype")    unsigned *  %{uint32_t%}
%typemap("m3rawinmode")     unsigned *  %{VAR%}
%typemap("m3wrapinmode")    unsigned *  %{VAR%}

//LLBMBool
%typemap("m3rawintype")   LLVMBool %{BOOLEAN%}
%typemap("m3wrapintype")  LLVMBool %{BOOLEAN%}
%typemap("m3rawrettype")  LLVMBool %{BOOLEAN%}
%typemap("m3wraprettype") LLVMBool %{BOOLEAN%}

//similar
%apply LLVMBool {LLVMBool *}
%typemap("m3rawinmode")   LLVMBool * %{VAR%}
%typemap("m3wrapinmode")  LLVMBool * %{VAR%}

//unsigned
%typemap("m3rawintype")     unsigned   %{C.unsigned_int%}
%typemap("m3wrapintype")    unsigned   %{unsigned%}
%typemap("m3rawrettype")    unsigned   %{C.unsigned_int%}
%typemap("m3wraprettype")   unsigned   %{unsigned%}

//uint16_t
%typemap("m3rawintype")     uint16_t   %{C.unsigned_short_int%}
%typemap("m3rawintype")     int16_t    %{C.short_int%}
%typemap("m3wrapintype")    int16_t    %{int16_t%}
%typemap("m3wrapintype")    uint16_t   %{uint16_t%}

%typemap("m3rawrettype")    uint16_t    %{C.unsigned_short_int%}
%typemap("m3wraprettype")   uint16_t    %{INTEGER%}
%typemap("m3rawrettype")    int16_t     %{C.short_int%}
%typemap("m3wraprettype")   int16_t     %{int16_t%}

//int32_t and uint32_t
%typemap("m3rawintype")     uint32_t   %{C.unsigned_int%}
%typemap("m3rawintype")     int32_t    %{C.int%}
%typemap("m3wrapintype")    int32_t    %{int32_t%}
%typemap("m3wrapintype")    uint32_t   %{uint32_t%}

%typemap("m3rawrettype")    uint32_t    %{C.unsigned_int%}
%typemap("m3wraprettype")   uint32_t    %{uint32_t%}
%typemap("m3rawrettype")    int32_t    %{C.int%}
%typemap("m3wraprettype")   int32_t    %{int32_t%}

%typemap(in) int32_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint32_t %{ $1 = ($1_ltype)$input; %}

//int64_t and uint64_t
// from core
%typemap("m3rawintype")     uint64_t   %{C.unsigned_long_long%}
%typemap("m3rawintype")     int64_t    %{C.long_long%}
%typemap("m3rawrettype")    uint64_t   %{C.unsigned_long_long%}
%typemap("m3rawrettype")    int64_t    %{C.long_long%}
%typemap("m3wrapintype")    int64_t    %{int64_t%}
%typemap("m3wrapintype")    uint64_t   %{uint64_t%}
%typemap("m3wraprettype")   int64_t    %{int64_t%}
%typemap("m3wraprettype")   uint64_t   %{uint64_t%}

%apply uint64_t {unsigned long long}
%apply int64_t  {long long}

%typemap(in) int64_t  %{ $1 = ($1_ltype)$input; %}
%typemap(in) uint64_t %{ $1 = ($1_ltype)$input; %}


//dont think these are used
%typemap("m3rawinmode")     int64_t  *   %{%}
%typemap("m3wrapinmode")    int64_t  *   %{%}
%typemap("m3rawintype")     int64_t  *   %{UNTRACED REF C.long_long%}
%typemap("m3wrapintype")    int64_t  *   %{UNTRACED REF int64_t%}

//function CreateExpression - needs this group 
%typemap("m3rawinmode")     uint64_t *   %{%}
%typemap("m3wrapinmode")    uint64_t *   %{%}
%typemap("m3rawintype")     uint64_t *   %{UNTRACED REF C.unsigned_long_long%}
%typemap("m3wrapintype")    uint64_t *   %{UNTRACED REF uint64_t%}


//rename to remove LLVMDIBuilder prefix and camelcase the name
//This prefix is the most common.
%rename("%(camelcase)s",sourcefmt="%(regex:/LLVMDIBuilder(.*)/\\1/)s",%$isfunction) ""; 

//this removes the LLVM but leaves a lot of other prefixes which are tricky
//to remove
//rename to remove LLVM prefix and camelcase the name
//%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isfunction) ""; 
//this works with 2 as capture group but still leaves llvm prefix on some
//funcs
//%rename("%(camelcase)s",sourcefmt="%(regex:/(LLVMDIType|LLVMDIBuilder)(.*)/\\2/)s",%$isfunction) ""; 


//renames the enum but not its uses has problems
//%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isenum) "";

//renames the enum item
%rename("%(camelcase)s",sourcefmt="%(regex:/LLVM(.*)/\\1/)s",%$isenumitem) "";

//removes the Ctypes import from the safe interface
%typemap("m3wrapretvar:import")  char * ""

%ignore LLVMDIBuilderCreateEnumeratorOfArbitraryPrecision;

//A list of ignores for all exported methods helps development. 
//Generated via swig -I/usr/local/include/llvm-c -modula3 -generateignores -c++ M3DebugInfo.i
//Once the list is generated paste it into this file.
//Useful where you ignore everything except the method you are working
//on and want to cut the error list down.


%include <DebugInfo.h>
