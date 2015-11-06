
(* File: M3DIBuilder.i3 --- Debug Information Builder -----*- Modula3 -*-===//*)
(* Derived from:                                                              *)
(*===--- llvm/DIBuilder.h - Debug Information Builder ---------*- C++ -*-===//*)
(**)
(*                     The LLVM Compiler Infrastructure*)
(**)
(* This file is distributed under the University of Illinois Open Source*)
(* License. See LICENSE.TXT for details.*)
(**)
(*===--------------------------------------------------------------------===//*)
(**)
(* This file defines a DIBuilder that is useful for creating debugging*)
(* information entries in LLVM IR form.*)
(**)
(*===--------------------------------------------------------------------===//*)

<*EXTERNAL*> INTERFACE M3DIBuilder

(*
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ValueHandle.h"
*)

; IMPORT LLVMTypes 
; FROM LLVMTypes IMPORT int64_t , uint64_t , unsigned 
; FROM LLVMTypes IMPORT Bool , False , True   
; FROM LLVMTypes IMPORT StringRef , StringRefEmpty 
(* All parameters of type StringRef are passed READONLY, which means the
   M3 compiler will pass them by reference.  This is necessary, because the
   parameter convention for passing a two-word RECORD by VALUE differs between
   the M3 backend (derived from gcc 4.7 or older) and gcc 4.8.1, and possibly, 
   llvm. *) 
; FROM LLVMTypes IMPORT ArrayRefOfMetadataRef , ArrayRefOfint64_t 
; FROM LLVMTypes IMPORT MetadataRef , MDNodeRef , FunctionRef , InstructionRef   
; IMPORT LLVM 

(* The DIBuilder itself.  In llvm C++ code, this is a class, with the 
   various debug info node constructors as member function. 
*) 

; TYPE DIBuilderRef = UNTRACED BRANDED "M3DIBOpaqueDIBuilder" REF Opaque

; PROCEDURE DIBBuilderCreate 
    ( Module : LLVM . ModuleRef ; AllowUnresolved : Bool := True ) 
  : DIBuilderRef
  (* Return a new, initialized builder. *) 

(** finalize - Construct any deferred debug info descriptors.*)
; PROCEDURE DIBfinalize ( Builder : DIBuilderRef )

(* C++ pointer-to-MDNode types, all treated as opaque here. *)

; TYPE Opaque = RECORD END

(* There is a large C++ subclass hierarchy rooted at DIDescriptor, whose
   counterparts here are ordered and indented to reflect.  Every one is a 
   class with a single data member, an MDNode*, with varying constraints 
   on the metadata tree it is the root of, the containing class type 
   reflecting this. 

   It is hard to recover this hierarchy in a Modula-3 type hierarchy, while
   working through a C intermediary binding.  It's maybe not feasible at all 
   to recover C++ dynamic types that are proper subtypes of the statically 
   declared result type of a function that returns it.  Also, since Modula-3's 
   object types are always heap allocated, it would probably entail an
   extra layer of heap-allocated wrappers around the C/C++ pointers.  
  
   Moreover, client code of this interface needs to be able to do 
   narrowings (in both directions) among them.  

   Here, they are all given distinct type names, so code can reflect what 
   is known about them, but the types are all equal, thus punting on this 
   issue.  The C++ code in the external C-to-C++ bindings this uses contains 
   runtime checks that convert failed narrowings to nil pointers. *) 

; TYPE LLVMDIDescriptor
       = (* "M3DIBOpaqueDIDescriptor"*) MetadataRef
; TYPE   LLVMDIArray
           = (* "M3DIBOpaqueDIArray"*) MetadataRef
; TYPE   LLVMDITypeArray
           = (* "M3DIBOpaqueDITypeArray"*) MetadataRef
; TYPE   LLVMDISubrange
         = (* "M3DIBOpaqueDISubrange"*) MetadataRef
; TYPE   LLVMDIEnumerator
         = (* "M3DIBOpaqueDIEnumerator"*) MetadataRef
; TYPE   LLVMDIScope
         = (* "M3DIBOpaqueDIScope"*) MetadataRef
; TYPE     LLVMDIType
           = (* "M3DIBOpaqueDIType"*) MetadataRef
; TYPE     LLVMDIFile
           = (* "M3DIBOpaqueDIFile"*) MetadataRef
; TYPE     LLVMDICompileUnit
           = (* "M3DIBOpaqueDICompileUnit"*) MetadataRef
; TYPE     LLVMDISubprogram
           = (* "M3DIBOpaqueDISubprogram"*) MetadataRef
; TYPE     LLVMDILexicalBlock
           = (* "M3DIBOpaqueDILexicalBlock"*) MetadataRef
; TYPE     LLVMDILexicalBlockFile
           = (* "M3DIBOpaqueDILexicalBlockFile"*) MetadataRef
; TYPE     LLVMDINameSpace
           = (* "M3DIBOpaqueDINameSpace"*) MetadataRef
; TYPE       LLVMDIBasicType
             = (* "M3DIBOpaqueDIBasicType"*) MetadataRef
; TYPE       LLVMDIDerivedType
             = (* "M3DIBOpaqueDIDerivedType"*) MetadataRef
; TYPE         LLVMDICompositeType
               = (* "M3DIBOpaqueDICompositeType"*) MetadataRef
; TYPE         LLVMDISubroutineType
               = (* "M3DIBOpaqueDISubroutineType"*) MetadataRef
; TYPE   LLVMDITemplateTypeParameter
         = (* "M3DIBOpaqueDITemplateTypeParameter"*) MetadataRef
; TYPE   LLVMDITemplateValueParameter
         = (* "M3DIBOpaqueDITemplateValueParameter"*) MetadataRef
; TYPE   LLVMDIGlobalVariable
         = (* "M3DIBOpaqueDIGlobalVariable"*) MetadataRef
; TYPE   LLVMDIVariable
         = (* "M3DIBOpaqueDIVariable"*) MetadataRef
; TYPE   LLVMDIExpression
         = (* "M3DIBOpaqueDIExpression"*) MetadataRef
      (* DILocation -- not needed. *)
; TYPE   LLVMDIObjCProperty
         = (* "M3DIBOpaqueDIObjCProperty"*) MetadataRef
; TYPE   LLVMDIImportedEntity
         = (* "M3DIBOpaqueDIImportedEntity"*) MetadataRef

(* These correspond to instances of template class DIRef, and are not derived 
   from DIDescriptor.  For our purposes, each behaves the same, i.e., we have
   a pointer to a metadata node. *) 

; TYPE     LLVMDITypeRef
           = (* "M3DIBOpaqueDITypeRef"*) MetadataRef
; TYPE     LLVMDIScopeRef
           = (* "M3DIBOpaqueDIScopeRef"*) MetadataRef

(* These are now obsolete. **
; TYPE LLVMDIDescriptor = RECORD MDNode : LLVMDIDescriptorRef := NIL END 
; TYPE   LLVMDISubrange = RECORD MDNode : LLVMDISubrangeRef := NIL END 
; TYPE   LLVMDIEnumerator = RECORD MDNode : LLVMDIEnumeratorRef := NIL END 
; TYPE   LLVMDIScope = RECORD MDNode : LLVMDIScopeRef := NIL END 
; TYPE     LLVMDIType = RECORD MDNode : LLVMDITypeRef := NIL END 
; TYPE       LLVMDIBasicType = LLVMDIBasicTypeRef  
(* ; TYPE       LLVMDIBasicType = RECORD MDNode : LLVMDIBasicTypeRef := NIL END *) 
; TYPE       LLVMDIDerivedType = RECORD MDNode : LLVMDIDerivedTypeRef := NIL END 
; TYPE         LLVMDICompositeType = RECORD MDNode : LLVMDICompositeTypeRef := NIL END 
; TYPE         LLVMDISubroutineType = RECORD MDNode : LLVMDISubroutineTypeRef := NIL END 
; TYPE     LLVMDIFile = RECORD MDNode : LLVMDIFileRef := NIL END 
; TYPE     LLVMDICompileUnit = RECORD MDNode : LLVMDICompileUnitRef := NIL END 
; TYPE     LLVMDISubprogram = RECORD MDNode : LLVMDISubprogramRef := NIL END 
; TYPE     LLVMDILexicalBlock = RECORD MDNode : LLVMDILexicalBlockRef := NIL END 
; TYPE     LLVMDILexicalBlockFile = RECORD MDNode : LLVMDILexicalBlockFileRef := NIL END 
; TYPE     LLVMDINameSpace = RECORD MDNode : LLVMDINameSpaceRef := NIL END 
; TYPE   LLVMDITemplateTypeParameter = RECORD MDNode : LLVMDITemplateTypeParameterRef := NIL END 
; TYPE   LLVMDITemplateValueParameter = RECORD MDNode : LLVMDITemplateValueParameterRef := NIL END 
; TYPE   LLVMDIGlobalVariable = RECORD MDNode : LLVMDIGlobalVariableRef := NIL END
; TYPE   LLVMDIVariable = RECORD MDNode : LLVMDIVariableRef := NIL END 
; TYPE   LLVMDIExpression = RECORD MDNode : LLVMDIExpressionRef := NIL END 
; TYPE   LLVMDIObjCProperty = RECORD MDNode : LLVMDIObjCPropertyRef := NIL END 
; TYPE   LLVMDIImportedEntity = RECORD MDNode : LLVMDIImportedEntityRef := NIL END


** *) 

; CONST LLVMDITypeEmpty : LLVMDIType = NIL  
; CONST LLVMDIArrayEmpty : LLVMDIArray = NIL  
; CONST LLVMDIDescriptorEmpty : LLVMDIDescriptor = NIL 
; CONST LLVMDIExpressionEmpty : LLVMDIExpression = NIL  

; TYPE ComplexAddrDom = { OpInvalid , OpPlus , OpDeref }
; TYPE ComplexAddrKind = [ ComplexAddrDom . OpPlus .. ComplexAddrDom . OpDeref ]

(* The procedure bindings to member functions of a DIBuilder class. *) 

; TYPE DebugEmissionKindBase = { Null , FullDebug , LineTablesOnly }
; TYPE DebugEmissionKind 
         = [ DebugEmissionKindBase . FullDebug 
             .. DebugEmissionKindBase . LineTablesOnly 
           ]  

(** createCompileUnit - A CompileUnit provides an anchor for all debugging*)
(** information generated during this instance of compilation.*)
(** @param Lang     Source programming language, eg. dwarf::DW_LANG_C99*)
(** @param File     File name*)
(** @param Dir      Directory*)
(** @param Producer String identify producer of debugging information.*)
(**                 Usually, this is a compiler version string.*)
(** @param isOptimized A boolean flag which indicates whether optimization*)
(**                    is ON or not.*)
(** @param Flags    This string lists command line options. This string is*)
(**                 directly embedded in debug info output which may be used*)
(**                 by a tool analyzing generated debugging information.*)
(** @param RV       This indicates runtime version for languages like*)
(**                 Objective-C.*)
(** @param SplitName The name of the file that we'll split debug info out*)
(**                  into.*)
(** @param Kind     The kind of debug information to generate. *)
(** @param EmitDebugInfo   A boolean flag which indicates whether debug*)
(**                        information should be written to the final*)
(**                        output or not. When this is false, debug*)
(**                        information annotations will be present in*)
(**                        the IL but they are not written to the final*)
(**                        assembly or object file. This supports tracking*)
(**                        source location information in the back end*)
(**                        without actually changing the output (e.g.,*)
(**                        when using optimization remarks).*)
; PROCEDURE DIBcreateCompileUnit
    ( Builder : DIBuilderRef
    ; Lang : unsigned
    ; READONLY File : StringRef
    ; READONLY Dir : StringRef
    ; READONLY Producer : StringRef
    ; isOptimized : Bool
    ; READONLY Flags : StringRef
    ; RV : unsigned
    ; READONLY SplitName : StringRef := StringRefEmpty 
    ; Kind := DebugEmissionKindBase . FullDebug
    ; EmitDebugInfo : Bool := True 
    )
  : LLVMDICompileUnit

(** createFile - Create a file descriptor to hold debugging information*)
(** for a file.*)
; PROCEDURE DIBcreateFile
    ( Builder : DIBuilderRef 
    ; READONLY Filename : StringRef 
    ; READONLY Directory : StringRef 
    )
  : LLVMDIFile

(** createEnumerator - Create a single enumerator value.*)
; PROCEDURE DIBcreateEnumerator
    ( Builder : DIBuilderRef ; READONLY Name : StringRef ; Val : int64_t )
  : LLVMDIEnumerator

(** \brief Create a DWARF unspecified type.*)
; PROCEDURE DIBcreateUnspecifiedType
    ( Builder : DIBuilderRef ; READONLY Name : StringRef )
  : LLVMDIBasicType

(** \brief Create C++11 nullptr type.*)
; PROCEDURE DIBcreateNullPtrType ( ) : LLVMDIBasicType

(** createBasicType - Create debugging information entry for a basic*)
(** type.*)
(** @param Name        Type name.*)
(** @param SizeInBits  Size of the type.*)
(** @param AlignInBits Type alignment.*)
(** @param Encoding    DWARF encoding code, e.g. dwarf::DW_ATE_float.*)
; PROCEDURE DIBcreateBasicType
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Encoding : unsigned
    )
  : LLVMDIBasicType

(** createQualifiedType - Create debugging information entry for a qualified*)
(** type, e.g. 'const int'.*)
(** @param Tag         Tag identifing type, e.g. dwarf::TAG_volatile_type*)
(** @param FromTy      Base Type.*)
; PROCEDURE DIBcreateQualifiedType
    ( Builder : DIBuilderRef ; Tag : unsigned ; FromTy : LLVMDIType )
  : LLVMDIDerivedType

(** createPointerType - Create debugging information entry for a pointer.*)
(** @param PointeeTy   Type pointed by this pointer.*)
(** @param SizeInBits  Size.*)
(** @param AlignInBits Alignment. (optional)*)
(** @param Name        Pointer type name. (optional)*)
; PROCEDURE DIBcreatePointerType
    ( Builder : DIBuilderRef
    ; PointeeTy : LLVMDIType
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t := 0
    ; READONLY Name : StringRef := StringRefEmpty
    )
  : LLVMDIDerivedType

(** \brief Create debugging information entry for a pointer to member.*)
(** @param PointeeTy Type pointed to by this pointer.*)
(** @param Class Type for which this pointer points to members of.*)
; PROCEDURE DIBcreateMemberPointerType
    ( Builder : DIBuilderRef 
    ; PointeeTy : LLVMDIType 
    ; Class : LLVMDIType 
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t := 0 ) 
  : LLVMDIDerivedType

(** createReferenceType - Create debugging information entry for a c++*)
(** style reference or rvalue reference type.*)
; PROCEDURE DIBcreateReferenceType
    ( Builder : DIBuilderRef ; Tag : unsigned ; RTy : LLVMDIType )
  : LLVMDIDerivedType

(** createTypedef - Create debugging information entry for a typedef.*)
(** @param Ty          Original type.*)
(** @param Name        Typedef name.*)
(** @param File        File where this type is defined.*)
(** @param LineNo      Line number.*)
(** @param Context     The surrounding context for the typedef.*)
; PROCEDURE DIBcreateTypedef
    ( Builder : DIBuilderRef
    ; Ty : LLVMDIType
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Context : LLVMDIDescriptor
    )
  : LLVMDIDerivedType

(** createFriend - Create debugging information entry for a 'friend'.*)
; PROCEDURE DIBcreateFriend
    ( Builder : DIBuilderRef ; Ty : LLVMDIType ; FriendTy : LLVMDIType )
  : LLVMDIDerivedType

(** createInheritance - Create debugging information entry to establish*)
(** inheritance relationship between two types.*)
(** @param Ty           Original type.*)
(** @param BaseTy       Base type. Ty is inherits from base.*)
(** @param BaseOffset   Base offset.*)
(** @param Flags        Flags to describe inheritance attribute,*)
(**                     e.g. private*)
; PROCEDURE DIBcreateInheritance
    ( Builder : DIBuilderRef
    ; Ty : LLVMDIType
    ; BaseTy : LLVMDIType
    ; BaseOffset : uint64_t
    ; Flags : unsigned
    )
  : LLVMDIDerivedType

(** createMemberType - Create debugging information entry for a member.*)
(** @param Scope        Member scope.*)
(** @param Name         Member name.*)
(** @param File         File where this member is defined.*)
(** @param LineNo       Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Ty           Parent type.*)
; PROCEDURE DIBcreateMemberType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; Ty : LLVMDIType
    )
  : LLVMDIDerivedType

(** createStaticMemberType - Create debugging information entry for a*)
(** C++ static data member.*)
(** @param Scope      Member scope.*)
(** @param Name       Member name.*)
(** @param File       File where this member is declared.*)
(** @param LineNo     Line number.*)
(** @param Ty         Type of the static member.*)
(** @param Flags      Flags to encode member attribute, e.g. private.*)
(** @param Val        Const initializer of the member.*)
; PROCEDURE DIBcreateStaticMemberType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; Flags : unsigned
    ; Val : LLVM . ConstantRef
    )
  : LLVMDIDerivedType

(** createObjCIVar - Create debugging information entry for Objective-C*)
(** instance variable.*)
(** @param Name         Member name.*)
(** @param File         File where this member is defined.*)
(** @param LineNo       Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Ty           Parent type.*)
(** @param PropertyNode Property associated with this ivar.*)
; PROCEDURE DIBcreateObjCIVar 
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; Ty : LLVMDIType
    ; PropertyNode : MDNodeRef
    )
  : LLVMDIDerivedType

(** createObjCProperty - Create debugging information entry for Objective-C*)
(** property.*)
(** @param Name         Property name.*)
(** @param File         File where this property is defined.*)
(** @param LineNumber   Line number.*)
(** @param GetterName   Name of the Objective C property getter selector.*)
(** @param SetterName   Name of the Objective C property setter selector.*)
(** @param PropertyAttributes Objective C property attributes.*)
(** @param Ty           Type.*)
; PROCEDURE DIBcreateObjCProperty
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; READONLY GetterName : StringRef
    ; READONLY SetterName : StringRef
    ; PropertyAttributes : unsigned
    ; Ty : LLVMDIType
    )
  : LLVMDIObjCProperty

(** createClassType - Create debugging information entry for a class.*)
(** @param Scope        Scope in which this class is defined.*)
(** @param Name         class name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     class members.*)
(** @param VTableHolder Debug info of the base class that contains vtable*)
(**                     for this type. This is used in*)
(**                     DW_AT_containing_type. See DWARF documentation*)
(**                     for more info.*)
(** @param TemplateParms Template type parameters.*)
(** @param UniqueIdentifier A unique identifier for the class.*)
; PROCEDURE DIBcreateClassType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; DerivedFrom : LLVMDIType
    ; Elements : LLVMDIArray
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; TemplateParms : MDNodeRef := NIL 
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** createStructType - Create debugging information entry for a struct.*)
(** @param Scope        Scope in which this struct is defined.*)
(** @param Name         Struct name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     Struct elements.*)
(** @param RunTimeLang  Optional parameter, Objective-C runtime version.*)
(** @param UniqueIdentifier A unique identifier for the struct.*)
; PROCEDURE DIBcreateStructType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Flags : unsigned
    ; DerivedFrom : LLVMDIType
    ; Elements : LLVMDIArray
    ; RunTimeLang : unsigned := 0
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** createUnionType - Create debugging information entry for an union.*)
(** @param Scope        Scope in which this union is defined.*)
(** @param Name         Union name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     Union elements.*)
(** @param RunTimeLang  Optional parameter, Objective-C runtime version.*)
(** @param UniqueIdentifier A unique identifier for the union.*)
; PROCEDURE DIBcreateUnionType
    ( Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Flags : unsigned
    ; Elements : LLVMDIArray
    ; RunTimeLang : unsigned := 0
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** createTemplateTypeParameter - Create debugging information for template*)
(** type parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Type parameter name.*)
(** @param Ty           Parameter type.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateTypeParameter
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateTypeParameter

(** createTemplateValueParameter - Create debugging information for template*)
(** value parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          Constant parameter value.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateValueParameter
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; Val : LLVM . ConstantRef 
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** \brief Create debugging information for a template template parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          The fully qualified name of the template.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateTemplateParameter
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; READONLY Val : StringRef
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** \brief Create debugging information for a template parameter pack.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          An array of types in the pack.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateParameterPack
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; Val : LLVMDIArray
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** createArrayType - Create debugging information entry for an array.*)
(** @param Size         Array size.*)
(** @param AlignInBits  Alignment.*)
(** @param Ty           Element type.*)
(** @param Subscripts   Subscripts.*)
; PROCEDURE DIBcreateArrayType
    ( Builder : DIBuilderRef
    ; Size : uint64_t
    ; AlignInBits : uint64_t
    ; Ty : LLVMDIType
    ; Subscripts : LLVMDIArray
    )
  : LLVMDICompositeType

(** createVectorType - Create debugging information entry for a vector type.*)
(** @param Size         Array size.*)
(** @param AlignInBits  Alignment.*)
(** @param Ty           Element type.*)
(** @param Subscripts   Subscripts.*)
; PROCEDURE DIBcreateVectorType
    ( Builder : DIBuilderRef
    ; Size : uint64_t
    ; AlignInBits : uint64_t
    ; Ty : LLVMDIType
    ; Subscripts : LLVMDIArray
    )
  : LLVMDICompositeType

(** createEnumerationType - Create debugging information entry for an*)
(** enumeration.*)
(** @param Scope          Scope in which this enumeration is defined.*)
(** @param Name           Union name.*)
(** @param File           File where this member is defined.*)
(** @param LineNumber     Line number.*)
(** @param SizeInBits     Member size.*)
(** @param AlignInBits    Member alignment.*)
(** @param Elements       Enumeration elements.*)
(** @param UnderlyingType Underlying type of a C++11/ObjC fixed enum.*)
(** @param UniqueIdentifier A unique identifier for the enum.*)
; PROCEDURE DIBcreateEnumerationType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Elements : LLVMDIArray
    ; UnderlyingType : LLVMDIType
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** createSubroutineType - Create subroutine type.*)
(** @param File           File in which this subroutine is defined.*)
(** @param ParameterTypes An array of subroutine parameter types. This*)
(**                       includes return type at 0th index.*)
(** @param Flags           E.g.: LValueReference.*)
(**                        These flags are used to emit dwarf attributes.*)
; PROCEDURE DIBcreateSubroutineType
    ( Builder : DIBuilderRef 
    ; File : LLVMDIFile 
    ; ParameterTypes : LLVMDITypeArray 
    ; Flags : unsigned := 0 )
  : LLVMDISubroutineType

(** createArtificialType - Create a new procedure with "artificial" flag set.*)
; PROCEDURE DIBcreateArtificialType ( Builder : DIBuilderRef ; Ty : LLVMDIType )
  : LLVMDIType

(** createObjectPointerType - Create a new procedure with the "object pointer"
    flag set.*)
; PROCEDURE DIBcreateObjectPointerType 
    ( Builder : DIBuilderRef ; Ty : LLVMDIType ) 
  : LLVMDIType

(** createForwardDecl - Create a permanent forward-declared type.*)
; PROCEDURE DIBcreateForwardDecl
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; READONLY Name : StringRef
    ; Scope : LLVMDIDescriptor
    ; F : LLVMDIFile
    ; Line : unsigned
    ; RuntimeLang : unsigned := 0
    ; SizeInBits : uint64_t := 0
    ; AlignInBits : uint64_t := 0
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** createReplaceableForwardDecl - Create a temporary forward-declared type.*)
; PROCEDURE DIBcreateReplaceableForwardDecl
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; READONLY Name : StringRef
    ; Scope : LLVMDIDescriptor
    ; F : LLVMDIFile
    ; Line : unsigned
    ; RuntimeLang : unsigned := 0
    ; SizeInBits : uint64_t := 0
    ; AlignInBits : uint64_t := 0
    ; READONLY UniqueIdentifier : StringRef := StringRefEmpty
    )
  : LLVMDICompositeType

(** retainType - Retain DIType in a module even if it is not referenced*)
(** through debug info anchors.*)
; PROCEDURE DIBretainType ( Builder : DIBuilderRef ; T : LLVMDIType )

(** createUnspecifiedParameter - Create unspecified type descriptor*)
(** for a subroutine type.*)
; PROCEDURE DIBcreateUnspecifiedParameter ( ) : LLVMDIBasicType

(** getOrCreateArray - Get a LLVMDIArray, create one if required.*)
; PROCEDURE DIBgetOrCreateArray
    ( Builder : DIBuilderRef 
    ; READONLY Elements : ArrayRefOfMetadataRef 
   (* ^ Pass struct by value; avoid ABI incompatabilities. *) 
    )
  : LLVMDIArray

(** getOrCreateTypeArray - Get a DITypeArray, create one if required.*)
(* NOTE: DIBuilder accepts parameters of this type, but appears not to
         produce values of this type.  Will LLVMDIArray work? *) 
; PROCEDURE DIBgetOrCreateTypeArray
    ( Builder : DIBuilderRef 
    ; READONLY Elements : ArrayRefOfMetadataRef 
   (* ^ Pass struct by value; avoid ABI incompatabilities. *) 
    )
  : LLVMDITypeArray

(** getOrCreateSubrange - Create a descriptor for a value range.  This*)
(** implicitly uniques the values returned.*)
; PROCEDURE DIBgetOrCreateSubrange
    ( Builder : DIBuilderRef ; Lo : int64_t ; Count : int64_t )
  : LLVMDISubrange

(** \brief Create a new descriptor for the specified global.*)
(** @param Context     Variable scope.*) 
(** @param Name        Name of the variable.*)
(** @param LinkageName Mangled variable name.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type.*)
(** @param isLocalToUnit Boolean flag indicate whether this variable is*)
(**                      externally visible or not.*)
(** @param Val         LLVM . ValueRef of the variable.*)
(** @param Decl        Reference to the corresponding declaration.*) 
; PROCEDURE DIBcreateGlobalVariable 
    ( Builder : DIBuilderRef
    ; Context : LLVMDIDescriptor 
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDITypeRef
    ; isLocalToUnit : Bool
    ; Val : LLVM . ConstantRef 
    ; Decl : MDNodeRef 
    )
  : LLVMDIGlobalVariable

(** createTempGlobalVariableFwdDecl - Identical to createGlobalVariable,*)
(** except that the resulting DbgNode is temporary and meant to be RAUWed.*)
; PROCEDURE DIBcreateTempGlobalVariableFwdDecl 
    ( Builder : DIBuilderRef
    ; Context : LLVMDIDescriptor 
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDITypeRef
    ; isLocalToUnit : Bool
    ; Val : LLVM . ConstantRef 
    ; Decl : MDNodeRef 
    )
  : LLVMDIGlobalVariable


(** createLocalVariable - Create a new descriptor for the specified*)
(** local variable.*)
(** @param Tag         Dwarf TAG. Usually DW_TAG_auto_variable or*)
(**                    DW_TAG_arg_variable.*)
(** @param Scope       Variable scope.*)
(** @param Name        Variable name.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type*)
(** @param AlwaysPreserve Boolean. Set to true if debug info for this*)
(**                       variable should be preserved in optimized build.*)
(** @param Flags       Flags, e.g. artificial variable.*)
(** @param ArgNo       If this variable is an argument then this argument's*)
(**                    number. 1 indicates 1st argument.*)
; PROCEDURE DIBcreateLocalVariable
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDITypeRef
    ; AlwaysPreserve : Bool := False
    ; Flags : unsigned := 0
    ; ArgNo : unsigned := 0
    )
  : LLVMDIVariable

(** createExpression - Create a new descriptor for the specifieda*)
(** variable which has a complex address expression for its address.*)
(** @param Addr        An array of complex address operations.*)
; PROCEDURE DIBcreateExpression
    ( Builder : DIBuilderRef
    ; READONLY Addr : ArrayRefOfint64_t
   (* ^ Pass struct by value; avoid ABI incompatabilities. *) 
    )
  : LLVMDIExpression

(** createPieceExpression - Create a descriptor to describe one part*)
(** of aggregate variable that is fragmented across multiple Values.*)
(** @param OffsetInBytes Offset of the piece in bytes.*)
(** @param SizeInBytes   Size of the piece in bytes.*) 
; PROCEDURE DIBcreatePieceExpression
    ( Builder : DIBuilderRef
    ; OffsetInBytes : unsigned 
    ; SizeInBytes : unsigned 
    )
  : LLVMDIExpression

(** createFunction - Create a new descriptor for the specified subprogram.*)
(** See comments in DISubprogram for descriptions of these fields.*)
(** @param Scope         Function scope.*)
(** @param Name          Function name.*)
(** @param LinkageName   Mangled function name.*)
(** @param File          File where this variable is defined.*)
(** @param LineNo        Line number.*)
(** @param Ty            Function type.*)
(** @param isLocalToUnit True if this function is not externally visible.*)
(** @param isDefinition  True if this is a function definition.*)
(** @param ScopeLine     Set to the beginning of the scope this starts*)
(** @param Flags         e.g. is this function prototyped or not.*)
(**                      These flags are used to emit dwarf attributes.*)
(** @param isOptimized   True if optimization is ON.*)
(** @param Fn            llvm::Function pointer.*)
(** @param TParam        Function template parameters.*)
; PROCEDURE DIBcreateFunction
         (* ^'Tho overloaded, not renamed, because its overload 
            (DIBcreateFunctionFromScope) is to be removed.*) 
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIScopeRef
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : Bool
    ; isDefinition : Bool
    ; ScopeLine : unsigned
    ; Flags : unsigned := 0
    ; isOptimized : Bool := False
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** createTempFunctionFwdDecl - Identical to createFunction,*)
(** except that the resulting DbgNode is meant to be RAUWed.*)
; PROCEDURE DIBcreateTempFunctionFwdDecl
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : Bool
    ; isDefinition : Bool
    ; ScopeLine : unsigned
    ; Flags : unsigned := 0
    ; isOptimized : Bool := False
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** FIXME: this is added for dragonegg. Once we update dragonegg*)
(** to call resolve function, this will be removed.*)
; PROCEDURE DIBcreateFunctionFromScope (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIScopeRef
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : Bool
    ; isDefinition : Bool
    ; ScopeLine : unsigned
    ; Flags : unsigned := 0
    ; isOptimized : Bool := False
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** createMethod - Create a new descriptor for the specified C++ method.*)
(** See comments in DISubprogram for descriptions of these fields.*)
(** @param Scope         Function scope.*)
(** @param Name          Function name.*)
(** @param LinkageName   Mangled function name.*)
(** @param File          File where this variable is defined.*)
(** @param LineNo        Line number.*)
(** @param Ty            Function type.*)
(** @param isLocalToUnit True if this function is not externally visible..*)
(** @param isDefinition  True if this is a function definition.*)
(** @param Virtuality    Attributes describing virtualness. e.g. pure*)
(**                      virtual function.*)
(** @param VTableIndex   Index no of this method in virtual table.*)
(** @param VTableHolder  Type that holds vtable.*)
(** @param Flags         e.g. is this function prototyped or not.*)
(**                      This flags are used to emit dwarf attributes.*)
(** @param isOptimized   True if optimization is ON.*)
(** @param Fn            llvm::Function pointer.*)
(** @param TParam        Function template parameters.*)
; PROCEDURE DIBcreateMethod
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : Bool
    ; isDefinition : Bool
    ; Virtuality : unsigned := 0
    ; VTableIndex : unsigned := 0
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; Flags : unsigned := 0
    ; isOptimized : Bool := False
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** createNameSpace - This creates new descriptor for a namespace*)
(** with the specified parent scope.*)
(** @param Scope       Namespace scope*)
(** @param Name        Name of this namespace*)
(** @param File        Source file*)
(** @param LineNo      Line number*)
; PROCEDURE DIBcreateNameSpace
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    )
  : LLVMDINameSpace


(** createLexicalBlockFile - This creates a descriptor for a lexical*)
(** block with a new file attached. This merely extends the existing*)
(** lexical block as it crosses a file.*)
(** @param Scope       Lexical block.*)
(** @param File        Source file.*)
(** @param Discriminator DWARF path discriminator value.*) 
; PROCEDURE DIBcreateLexicalBlockFile
    ( Builder : DIBuilderRef 
    ; Scope : LLVMDIDescriptor 
    ; File : LLVMDIFile 
    ; Discriminator : unsigned := 0 
    )
  : LLVMDILexicalBlockFile

(** createLexicalBlock - This creates a descriptor for a lexical block*)
(** with the specified parent context.*)
(** @param Scope         Parent lexical scope.*)
(** @param File          Source file*)
(** @param Line          Line number*)
(** @param Col           Column number*)
; PROCEDURE DIBcreateLexicalBlock
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; File : LLVMDIFile
    ; Line : unsigned
    ; Col : unsigned
    )
  : LLVMDILexicalBlock

(** \brief Create a descriptor for an imported module.*)
(** @param Context The scope this module is imported into*)
(** @param NS The namespace being imported here*)
(** @param Line Line number*)
; PROCEDURE DIBcreateImportedModuleFromNamespace 
            (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; NS : LLVMDINameSpace
    ; Line : unsigned
    )
  : LLVMDIImportedEntity

(** \brief Create a descriptor for an imported module.*)
(** @param Context The scope this module is imported into*)
(** @param NS An aliased namespace*)
(** @param Line Line number*)
; PROCEDURE DIBcreateImportedModuleFromImportedEntity 
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; NS : LLVMDIImportedEntity
    ; Line : unsigned
    )
  : LLVMDIImportedEntity

(** \brief Create a descriptor for an imported function.*)
(** @param Context The scope this module is imported into*)
(** @param Decl The declaration (or definition) of a function, type, or*)
(**             variable*)
(** @param Line Line number*)
(** @param Name        Imported decl name.*)
; PROCEDURE DIBcreateImportedDeclarationFromDecl
            (* ^Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; Decl : LLVMDIDescriptor
    ; Line : unsigned
    ; READONLY Name : StringRef := StringRefEmpty
    )
  : LLVMDIImportedEntity

(** \brief Create a descriptor for an imported function.*)
(** @param Context The scope this module is imported into*)
(** @param Decl The declaration (or definition) of a function, type, or*)
(**             variable*)
(** @param Line Line number*)
(** @param Name        Imported decl name.*)
; PROCEDURE DIBcreateImportedDeclarationFromImportedEntity
            (* ^Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; NS : LLVMDIImportedEntity 
    ; Line : unsigned
    ; READONLY Name : StringRef := StringRefEmpty
    )
  : LLVMDIImportedEntity

(** insertDeclare - Insert a new llvm.dbg.declare intrinsic call.*)
(** @param Storage     LLVM . ValueRef of the variable*)
(** @param VarInfo     Variable's debug info descriptor.*)
(** @param Expr         A complex location expression.*)
(** @param InsertAtEnd Location for the new intrinsic.*)
; PROCEDURE DIBinsertDeclareAtEnd (* Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Storage : LLVM . ValueRef 
    ; VarInfo : LLVMDIVariable
    ; Expr : LLVMDIExpression 
    ; InsertAtEnd : LLVM . BasicBlockRef
    )
  : InstructionRef

(** insertDeclare - Insert a new llvm.dbg.declare intrinsic call.*)
(** @param Storage      LLVM . ValueRef of the variable*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param Expr         A complex location expression.*)
(** @param InsertBefore Location for the new intrinsic.*)
; PROCEDURE DIBinsertDeclareBefore (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Storage : LLVM . ValueRef 
    ; VarInfo : LLVMDIVariable
    ; Expr : LLVMDIExpression 
    ; InsertBefore : InstructionRef
    )
  : InstructionRef

(** insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.*)
(** @param Val          LLVM . ValueRef of the variable*)
(** @param Offset       Offset*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param Expr         A complex location expression.*)
(** @param InsertAtEnd Location for the new intrinsic.*)
; PROCEDURE DIBinsertDbgValueIntrinsicAtEnd (* Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Val : LLVM . ValueRef
    ; Offset : uint64_t
    ; VarInfo : LLVMDIVariable
    ; Expr : LLVMDIExpression 
    ; InsertAtEnd : LLVM . BasicBlockRef
    )
  : InstructionRef

(** insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.*)
(** @param Val          LLVM . ValueRef of the variable*)
(** @param Offset       Offset*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param Expr         A complex location expression.*)
(** @param InsertBefore Location for the new intrinsic.*)
; PROCEDURE DIBinsertDbgValueIntrinsicBefore (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Val : LLVM . ValueRef 
    ; Offset : uint64_t
    ; VarInfo : LLVMDIVariable
    ; Expr : LLVMDIExpression 
    ; InsertBefore : InstructionRef
    )
  : InstructionRef

(** \brief Replace the vtable holder in the given composite type.*)
(** *)
(** If this creates a self reference, it may orphan some unresolved cycles *)
(** in the operands of \c T, so \a DIBuilder needs to track that. *)
; PROCEDURE DIBreplaceVTableHolder
    ( Builder : DIBuilderRef 
    ; READONLY T : LLVMDICompositeType 
    ; VTableHolder : LLVMDICompositeType 
    )

(** \brief Replace arrays on a composite type. *) 
(** *)
(** If \c T is resolved, but the arrays aren't -- which can happen if \c T *)
(** has a self-reference -- \a DIBuilder needs to track the array to *)
(** resolve cycles. *) 
; PROCEDURE DIBReplaceArrays
    ( Builder : DIBuilderRef 
    ; READONLY T : LLVMDICompositeType  
    ; Elements : LLVMDIArray 
    ; TParems : LLVMDIArray := LLVMDIArrayEmpty  
    ) 

(* This apparently was in bindings/go/llvm/DIBuilderBindings.h of an earlier
   llvm than 3.6.1.  It is in DIBuilder because that is where the stuff needed
   by its implementation is found. *) 
; PROCEDURE DIBgetDebugLoc
    ( Line : unsigned ; Col : unsigned ; Scope : LLVMDIScope )
  : LLVM . ValueRef 

; END M3DIBuilder
.


