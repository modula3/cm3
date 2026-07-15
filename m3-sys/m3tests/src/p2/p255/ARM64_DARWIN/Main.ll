; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare ptr @RTHooks__AllocateTracedObj(ptr)
declare ptr @memcpy(ptr, ptr, i64)

define void @Main__Main() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %t77 = alloca i16
  %t76 = alloca i1088
  %t65 = alloca { ptr, i64 }
  %t62 = alloca { ptr, i64 }
  %t60 = alloca i64
  %t57 = alloca { i64 }
  %t54 = alloca { i64 }
  %t52 = alloca { i64 }
  %t49 = alloca { i64 }
  %t47 = alloca { i64 }
  %t44 = alloca { i64 }
  %t37 = alloca i16
  %t36 = alloca i1088
  %t25 = alloca { ptr, i64 }
  %t22 = alloca { ptr, i64 }
  %t20 = alloca i64
  %t17 = alloca { i64 }
  %t14 = alloca { i64 }
  %t12 = alloca { i64 }
  %t9 = alloca { i64 }
  %t7 = alloca { i64 }
  %t4 = alloca { i64 }
  %t1 = load ptr, ptr @tl_obj_-132573217
  %t2 = call ptr @RTHooks__AllocateTracedObj(ptr %t1), !dbg !26
  %t3 = bitcast ptr %t2 to ptr
  %t5 = getelementptr i8, ptr %t4, i64 0
  store i64 3, ptr %t5
  %t6 = load { i64 }, ptr %t4
  %t8 = getelementptr i8, ptr %t7, i64 0
  store i64 3, ptr %t8
  %t10 = getelementptr i8, ptr %t9, i64 0
  store i64 1, ptr %t10
  %t11 = load { i64 }, ptr %t9
  %t13 = getelementptr i8, ptr %t12, i64 0
  store i64 1, ptr %t13
  %t15 = getelementptr i8, ptr %t14, i64 0
  store i64 0, ptr %t15
  %t16 = load { i64 }, ptr %t14
  %t18 = getelementptr i8, ptr %t17, i64 0
  store i64 1, ptr %t18
  %t19 = load [1 x i64], ptr @constarray_0
  %t21 = call ptr @memcpy(ptr %t20, ptr @constarray_1, i64 8), !dbg !26
  %t23 = getelementptr i8, ptr %t22, i64 0
  store ptr %t20, ptr %t23
  %t24 = getelementptr i8, ptr %t22, i64 8
  store i64 1, ptr %t24
  %t26 = getelementptr i8, ptr %t25, i64 0
  store ptr @constarray_1, ptr %t26
  %t27 = getelementptr i8, ptr %t25, i64 8
  store i64 1, ptr %t27
  %t28 = lshr i1088 -1, 1086
  %t29 = shl i1088 -1, 1
  %t30 = and i1088 %t28, %t29
  %t31 = or i1088 0, %t30
  %t32 = lshr i1088 -1, 1086
  %t33 = shl i1088 -1, 1
  %t34 = and i1088 %t32, %t33
  %t35 = or i1088 0, %t34
  store i1088 %t35, ptr %t36
  store i16 2, ptr %t37
  %t38 = bitcast ptr %t3 to ptr
  %t39 = load ptr, ptr %t38
  %t40 = load ptr, ptr %t39
  call void %t40(ptr %t3, { i64 } %t6, ptr %t7, { i64 } %t11, ptr %t12, { i64 } %t16, ptr %t17, [1 x i64] %t19, ptr @constarray_0, ptr %t22, ptr %t25, i1088 %t31, ptr %t36, i16 2, ptr %t37), !dbg !26
  %t41 = load ptr, ptr @tl_obj_-132573217
  %t42 = call ptr @RTHooks__AllocateTracedObj(ptr %t41), !dbg !25
  %t43 = bitcast ptr %t42 to ptr
  %t45 = getelementptr i8, ptr %t44, i64 0
  store i64 3, ptr %t45
  %t46 = load { i64 }, ptr %t44
  %t48 = getelementptr i8, ptr %t47, i64 0
  store i64 3, ptr %t48
  %t50 = getelementptr i8, ptr %t49, i64 0
  store i64 1, ptr %t50
  %t51 = load { i64 }, ptr %t49
  %t53 = getelementptr i8, ptr %t52, i64 0
  store i64 1, ptr %t53
  %t55 = getelementptr i8, ptr %t54, i64 0
  store i64 0, ptr %t55
  %t56 = load { i64 }, ptr %t54
  %t58 = getelementptr i8, ptr %t57, i64 0
  store i64 1, ptr %t58
  %t59 = load [1 x i64], ptr @constarray_0
  %t61 = call ptr @memcpy(ptr %t60, ptr @constarray_1, i64 8), !dbg !25
  %t63 = getelementptr i8, ptr %t62, i64 0
  store ptr %t60, ptr %t63
  %t64 = getelementptr i8, ptr %t62, i64 8
  store i64 1, ptr %t64
  %t66 = getelementptr i8, ptr %t65, i64 0
  store ptr @constarray_1, ptr %t66
  %t67 = getelementptr i8, ptr %t65, i64 8
  store i64 1, ptr %t67
  %t68 = lshr i1088 -1, 1086
  %t69 = shl i1088 -1, 1
  %t70 = and i1088 %t68, %t69
  %t71 = or i1088 0, %t70
  %t72 = lshr i1088 -1, 1086
  %t73 = shl i1088 -1, 1
  %t74 = and i1088 %t72, %t73
  %t75 = or i1088 0, %t74
  store i1088 %t75, ptr %t76
  store i16 2, ptr %t77
  %t78 = bitcast ptr %t43 to ptr
  %t79 = load ptr, ptr %t78
  %t80 = load ptr, ptr %t79
  call void %t80(ptr %t43, { i64 } %t46, ptr %t47, { i64 } %t51, ptr %t52, { i64 } %t56, ptr %t57, [1 x i64] %t59, ptr @constarray_0, ptr %t62, ptr %t65, i1088 %t71, ptr %t76, i16 2, ptr %t77), !dbg !25
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  store i64 1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.1, i64 0)
  %t2 = getelementptr i8, ptr %t1, i64 104
  %t3 = load i64, ptr %t2
  %t4 = add i64 %t3, 1
  store i64 %t4, ptr %t2
  %t5 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t6 = add i64 %t5, 1
  store i64 %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  call void @Main__Main()
  %t7 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.1, i64 0)
  %t8 = getelementptr i8, ptr %t7, i64 112
  %t9 = load i64, ptr %t8
  %t10 = add i64 %t9, 1
  store i64 %t10, ptr %t8
  ret void
}

; CONST array globals
@constarray_0 = private constant [1 x i64] [
  i64 1
]
@constarray_1 = private constant [1 x i64] [
  i64 1
]

; TypeLink globals (MI_type_cell_ptrs chain)
%TypeLink_t = type { ptr, i64 }
@tl_obj_-132573217 = internal global %TypeLink_t {
  ptr null,
  i64 -132573217
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @HighlightVBT_I3(i64)
declare ptr @VBT_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @HighlightVBT_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @VBT_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [8 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr @tl_obj_-132573217,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Main_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Main_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [8 x i8] zeroinitializer  ; user globals (8 bytes)
}
@Main__Main_var_b = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)

define ptr @Main_M3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @Main__Main_M3()
  br label %done
done:
  ret ptr @Main_M3_info
}

; DWARF debug metadata
!16 = distinct !DISubprogram(name: "Main__Main", linkageName: "Main__Main", scope: !4, file: !3, line: 11, type: !6, scopeLine: 11, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DICompositeType(tag: DW_TAG_structure_type, name: "", size: 64, elements: !19)
!19 = !{!20, !21}
!20 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !15, size: 64, offset: 0)
!21 = !DIDerivedType(tag: DW_TAG_member, name: "", baseType: !7, size: 64, offset: 0)
!22 = !DICompositeType(tag: DW_TAG_structure_type, name: "", size: 64, elements: !23)
!23 = !{!24}
!24 = !DIDerivedType(tag: DW_TAG_member, name: "Point_T_field", baseType: !7, size: 64, offset: 0)
!25 = !DILocation(line: 14, column: 0, scope: !16)
!26 = !DILocation(line: 13, column: 0, scope: !16)
!3 = !DIFile(filename: "Main.m3", directory: "..")
!4 = !DINamespace(name: "Main", scope: !2)
!5 = !{null}
!6 = !DISubroutineType(types: !5)
!7 = !DIBasicType(name: "INTEGER", size: 64, encoding: DW_ATE_signed)
!8 = !DIBasicType(name: "CARDINAL", size: 64, encoding: DW_ATE_unsigned)
!9 = !DIBasicType(name: "INTEGER32", size: 32, encoding: DW_ATE_signed)
!10 = !DIBasicType(name: "CARDINAL32", size: 32, encoding: DW_ATE_unsigned)
!11 = !DIBasicType(name: "BOOLEAN", size: 1, encoding: DW_ATE_boolean)
!12 = !DIBasicType(name: "REAL", size: 32, encoding: DW_ATE_float)
!13 = !DIBasicType(name: "LONGREAL", size: 64, encoding: DW_ATE_float)
!14 = !DIBasicType(name: "CHAR", size: 8, encoding: DW_ATE_unsigned_char)
!15 = !DIBasicType(name: "ADDRESS", size: 64, encoding: DW_ATE_address)
!2 = distinct !DICompileUnit(language: DW_LANG_Modula3, file: !3, producer: "CM3 MSIR", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}
