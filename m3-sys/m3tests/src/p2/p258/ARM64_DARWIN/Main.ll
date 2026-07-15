; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare void @Dump(i64, i64, i64, i64, i64, ptr)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__F31(i64 %a.start, i64 %a.count) personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  %t6 = alloca i1
  %t5 = alloca i1
  %a.slot = alloca i32
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !24, metadata !DIExpression()), !dbg !41
  %end.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %end.slot, metadata !25, metadata !DIExpression()), !dbg !41
  %count.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %count.slot, metadata !26, metadata !DIExpression()), !dbg !41
  %start.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %start.slot, metadata !27, metadata !DIExpression()), !dbg !41
  store i64 %a.start, ptr %start.slot
  store i64 %a.count, ptr %count.slot
  %t1 = load i64, ptr %start.slot
  %t2 = load i64, ptr %count.slot
  %t3 = add i64 %t1, %t2
  %t4 = sub i64 %t3, 1
  store i64 %t4, ptr %end.slot
  store i32 0, ptr %a.slot
  store i1 1, ptr %t5
  store i1 1, ptr %t6
  %t7 = load i64, ptr %end.slot
  %t8 = icmp sgt i64 %t7, 31
  br i1 %t8, label %or.merge.2, label %or.rhs.1
or.rhs.1:
  %t9 = load i64, ptr %end.slot
  %t10 = icmp slt i64 %t9, 0
  store i1 %t10, ptr %t6
  br label %or.merge.2
or.merge.2:
  %t11 = load i1, ptr %t6
  br i1 %t11, label %or.merge.4, label %or.rhs.3
or.rhs.3:
  %t12 = load i64, ptr %start.slot
  %t13 = icmp sgt i64 %t12, 31
  store i1 %t13, ptr %t5
  br label %or.merge.4
or.merge.4:
  %t14 = load i1, ptr %t5
  br i1 %t14, label %if.then.5, label %if.merge.6
if.then.5:
  ret void
if.merge.6:
  %t15 = load i64, ptr %start.slot
  %t16 = load i64, ptr %end.slot
  %t17 = trunc i64 %t15 to i32
  %t18 = trunc i64 %t16 to i32
  %t19 = sub i32 31, %t18
  %t20 = ashr i32 -1, %t19
  %t21 = shl i32 -1, %t17
  %t22 = and i32 %t20, %t21
  %t23 = or i32 0, %t22
  store i32 %t23, ptr %a.slot
  %t24 = load i64, ptr %start.slot
  %t25 = load i64, ptr %count.slot
  call void @Dump(i64 31, i64 %t24, i64 %t25, i64 32, i64 4, ptr %a.slot), !dbg !43
  ret void
}

define void @Main__F32(i64 %a.start, i64 %a.count) personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %t6 = alloca i1
  %t5 = alloca i1
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !28, metadata !DIExpression()), !dbg !45
  %end.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %end.slot, metadata !29, metadata !DIExpression()), !dbg !45
  %count.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %count.slot, metadata !30, metadata !DIExpression()), !dbg !45
  %start.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %start.slot, metadata !31, metadata !DIExpression()), !dbg !45
  store i64 %a.start, ptr %start.slot
  store i64 %a.count, ptr %count.slot
  %t1 = load i64, ptr %start.slot
  %t2 = load i64, ptr %count.slot
  %t3 = add i64 %t1, %t2
  %t4 = sub i64 %t3, 1
  store i64 %t4, ptr %end.slot
  store i64 0, ptr %a.slot
  store i1 1, ptr %t5
  store i1 1, ptr %t6
  %t7 = load i64, ptr %end.slot
  %t8 = icmp sgt i64 %t7, 32
  br i1 %t8, label %or.merge.2, label %or.rhs.1
or.rhs.1:
  %t9 = load i64, ptr %end.slot
  %t10 = icmp slt i64 %t9, 0
  store i1 %t10, ptr %t6
  br label %or.merge.2
or.merge.2:
  %t11 = load i1, ptr %t6
  br i1 %t11, label %or.merge.4, label %or.rhs.3
or.rhs.3:
  %t12 = load i64, ptr %start.slot
  %t13 = icmp sgt i64 %t12, 32
  store i1 %t13, ptr %t5
  br label %or.merge.4
or.merge.4:
  %t14 = load i1, ptr %t5
  br i1 %t14, label %if.then.5, label %if.merge.6
if.then.5:
  ret void
if.merge.6:
  %t15 = load i64, ptr %start.slot
  %t16 = load i64, ptr %end.slot
  %t17 = sub i64 63, %t16
  %t18 = ashr i64 -1, %t17
  %t19 = shl i64 -1, %t15
  %t20 = and i64 %t18, %t19
  %t21 = or i64 0, %t20
  store i64 %t21, ptr %a.slot
  %t22 = load i64, ptr %start.slot
  %t23 = load i64, ptr %count.slot
  call void @Dump(i64 32, i64 %t22, i64 %t23, i64 64, i64 8, ptr %a.slot), !dbg !47
  ret void
}

define void @Main__F63(i64 %a.start, i64 %a.count) personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %t6 = alloca i1
  %t5 = alloca i1
  %a.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !32, metadata !DIExpression()), !dbg !49
  %end.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %end.slot, metadata !33, metadata !DIExpression()), !dbg !49
  %count.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %count.slot, metadata !34, metadata !DIExpression()), !dbg !49
  %start.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %start.slot, metadata !35, metadata !DIExpression()), !dbg !49
  store i64 %a.start, ptr %start.slot
  store i64 %a.count, ptr %count.slot
  %t1 = load i64, ptr %start.slot
  %t2 = load i64, ptr %count.slot
  %t3 = add i64 %t1, %t2
  %t4 = sub i64 %t3, 1
  store i64 %t4, ptr %end.slot
  store i64 0, ptr %a.slot
  store i1 1, ptr %t5
  store i1 1, ptr %t6
  %t7 = load i64, ptr %end.slot
  %t8 = icmp sgt i64 %t7, 63
  br i1 %t8, label %or.merge.2, label %or.rhs.1
or.rhs.1:
  %t9 = load i64, ptr %end.slot
  %t10 = icmp slt i64 %t9, 0
  store i1 %t10, ptr %t6
  br label %or.merge.2
or.merge.2:
  %t11 = load i1, ptr %t6
  br i1 %t11, label %or.merge.4, label %or.rhs.3
or.rhs.3:
  %t12 = load i64, ptr %start.slot
  %t13 = icmp sgt i64 %t12, 63
  store i1 %t13, ptr %t5
  br label %or.merge.4
or.merge.4:
  %t14 = load i1, ptr %t5
  br i1 %t14, label %if.then.5, label %if.merge.6
if.then.5:
  ret void
if.merge.6:
  %t15 = load i64, ptr %start.slot
  %t16 = load i64, ptr %end.slot
  %t17 = sub i64 63, %t16
  %t18 = ashr i64 -1, %t17
  %t19 = shl i64 -1, %t15
  %t20 = and i64 %t18, %t19
  %t21 = or i64 0, %t20
  store i64 %t21, ptr %a.slot
  %t22 = load i64, ptr %start.slot
  %t23 = load i64, ptr %count.slot
  call void @Dump(i64 63, i64 %t22, i64 %t23, i64 64, i64 8, ptr %a.slot), !dbg !51
  ret void
}

define void @Main__F64(i64 %a.start, i64 %a.count) personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %t6 = alloca i1
  %t5 = alloca i1
  %a.slot = alloca i128
  call void @llvm.dbg.declare(metadata ptr %a.slot, metadata !36, metadata !DIExpression()), !dbg !53
  %end.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %end.slot, metadata !37, metadata !DIExpression()), !dbg !53
  %count.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %count.slot, metadata !38, metadata !DIExpression()), !dbg !53
  %start.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %start.slot, metadata !39, metadata !DIExpression()), !dbg !53
  store i64 %a.start, ptr %start.slot
  store i64 %a.count, ptr %count.slot
  %t1 = load i64, ptr %start.slot
  %t2 = load i64, ptr %count.slot
  %t3 = add i64 %t1, %t2
  %t4 = sub i64 %t3, 1
  store i64 %t4, ptr %end.slot
  store i128 0, ptr %a.slot
  store i1 1, ptr %t5
  store i1 1, ptr %t6
  %t7 = load i64, ptr %end.slot
  %t8 = icmp sgt i64 %t7, 64
  br i1 %t8, label %or.merge.2, label %or.rhs.1
or.rhs.1:
  %t9 = load i64, ptr %end.slot
  %t10 = icmp slt i64 %t9, 0
  store i1 %t10, ptr %t6
  br label %or.merge.2
or.merge.2:
  %t11 = load i1, ptr %t6
  br i1 %t11, label %or.merge.4, label %or.rhs.3
or.rhs.3:
  %t12 = load i64, ptr %start.slot
  %t13 = icmp sgt i64 %t12, 64
  store i1 %t13, ptr %t5
  br label %or.merge.4
or.merge.4:
  %t14 = load i1, ptr %t5
  br i1 %t14, label %if.then.5, label %if.merge.6
if.then.5:
  ret void
if.merge.6:
  %t15 = load i64, ptr %start.slot
  %t16 = load i64, ptr %end.slot
  %t17 = zext i64 %t15 to i128
  %t18 = zext i64 %t16 to i128
  %t19 = sub i128 127, %t18
  %t20 = lshr i128 -1, %t19
  %t21 = shl i128 -1, %t17
  %t22 = and i128 %t20, %t21
  %t23 = or i128 0, %t22
  store i128 %t23, ptr %a.slot
  %t24 = load i64, ptr %start.slot
  %t25 = load i64, ptr %count.slot
  call void @Dump(i64 64, i64 %t24, i64 %t25, i64 128, i64 16, ptr %a.slot), !dbg !55
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t18 = alloca i64
  %count.slot = alloca i64
  %offset.slot = alloca i64
  %t11 = alloca [3 x i64]
  %t5 = alloca i64
  %offset_offset.slot = alloca i64
  %t1 = alloca i64
  %offset_base.slot = alloca i64
  store i64 0, ptr %offset_base.slot
  store i64 2, ptr %t1
  br label %for.header.1
for.header.1:
  %t2 = load i64, ptr %offset_base.slot
  %t3 = load i64, ptr %t1
  %t4 = icmp sle i64 %t2, %t3
  br i1 %t4, label %for.body.2, label %for.exit.3
for.body.2:
  store i64 -1, ptr %offset_offset.slot
  store i64 1, ptr %t5
  br label %for.header.4
for.exit.3:
  ret void
for.header.4:
  %t6 = load i64, ptr %offset_offset.slot
  %t7 = load i64, ptr %t5
  %t8 = icmp sle i64 %t6, %t7
  br i1 %t8, label %for.body.5, label %for.exit.6
for.body.5:
  %t9 = load i64, ptr %offset_base.slot
  %t10 = load [3 x i64], ptr @constarray_0
  store [3 x i64] %t10, ptr %t11
  %t12 = getelementptr inbounds [3 x i64], ptr %t11, i64 0, i64 %t9
  %t13 = load i64, ptr %t12
  %t14 = load i64, ptr %offset_offset.slot
  %t15 = add i64 %t13, %t14
  store i64 %t15, ptr %offset.slot
  %t16 = load i64, ptr %offset.slot
  %t17 = icmp sge i64 %t16, 0
  br i1 %t17, label %if.then.7, label %if.merge.8
for.exit.6:
  %t34 = load i64, ptr %offset_base.slot
  %t35 = add i64 %t34, 1
  store i64 %t35, ptr %offset_base.slot
  br label %for.header.1
if.then.7:
  store i64 0, ptr %count.slot
  store i64 2, ptr %t18
  br label %for.header.9
if.merge.8:
  %t32 = load i64, ptr %offset_offset.slot
  %t33 = add i64 %t32, 1
  store i64 %t33, ptr %offset_offset.slot
  br label %for.header.4
for.header.9:
  %t19 = load i64, ptr %count.slot
  %t20 = load i64, ptr %t18
  %t21 = icmp sle i64 %t19, %t20
  br i1 %t21, label %for.body.10, label %for.exit.11
for.body.10:
  %t22 = load i64, ptr %offset.slot
  %t23 = load i64, ptr %count.slot
  call void @Main__F31(i64 %t22, i64 %t23)
  %t24 = load i64, ptr %offset.slot
  %t25 = load i64, ptr %count.slot
  call void @Main__F32(i64 %t24, i64 %t25)
  %t26 = load i64, ptr %offset.slot
  %t27 = load i64, ptr %count.slot
  call void @Main__F63(i64 %t26, i64 %t27)
  %t28 = load i64, ptr %offset.slot
  %t29 = load i64, ptr %count.slot
  call void @Main__F64(i64 %t28, i64 %t29)
  %t30 = load i64, ptr %count.slot
  %t31 = add i64 %t30, 1
  store i64 %t31, ptr %count.slot
  br label %for.header.9
for.exit.11:
  br label %if.merge.8
}

; CONST array globals
@constarray_0 = private constant [3 x i64] [
  i64 0,
  i64 32,
  i64 64
]

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Dump_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Dump_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @Main_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Main_M3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

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
!16 = distinct !DISubprogram(name: "Main__F31", linkageName: "Main__F31", scope: !4, file: !3, line: 11, type: !6, scopeLine: 11, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__F32", linkageName: "Main__F32", scope: !4, file: !3, line: 25, type: !6, scopeLine: 25, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__F63", linkageName: "Main__F63", scope: !4, file: !3, line: 37, type: !6, scopeLine: 37, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__F64", linkageName: "Main__F64", scope: !4, file: !3, line: 49, type: !6, scopeLine: 49, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!23 = !DILocation(line: 0, column: 0, scope: !22)
!24 = !DILocalVariable(name: "a", scope: !16, file: !3, line: 11, type: !9)
!25 = !DILocalVariable(name: "end", scope: !16, file: !3, line: 11, type: !7)
!26 = !DILocalVariable(name: "count", scope: !16, file: !3, line: 11, type: !7)
!27 = !DILocalVariable(name: "start", scope: !16, file: !3, line: 11, type: !7)
!28 = !DILocalVariable(name: "a", scope: !18, file: !3, line: 25, type: !7)
!29 = !DILocalVariable(name: "end", scope: !18, file: !3, line: 25, type: !7)
!30 = !DILocalVariable(name: "count", scope: !18, file: !3, line: 25, type: !7)
!31 = !DILocalVariable(name: "start", scope: !18, file: !3, line: 25, type: !7)
!32 = !DILocalVariable(name: "a", scope: !20, file: !3, line: 37, type: !7)
!33 = !DILocalVariable(name: "end", scope: !20, file: !3, line: 37, type: !7)
!34 = !DILocalVariable(name: "count", scope: !20, file: !3, line: 37, type: !7)
!35 = !DILocalVariable(name: "start", scope: !20, file: !3, line: 37, type: !7)
!36 = !DILocalVariable(name: "a", scope: !22, file: !3, line: 49, type: !15)
!37 = !DILocalVariable(name: "end", scope: !22, file: !3, line: 49, type: !7)
!38 = !DILocalVariable(name: "count", scope: !22, file: !3, line: 49, type: !7)
!39 = !DILocalVariable(name: "start", scope: !22, file: !3, line: 49, type: !7)
!40 = !DILocation(line: 18, column: 0, scope: !16)
!41 = !DILocation(line: 11, column: 0, scope: !16)
!42 = !DILocation(line: 21, column: 0, scope: !16)
!43 = !DILocation(line: 22, column: 0, scope: !16)
!44 = !DILocation(line: 32, column: 0, scope: !18)
!45 = !DILocation(line: 25, column: 0, scope: !18)
!46 = !DILocation(line: 33, column: 0, scope: !18)
!47 = !DILocation(line: 34, column: 0, scope: !18)
!48 = !DILocation(line: 44, column: 0, scope: !20)
!49 = !DILocation(line: 37, column: 0, scope: !20)
!50 = !DILocation(line: 45, column: 0, scope: !20)
!51 = !DILocation(line: 46, column: 0, scope: !20)
!52 = !DILocation(line: 57, column: 0, scope: !22)
!53 = !DILocation(line: 49, column: 0, scope: !22)
!54 = !DILocation(line: 58, column: 0, scope: !22)
!55 = !DILocation(line: 59, column: 0, scope: !22)
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
