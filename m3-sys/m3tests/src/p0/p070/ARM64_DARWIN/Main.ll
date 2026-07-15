; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


declare i64 @RTProcedureSRC__NumProcedures()
declare ptr @Fmt__Int(i64, i8)
declare ptr @RTHooks__Concat(ptr, ptr)
declare void @Wr__PutText(ptr, ptr)
declare void @RTProcedure__ToFingerprint(ptr, ptr)
declare ptr @RTProcedure__FromFingerprint(ptr)
declare void @Test__checkB(i1, i1)
declare void @Test__done()
declare void @RTHooks__TextLitInfo(ptr, ptr)
declare i8 @RTHooks__TextLitGetChar(ptr, i64)
declare i16 @RTHooks__TextLitGetWideChar(ptr, i64)
declare void @RTHooks__TextLitGetChars(ptr, ptr, i64)
declare void @RTHooks__TextLitGetWideChars(ptr, ptr, i64)

define void @Main__Toto() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  %t10 = alloca { [8 x i8] }
  %t1 = call i64 @RTProcedureSRC__NumProcedures()
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t2 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t3 = getelementptr i8, ptr %t2, i64 120
  %t4 = load ptr, ptr %t3
  %t5 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
  %t6 = trunc i64 10 to i8
  %t7 = call ptr @Fmt__Int(i64 %t5, i8 %t6)
  %t8 = call ptr @RTHooks__Concat(ptr %t7, ptr getelementptr inbounds (i8, ptr @textlit_0, i64 8))
  call void @Wr__PutText(ptr %t4, ptr %t8)
  store ptr @Main__Toto, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
  %t9 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
  call void @RTProcedure__ToFingerprint(ptr %t10, ptr %t9)
  %t11 = load { [8 x i8] }, ptr %t10
  store { [8 x i8] } %t11, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112)
  %t12 = load [2 x i32], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112)
  store [2 x i32] %t12, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
  %t13 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_imp.3, i64 0)
  %t14 = getelementptr i8, ptr %t13, i64 120
  %t15 = load ptr, ptr %t14
  %t16 = getelementptr inbounds [2 x i32], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128), i64 0, i64 0
  %t17 = load i32, ptr %t16
  %t18 = sext i32 %t17 to i64
  %t19 = trunc i64 10 to i8
  %t20 = call ptr @Fmt__Int(i64 %t18, i8 %t19)
  %t21 = call ptr @RTHooks__Concat(ptr getelementptr inbounds (i8, ptr @textlit_1, i64 8), ptr %t20)
  %t22 = call ptr @RTHooks__Concat(ptr %t21, ptr getelementptr inbounds (i8, ptr @textlit_2, i64 8))
  %t23 = getelementptr inbounds [2 x i32], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128), i64 0, i64 1
  %t24 = load i32, ptr %t23
  %t25 = sext i32 %t24 to i64
  %t26 = trunc i64 10 to i8
  %t27 = call ptr @Fmt__Int(i64 %t25, i8 %t26)
  %t28 = call ptr @RTHooks__Concat(ptr %t22, ptr %t27)
  %t29 = call ptr @RTHooks__Concat(ptr %t28, ptr getelementptr inbounds (i8, ptr @textlit_3, i64 8))
  call void @Wr__PutText(ptr %t15, ptr %t29)
  %t30 = call ptr @RTProcedure__FromFingerprint(ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112))
  store ptr %t30, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
  %t31 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
  %t32 = icmp eq ptr %t31, @Main__Toto
  call void @Test__checkB(i1 %t32, i1 1)
  call void @Test__done()
  ret void
}

; TEXT literal globals
@textlit_methods = internal constant [5 x ptr] [
  ptr @RTHooks__TextLitInfo,
  ptr @RTHooks__TextLitGetChar,
  ptr @RTHooks__TextLitGetWideChar,
  ptr @RTHooks__TextLitGetChars,
  ptr @RTHooks__TextLitGetWideChars
]
@textlit_0 = internal constant { i64, ptr, i64, [13 x i8] } { i64 2, ptr @textlit_methods, i64 12, [13 x i8] c" procedures\0a\00" }
@textlit_1 = internal constant { i64, ptr, i64, [21 x i8] } { i64 2, ptr @textlit_methods, i64 20, [21 x i8] c"Toto fingerprint = {\00" }
@textlit_2 = internal constant { i64, ptr, i64, [3 x i8] } { i64 2, ptr @textlit_methods, i64 2, [3 x i8] c", \00" }
@textlit_3 = internal constant { i64, ptr, i64, [3 x i8] } { i64 2, ptr @textlit_methods, i64 2, [3 x i8] c"}\0a\00" }

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_138420323 = internal global %TC_t {
  i64 0,
  i64 138420323,
  i64 u0x6aca01f2628a2191,
  i8 0,
  i8 1,
  i8 0,
  i8 0,
  [4 x i8] zeroinitializer,
  i64 0,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null
}

define weak ptr @Main_I3(i64 %mode) {
entry:
  ret ptr @Main_M3_info
}

; RT0.ImportInfo chain for Main
declare ptr @Test_I3(i64)
declare ptr @Fmt_I3(i64)
declare ptr @Stdio_I3(i64)
declare ptr @Wr_I3(i64)
declare ptr @RTProcedureSRC_I3(i64)
declare ptr @RTProcedure_I3(i64)
declare ptr @Fingerprint_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @Test_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Fmt_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Stdio_I3, ptr @Main_M3_imp.4 }
@Main_M3_imp.4 = internal global { ptr, ptr, ptr } { ptr null, ptr @Wr_I3, ptr @Main_M3_imp.5 }
@Main_M3_imp.5 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTProcedureSRC_I3, ptr @Main_M3_imp.6 }
@Main_M3_imp.6 = internal global { ptr, ptr, ptr } { ptr null, ptr @RTProcedure_I3, ptr @Main_M3_imp.7 }
@Main_M3_imp.7 = internal global { ptr, ptr, ptr } { ptr null, ptr @Fingerprint_I3, ptr null }

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [32 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_138420323,  ; type_cells (+8)
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
  i64 3,  ; gc_flags (+96)
  [32 x i8] zeroinitializer  ; user globals (32 bytes)
}
@Main__n = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
@Main__fp = alias { [8 x i8] }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112)
@Main__proc = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
@Main__xfp = alias [2 x i32], ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)

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
!16 = distinct !DISubprogram(name: "Main__Toto", linkageName: "Main__Toto", scope: !4, file: !3, line: 20, type: !6, scopeLine: 20, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!18 = !DILocation(line: 20, column: 0, scope: !16)
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
