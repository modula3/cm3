; ModuleID = 'Main'
source_filename = "Main"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)

declare void @RTHooks__CheckLoadTracedRef(ptr)
declare void @RTHooks__CheckStoreTraced(ptr)


declare ptr @RTHooks__AllocateTracedRef(ptr)
declare void @llvm.dbg.declare(metadata, metadata, metadata)


define void @Main__Test_AtomicBoolean_Fence() personality ptr @__gxx_personality_v0 !dbg !16 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicBoolean_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !18 {
entry:
  %t1 = load i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  %t2 = zext i1 %t1 to i8
  %t3.exp = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168), align 1
  %t3.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t3.exp, i8 %t2 seq_cst seq_cst
  %t3.old = extractvalue {i8, i1} %t3.cx, 0
  %t3 = extractvalue {i8, i1} %t3.cx, 1
  store i8 %t3.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168), align 1
  store i1 %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicBoolean_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !20 {
entry:
  %t1 = load i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t2 = zext i1 %t1 to i8
  %t3 = atomicrmw and ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t2 seq_cst
  %t4 = trunc i8 %t3 to i1
  store i1 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean_FetchDec() personality ptr @__gxx_personality_v0 !dbg !22 {
entry:
  %t1 = trunc i64 1 to i8
  %t2 = atomicrmw sub ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t1 seq_cst
  %t3 = trunc i8 %t2 to i1
  store i1 %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean_FetchInc() personality ptr @__gxx_personality_v0 !dbg !24 {
entry:
  %t1 = trunc i64 1 to i8
  %t2 = atomicrmw add ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t1 seq_cst
  %t3 = trunc i8 %t2 to i1
  store i1 %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean_FetchOr() personality ptr @__gxx_personality_v0 !dbg !26 {
entry:
  %t1 = load i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t2 = zext i1 %t1 to i8
  %t3 = atomicrmw or ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t2 seq_cst
  %t4 = trunc i8 %t3 to i1
  store i1 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean_FetchXor() personality ptr @__gxx_personality_v0 !dbg !28 {
entry:
  %t1 = load i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t2 = zext i1 %t1 to i8
  %t3 = atomicrmw xor ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t2 seq_cst
  %t4 = trunc i8 %t3 to i1
  store i1 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define i1 @Main__Test_AtomicBoolean_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !30 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !172, metadata !DIExpression()), !dbg !191
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicBoolean_LoadStore() personality ptr @__gxx_personality_v0 !dbg !32 {
entry:
  %t1 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t2 = trunc i8 %t1 to i1
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t3 = zext i1 0 to i8
  store atomic i8 %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t4 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t5 = trunc i8 %t4 to i1
  store i1 %t5, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t6 = zext i1 1 to i8
  store atomic i8 %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t7 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t8 = trunc i8 %t7 to i1
  store i1 %t8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  %t9 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104) seq_cst, align 1
  %t10 = trunc i8 %t9 to i1
  store i1 %t10, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean_Swap() personality ptr @__gxx_personality_v0 !dbg !34 {
entry:
  %t1 = load i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
  %t2 = zext i1 %t1 to i8
  %t3 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104), i8 %t2 seq_cst
  %t4 = trunc i8 %t3 to i1
  store i1 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
  ret void
}

define void @Main__Test_AtomicBoolean() personality ptr @__gxx_personality_v0 !dbg !36 {
entry:
  call void @Main__Test_AtomicBoolean_Fence(), !dbg !200
  call void @Main__Test_AtomicBoolean_CompareSwap(), !dbg !201
  call void @Main__Test_AtomicBoolean_FetchAnd(), !dbg !202
  call void @Main__Test_AtomicBoolean_FetchDec(), !dbg !203
  call void @Main__Test_AtomicBoolean_FetchInc(), !dbg !204
  call void @Main__Test_AtomicBoolean_FetchOr(), !dbg !205
  call void @Main__Test_AtomicBoolean_FetchXor(), !dbg !206
  %t1 = call i1 @Main__Test_AtomicBoolean_IsLockFree(), !dbg !207
  call void @Main__Test_AtomicBoolean_LoadStore(), !dbg !208
  call void @Main__Test_AtomicBoolean_Swap(), !dbg !209
  ret void
}

define void @Main__Test_AtomicChar_Fence() personality ptr @__gxx_personality_v0 !dbg !38 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicChar_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !40 {
entry:
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  %t2.exp = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170), align 1
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t2.exp, i8 %t1 seq_cst seq_cst
  %t2.old = extractvalue {i8, i1} %t2.cx, 0
  %t2 = extractvalue {i8, i1} %t2.cx, 1
  store i8 %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170), align 1
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicChar_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !42 {
entry:
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  %t2 = atomicrmw and ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar_FetchDec() personality ptr @__gxx_personality_v0 !dbg !44 {
entry:
  %t1 = trunc i64 1 to i8
  %t2 = atomicrmw sub ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar_FetchInc() personality ptr @__gxx_personality_v0 !dbg !46 {
entry:
  %t1 = trunc i64 1 to i8
  %t2 = atomicrmw add ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar_FetchOr() personality ptr @__gxx_personality_v0 !dbg !48 {
entry:
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  %t2 = atomicrmw or ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar_FetchXor() personality ptr @__gxx_personality_v0 !dbg !50 {
entry:
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  %t2 = atomicrmw xor ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define i1 @Main__Test_AtomicChar_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !52 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !173, metadata !DIExpression()), !dbg !217
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicChar_LoadStore() personality ptr @__gxx_personality_v0 !dbg !54 {
entry:
  %t1 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  store i8 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  store atomic i8 6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  %t2 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  store atomic i8 6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  %t3 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  store i8 %t3, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  %t4 = load atomic i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105) seq_cst, align 1
  store i8 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar_Swap() personality ptr @__gxx_personality_v0 !dbg !56 {
entry:
  %t1 = load i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105), i8 %t1 seq_cst
  store i8 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
  ret void
}

define void @Main__Test_AtomicChar() personality ptr @__gxx_personality_v0 !dbg !58 {
entry:
  call void @Main__Test_AtomicChar_Fence(), !dbg !226
  call void @Main__Test_AtomicChar_CompareSwap(), !dbg !227
  call void @Main__Test_AtomicChar_FetchAnd(), !dbg !228
  call void @Main__Test_AtomicChar_FetchDec(), !dbg !229
  call void @Main__Test_AtomicChar_FetchInc(), !dbg !230
  call void @Main__Test_AtomicChar_FetchOr(), !dbg !231
  call void @Main__Test_AtomicChar_FetchXor(), !dbg !232
  %t1 = call i1 @Main__Test_AtomicChar_IsLockFree(), !dbg !233
  call void @Main__Test_AtomicChar_LoadStore(), !dbg !234
  call void @Main__Test_AtomicChar_Swap(), !dbg !235
  ret void
}

define void @Main__Test_AtomicWidechar_Fence() personality ptr @__gxx_personality_v0 !dbg !60 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicWidechar_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !62 {
entry:
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  %t2.exp = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208), align 2
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t2.exp, i16 %t1 seq_cst seq_cst
  %t2.old = extractvalue {i16, i1} %t2.cx, 0
  %t2 = extractvalue {i16, i1} %t2.cx, 1
  store i16 %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208), align 2
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicWidechar_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !64 {
entry:
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t2 = atomicrmw and ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define void @Main__Test_AtomicWidechar_FetchDec() personality ptr @__gxx_personality_v0 !dbg !66 {
entry:
  %t1 = trunc i64 1 to i16
  %t2 = atomicrmw sub ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define void @Main__Test_AtomicWidechar_FetchInc() personality ptr @__gxx_personality_v0 !dbg !68 {
entry:
  %t1 = trunc i64 1 to i16
  %t2 = atomicrmw add ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define void @Main__Test_AtomicWidechar_FetchOr() personality ptr @__gxx_personality_v0 !dbg !70 {
entry:
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t2 = atomicrmw or ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define void @Main__Test_AtomicWidechar_FetchXor() personality ptr @__gxx_personality_v0 !dbg !72 {
entry:
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t2 = atomicrmw xor ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define i1 @Main__Test_AtomicWidechar_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !74 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !174, metadata !DIExpression()), !dbg !243
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicWidechar_LoadStore() personality ptr @__gxx_personality_v0 !dbg !76 {
entry:
  %integerC.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %integerC.slot, metadata !175, metadata !DIExpression()), !dbg !245
  %t1 = load atomic i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  store i16 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  store atomic i16 6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  %t2 = load atomic i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  store atomic i16 6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  %t3 = load atomic i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  %t4 = zext i16 %t3 to i64
  store i64 %t4, ptr %integerC.slot
  %t5 = load atomic i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136) seq_cst, align 2
  %t6 = zext i16 %t5 to i64
  store i64 %t6, ptr %integerC.slot
  ret void
}

define void @Main__Test_AtomicWidechar_Swap() personality ptr @__gxx_personality_v0 !dbg !78 {
entry:
  %t1 = load i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136), i16 %t1 seq_cst
  store i16 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
  ret void
}

define void @Main__Test_AtomicWidechar() personality ptr @__gxx_personality_v0 !dbg !80 {
entry:
  call void @Main__Test_AtomicWidechar_Fence(), !dbg !253
  call void @Main__Test_AtomicWidechar_CompareSwap(), !dbg !254
  call void @Main__Test_AtomicWidechar_FetchAnd(), !dbg !255
  call void @Main__Test_AtomicWidechar_FetchDec(), !dbg !256
  call void @Main__Test_AtomicWidechar_FetchInc(), !dbg !257
  call void @Main__Test_AtomicWidechar_FetchOr(), !dbg !258
  call void @Main__Test_AtomicWidechar_FetchXor(), !dbg !259
  %t1 = call i1 @Main__Test_AtomicWidechar_IsLockFree(), !dbg !260
  call void @Main__Test_AtomicWidechar_LoadStore(), !dbg !261
  call void @Main__Test_AtomicWidechar_Swap(), !dbg !262
  ret void
}

define void @Main__Test_AtomicRefany_Fence() personality ptr @__gxx_personality_v0 !dbg !82 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicRefany_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !84 {
entry:
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  %__gc_nil.1 = icmp eq ptr %t1, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t1 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t1, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t1), !dbg !264
  br label %gc.skip.1
gc.skip.1:
  %t2.exp = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192), align 8
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128), ptr %t2.exp, ptr %t1 seq_cst seq_cst
  %t2.old = extractvalue {ptr, i1} %t2.cx, 0
  %t2 = extractvalue {ptr, i1} %t2.cx, 1
  store ptr %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192), align 8
  %__gc_nil.2 = icmp eq ptr %t2.old, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t2.old to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t2.old, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2.old), !dbg !264
  br label %gc.skip.2
gc.skip.2:
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicRefany_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !86 {
entry:
  ret void
}

define void @Main__Test_AtomicRefany_FetchDec() personality ptr @__gxx_personality_v0 !dbg !88 {
entry:
  ret void
}

define void @Main__Test_AtomicRefany_FetchInc() personality ptr @__gxx_personality_v0 !dbg !90 {
entry:
  ret void
}

define void @Main__Test_AtomicRefany_FetchOr() personality ptr @__gxx_personality_v0 !dbg !92 {
entry:
  ret void
}

define void @Main__Test_AtomicRefany_FetchXor() personality ptr @__gxx_personality_v0 !dbg !94 {
entry:
  ret void
}

define i1 @Main__Test_AtomicRefany_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !96 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !176, metadata !DIExpression()), !dbg !270
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicRefany_LoadStore() personality ptr @__gxx_personality_v0 !dbg !98 {
entry:
  %xxx.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %xxx.slot, metadata !177, metadata !DIExpression()), !dbg !272
  %refanyC.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %refanyC.slot, metadata !178, metadata !DIExpression()), !dbg !272
  store ptr null, ptr %refanyC.slot
  store ptr null, ptr %xxx.slot
  %t1 = call ptr @RTHooks__AllocateTracedRef(ptr @tc_ref_1355119930), !dbg !272
  %t2 = bitcast ptr %t1 to ptr
  store ptr %t2, ptr %xxx.slot
  %t3 = load ptr, ptr %xxx.slot
  store i64 23, ptr %t3
  %t4 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %__gc_nil.1 = icmp eq ptr %t4, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t4 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t4, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t4), !dbg !274
  br label %gc.skip.1
gc.skip.1:
  store ptr %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192)
  %t5 = load ptr, ptr %xxx.slot
  store atomic ptr %t5, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %t6 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %__gc_nil.2 = icmp eq ptr %t6, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t6 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t6, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t6), !dbg !276
  br label %gc.skip.2
gc.skip.2:
  store ptr %t6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192)
  %t7 = load ptr, ptr %xxx.slot
  store atomic ptr %t7, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %t8 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %__gc_nil.3 = icmp eq ptr %t8, null
  br i1 %__gc_nil.3, label %gc.skip.3, label %gc.check.3
gc.check.3:
  %__gc_int.3 = ptrtoint ptr %t8 to i64
  %__gc_low.3 = and i64 %__gc_int.3, 1
  %__gc_ma.3 = icmp ne i64 %__gc_low.3, 0
  br i1 %__gc_ma.3, label %gc.skip.3, label %gc.gray.3
gc.gray.3:
  %__gc_hptr.3 = getelementptr i8, ptr %t8, i64 -8
  %__gc_hdr.3 = load i64, ptr %__gc_hptr.3
  %__gc_gb.3 = and i64 %__gc_hdr.3, 4194304
  %__gc_gr.3 = icmp ne i64 %__gc_gb.3, 0
  br i1 %__gc_gr.3, label %gc.slow.3, label %gc.skip.3
gc.slow.3:
  call void @RTHooks__CheckLoadTracedRef(ptr %t8), !dbg !278
  br label %gc.skip.3
gc.skip.3:
  store ptr %t8, ptr %refanyC.slot
  %t9 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128) seq_cst, align 8
  %__gc_nil.4 = icmp eq ptr %t9, null
  br i1 %__gc_nil.4, label %gc.skip.4, label %gc.check.4
gc.check.4:
  %__gc_int.4 = ptrtoint ptr %t9 to i64
  %__gc_low.4 = and i64 %__gc_int.4, 1
  %__gc_ma.4 = icmp ne i64 %__gc_low.4, 0
  br i1 %__gc_ma.4, label %gc.skip.4, label %gc.gray.4
gc.gray.4:
  %__gc_hptr.4 = getelementptr i8, ptr %t9, i64 -8
  %__gc_hdr.4 = load i64, ptr %__gc_hptr.4
  %__gc_gb.4 = and i64 %__gc_hdr.4, 4194304
  %__gc_gr.4 = icmp ne i64 %__gc_gb.4, 0
  br i1 %__gc_gr.4, label %gc.slow.4, label %gc.skip.4
gc.slow.4:
  call void @RTHooks__CheckLoadTracedRef(ptr %t9), !dbg !279
  br label %gc.skip.4
gc.skip.4:
  store ptr %t9, ptr %refanyC.slot
  ret void
}

define void @Main__Test_AtomicRefany_Swap() personality ptr @__gxx_personality_v0 !dbg !100 {
entry:
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192)
  %__gc_nil.1 = icmp eq ptr %t1, null
  br i1 %__gc_nil.1, label %gc.skip.1, label %gc.check.1
gc.check.1:
  %__gc_int.1 = ptrtoint ptr %t1 to i64
  %__gc_low.1 = and i64 %__gc_int.1, 1
  %__gc_ma.1 = icmp ne i64 %__gc_low.1, 0
  br i1 %__gc_ma.1, label %gc.skip.1, label %gc.gray.1
gc.gray.1:
  %__gc_hptr.1 = getelementptr i8, ptr %t1, i64 -8
  %__gc_hdr.1 = load i64, ptr %__gc_hptr.1
  %__gc_gb.1 = and i64 %__gc_hdr.1, 4194304
  %__gc_gr.1 = icmp ne i64 %__gc_gb.1, 0
  br i1 %__gc_gr.1, label %gc.slow.1, label %gc.skip.1
gc.slow.1:
  call void @RTHooks__CheckLoadTracedRef(ptr %t1), !dbg !280
  br label %gc.skip.1
gc.skip.1:
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128), ptr %t1 seq_cst
  %__gc_nil.2 = icmp eq ptr %t2, null
  br i1 %__gc_nil.2, label %gc.skip.2, label %gc.check.2
gc.check.2:
  %__gc_int.2 = ptrtoint ptr %t2 to i64
  %__gc_low.2 = and i64 %__gc_int.2, 1
  %__gc_ma.2 = icmp ne i64 %__gc_low.2, 0
  br i1 %__gc_ma.2, label %gc.skip.2, label %gc.gray.2
gc.gray.2:
  %__gc_hptr.2 = getelementptr i8, ptr %t2, i64 -8
  %__gc_hdr.2 = load i64, ptr %__gc_hptr.2
  %__gc_gb.2 = and i64 %__gc_hdr.2, 4194304
  %__gc_gr.2 = icmp ne i64 %__gc_gb.2, 0
  br i1 %__gc_gr.2, label %gc.slow.2, label %gc.skip.2
gc.slow.2:
  call void @RTHooks__CheckLoadTracedRef(ptr %t2), !dbg !280
  br label %gc.skip.2
gc.skip.2:
  store ptr %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
  ret void
}

define void @Main__Test_AtomicRefany() personality ptr @__gxx_personality_v0 !dbg !102 {
entry:
  call void @Main__Test_AtomicRefany_Fence(), !dbg !281
  call void @Main__Test_AtomicRefany_CompareSwap(), !dbg !282
  call void @Main__Test_AtomicRefany_FetchAnd(), !dbg !283
  call void @Main__Test_AtomicRefany_FetchDec(), !dbg !284
  call void @Main__Test_AtomicRefany_FetchInc(), !dbg !285
  call void @Main__Test_AtomicRefany_FetchOr(), !dbg !286
  call void @Main__Test_AtomicRefany_FetchXor(), !dbg !287
  %t1 = call i1 @Main__Test_AtomicRefany_IsLockFree(), !dbg !288
  call void @Main__Test_AtomicRefany_LoadStore(), !dbg !289
  call void @Main__Test_AtomicRefany_Swap(), !dbg !290
  ret void
}

define void @Main__Test_AtomicAddress_Fence() personality ptr @__gxx_personality_v0 !dbg !104 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicAddress_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !106 {
entry:
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
  %t2.exp = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216), align 8
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144), ptr %t2.exp, ptr %t1 seq_cst seq_cst
  %t2.old = extractvalue {ptr, i1} %t2.cx, 0
  %t2 = extractvalue {ptr, i1} %t2.cx, 1
  store ptr %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216), align 8
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicAddress_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !108 {
entry:
  ret void
}

define void @Main__Test_AtomicAddress_FetchDec() personality ptr @__gxx_personality_v0 !dbg !110 {
entry:
  ret void
}

define void @Main__Test_AtomicAddress_FetchInc() personality ptr @__gxx_personality_v0 !dbg !112 {
entry:
  ret void
}

define void @Main__Test_AtomicAddress_FetchOr() personality ptr @__gxx_personality_v0 !dbg !114 {
entry:
  ret void
}

define void @Main__Test_AtomicAddress_FetchXor() personality ptr @__gxx_personality_v0 !dbg !116 {
entry:
  ret void
}

define i1 @Main__Test_AtomicAddress_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !118 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !179, metadata !DIExpression()), !dbg !298
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicAddress_LoadStore() personality ptr @__gxx_personality_v0 !dbg !120 {
entry:
  %addressC.slot = alloca ptr
  call void @llvm.dbg.declare(metadata ptr %addressC.slot, metadata !180, metadata !DIExpression()), !dbg !300
  store ptr null, ptr %addressC.slot
  %t1 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  store ptr %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  %t2 = add i64 1, 2
  %t3 = add i64 %t2, 3
  %t4 = inttoptr i64 %t3 to ptr
  store atomic ptr %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  %t5 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  store ptr %t5, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  %t6 = add i64 1, 2
  %t7 = add i64 %t6, 3
  %t8 = inttoptr i64 %t7 to ptr
  store atomic ptr %t8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  %t9 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  store ptr %t9, ptr %addressC.slot
  %t10 = load atomic ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144) seq_cst, align 8
  store ptr %t10, ptr %addressC.slot
  ret void
}

define void @Main__Test_AtomicAddress_Swap() personality ptr @__gxx_personality_v0 !dbg !122 {
entry:
  %t1 = load ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144), ptr %t1 seq_cst
  store ptr %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
  ret void
}

define void @Main__Test_AtomicAddress() personality ptr @__gxx_personality_v0 !dbg !124 {
entry:
  call void @Main__Test_AtomicAddress_Fence(), !dbg !308
  call void @Main__Test_AtomicAddress_CompareSwap(), !dbg !309
  call void @Main__Test_AtomicAddress_FetchAnd(), !dbg !310
  call void @Main__Test_AtomicAddress_FetchDec(), !dbg !311
  call void @Main__Test_AtomicAddress_FetchInc(), !dbg !312
  call void @Main__Test_AtomicAddress_FetchOr(), !dbg !313
  call void @Main__Test_AtomicAddress_FetchXor(), !dbg !314
  %t1 = call i1 @Main__Test_AtomicAddress_IsLockFree(), !dbg !315
  call void @Main__Test_AtomicAddress_LoadStore(), !dbg !316
  call void @Main__Test_AtomicAddress_Swap(), !dbg !317
  ret void
}

define void @Main__Test_AtomicInteger_Fence() personality ptr @__gxx_personality_v0 !dbg !126 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicInteger_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !128 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  %t2.exp = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152), align 8
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t2.exp, i64 %t1 seq_cst seq_cst
  %t2.old = extractvalue {i64, i1} %t2.cx, 0
  %t2 = extractvalue {i64, i1} %t2.cx, 1
  store i64 %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152), align 8
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  %t3 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  %t4 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t3 seq_cst
  store i64 %t4, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !130 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  %t2 = atomicrmw and ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger_FetchDec() personality ptr @__gxx_personality_v0 !dbg !132 {
entry:
  %t1 = atomicrmw sub ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 1 seq_cst
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger_FetchInc() personality ptr @__gxx_personality_v0 !dbg !134 {
entry:
  %t1 = atomicrmw add ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 1 seq_cst
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger_FetchOr() personality ptr @__gxx_personality_v0 !dbg !136 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  %t2 = atomicrmw or ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger_FetchXor() personality ptr @__gxx_personality_v0 !dbg !138 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  %t2 = atomicrmw xor ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define i1 @Main__Test_AtomicInteger_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !140 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !181, metadata !DIExpression()), !dbg !326
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicInteger_LoadStore() personality ptr @__gxx_personality_v0 !dbg !142 {
entry:
  %integerC.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %integerC.slot, metadata !182, metadata !DIExpression()), !dbg !328
  %t1 = load atomic i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  store atomic i64 6, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  %t2 = load atomic i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  store atomic i64 10, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  %t3 = load atomic i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  store i64 %t3, ptr %integerC.slot
  %t4 = load atomic i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112) seq_cst, align 8
  store i64 %t4, ptr %integerC.slot
  ret void
}

define void @Main__Test_AtomicInteger_Swap() personality ptr @__gxx_personality_v0 !dbg !144 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
  ret void
}

define void @Main__Test_AtomicInteger() personality ptr @__gxx_personality_v0 !dbg !146 {
entry:
  call void @Main__Test_AtomicInteger_Fence(), !dbg !336
  call void @Main__Test_AtomicInteger_CompareSwap(), !dbg !337
  call void @Main__Test_AtomicInteger_FetchAnd(), !dbg !338
  call void @Main__Test_AtomicInteger_FetchDec(), !dbg !339
  call void @Main__Test_AtomicInteger_FetchInc(), !dbg !340
  call void @Main__Test_AtomicInteger_FetchOr(), !dbg !341
  call void @Main__Test_AtomicInteger_FetchXor(), !dbg !342
  %t1 = call i1 @Main__Test_AtomicInteger_IsLockFree(), !dbg !343
  call void @Main__Test_AtomicInteger_LoadStore(), !dbg !344
  call void @Main__Test_AtomicInteger_Swap(), !dbg !345
  ret void
}

define void @Main__Test_AtomicLongint_Fence() personality ptr @__gxx_personality_v0 !dbg !148 {
entry:
  fence seq_cst
  ret void
}

define void @Main__Test_AtomicLongint_CompareSwap() personality ptr @__gxx_personality_v0 !dbg !150 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  %t2.exp = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176), align 8
  %t2.cx = cmpxchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 %t2.exp, i64 %t1 seq_cst seq_cst
  %t2.old = extractvalue {i64, i1} %t2.cx, 0
  %t2 = extractvalue {i64, i1} %t2.cx, 1
  store i64 %t2.old, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176), align 8
  store i1 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)
  ret void
}

define void @Main__Test_AtomicLongint_FetchAnd() personality ptr @__gxx_personality_v0 !dbg !152 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %t2 = atomicrmw and ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  ret void
}

define void @Main__Test_AtomicLongint_FetchDec() personality ptr @__gxx_personality_v0 !dbg !154 {
entry:
  %t1 = atomicrmw sub ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 1 seq_cst
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  ret void
}

define void @Main__Test_AtomicLongint_FetchInc() personality ptr @__gxx_personality_v0 !dbg !156 {
entry:
  %t1 = atomicrmw add ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 1 seq_cst
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  ret void
}

define void @Main__Test_AtomicLongint_FetchOr() personality ptr @__gxx_personality_v0 !dbg !158 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %t2 = atomicrmw or ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  ret void
}

define void @Main__Test_AtomicLongint_FetchXor() personality ptr @__gxx_personality_v0 !dbg !160 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %t2 = atomicrmw xor ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  ret void
}

define i1 @Main__Test_AtomicLongint_IsLockFree() personality ptr @__gxx_personality_v0 !dbg !162 {
entry:
  %_result.slot = alloca i64
  call void @llvm.dbg.declare(metadata ptr %_result.slot, metadata !183, metadata !DIExpression()), !dbg !353
  store i64 0, ptr %_result.slot
  ret i1 1
}

define void @Main__Test_AtomicLongint_Load() personality ptr @__gxx_personality_v0 !dbg !164 {
entry:
  %t1 = load atomic i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120) seq_cst, align 8
  store i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  ret void
}

define void @Main__Test_AtomicLongint_Store() personality ptr @__gxx_personality_v0 !dbg !166 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  store atomic i64 %t1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120) seq_cst, align 8
  ret void
}

define void @Main__Test_AtomicLongint_Swap() personality ptr @__gxx_personality_v0 !dbg !168 {
entry:
  %t1 = load i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
  %t2 = atomicrmw xchg ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120), i64 %t1 seq_cst
  store i64 %t2, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
  ret void
}

define void @Main__Test_AtomicLongint() personality ptr @__gxx_personality_v0 !dbg !170 {
entry:
  call void @Main__Test_AtomicLongint_Fence(), !dbg !358
  call void @Main__Test_AtomicLongint_CompareSwap(), !dbg !359
  call void @Main__Test_AtomicLongint_FetchAnd(), !dbg !360
  call void @Main__Test_AtomicLongint_FetchDec(), !dbg !361
  call void @Main__Test_AtomicLongint_FetchInc(), !dbg !362
  call void @Main__Test_AtomicLongint_FetchOr(), !dbg !363
  call void @Main__Test_AtomicLongint_FetchXor(), !dbg !364
  %t1 = call i1 @Main__Test_AtomicLongint_IsLockFree(), !dbg !365
  call void @Main__Test_AtomicLongint_Load(), !dbg !366
  call void @Main__Test_AtomicLongint_Store(), !dbg !367
  call void @Main__Test_AtomicLongint_Swap(), !dbg !368
  ret void
}

define void @Main__Main_M3() personality ptr @__gxx_personality_v0 {
entry:
  call void @Main__Test_AtomicBoolean()
  call void @Main__Test_AtomicChar()
  call void @Main__Test_AtomicWidechar()
  call void @Main__Test_AtomicLongint()
  call void @Main__Test_AtomicInteger()
  call void @Main__Test_AtomicAddress()
  call void @Main__Test_AtomicRefany()
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_471614950 = internal global %TC_t {
  i64 0,
  i64 471614950,
  i64 u0x796e616665722480,
  i8 1,
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
  ptr @tc_ref_138420323
}
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
  ptr @tc_ref_1355119930
}
@tc_ref_1355119930 = internal global %TC_t {
  i64 0,
  i64 1355119930,
  i64 u0x01ae5ff0516b22ca,
  i8 1,
  i8 1,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 8,
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
declare ptr @WideChar_I3(i64)
declare ptr @Refany_I3(i64)
declare ptr @Longint_I3(i64)
declare ptr @Integer_I3(i64)
declare ptr @Char_I3(i64)
declare ptr @Boolean_I3(i64)
declare ptr @Address_I3(i64)
declare ptr @AtomicWideChar_I3(i64)
declare ptr @AtomicRefany_I3(i64)
declare ptr @AtomicLongint_I3(i64)
declare ptr @AtomicInteger_I3(i64)
declare ptr @AtomicChar_I3(i64)
declare ptr @AtomicBoolean_I3(i64)
declare ptr @AtomicAddress_I3(i64)
@Main_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_I3, ptr @Main_M3_imp.1 }
@Main_M3_imp.1 = internal global { ptr, ptr, ptr } { ptr null, ptr @WideChar_I3, ptr @Main_M3_imp.2 }
@Main_M3_imp.2 = internal global { ptr, ptr, ptr } { ptr null, ptr @Refany_I3, ptr @Main_M3_imp.3 }
@Main_M3_imp.3 = internal global { ptr, ptr, ptr } { ptr null, ptr @Longint_I3, ptr @Main_M3_imp.4 }
@Main_M3_imp.4 = internal global { ptr, ptr, ptr } { ptr null, ptr @Integer_I3, ptr @Main_M3_imp.5 }
@Main_M3_imp.5 = internal global { ptr, ptr, ptr } { ptr null, ptr @Char_I3, ptr @Main_M3_imp.6 }
@Main_M3_imp.6 = internal global { ptr, ptr, ptr } { ptr null, ptr @Boolean_I3, ptr @Main_M3_imp.7 }
@Main_M3_imp.7 = internal global { ptr, ptr, ptr } { ptr null, ptr @Address_I3, ptr @Main_M3_imp.8 }
@Main_M3_imp.8 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicWideChar_I3, ptr @Main_M3_imp.9 }
@Main_M3_imp.9 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicRefany_I3, ptr @Main_M3_imp.10 }
@Main_M3_imp.10 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicLongint_I3, ptr @Main_M3_imp.11 }
@Main_M3_imp.11 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicInteger_I3, ptr @Main_M3_imp.12 }
@Main_M3_imp.12 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicChar_I3, ptr @Main_M3_imp.13 }
@Main_M3_imp.13 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicBoolean_I3, ptr @Main_M3_imp.14 }
@Main_M3_imp.14 = internal global { ptr, ptr, ptr } { ptr null, ptr @AtomicAddress_I3, ptr null }
@Main_M3_gc_map = internal constant [7 x i8] c"\2a\68\2a\58\04\04\00"

; RT0.ModuleInfo for Main (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, [129 x i8] }
@Main_M3_info = global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_471614950,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr @Main_M3_gc_map,  ; var_map (+56)
  ptr @Main_M3_gc_map,  ; gc_map (+64)
  ptr @Main_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @Main_M3,  ; binder (+88)
  i64 3,  ; gc_flags (+96)
  [129 x i8] zeroinitializer  ; user globals (129 bytes)
}
@Main__atomicBooleanA = alias { i8 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 104)
@Main__atomicCharA = alias { i8 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 105)
@Main__atomicIntegerA = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 112)
@Main__atomicLongintA = alias { i64 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 120)
@Main__atomicRefanyA = alias { ptr }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 128)
@Main__atomicWidecharA = alias { i16 }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 136)
@Main__atomicAddressA = alias { ptr }, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 144)
@Main__integerB = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 152)
@Main__integerC = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 160)
@Main__booleanB = alias i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 168)
@Main__booleanC = alias i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 169)
@Main__charB = alias i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 170)
@Main__charC = alias i8, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 171)
@Main__longintB = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 176)
@Main__longintC = alias i64, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 184)
@Main__refanyB = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 192)
@Main__refanyC = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 200)
@Main__widecharB = alias i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 208)
@Main__widecharC = alias i16, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 210)
@Main__addressB = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 216)
@Main__addressC = alias ptr, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 224)
@Main__bool = alias i1, ptr getelementptr inbounds (i8, ptr @Main_M3_info, i64 232)

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
!16 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_Fence", linkageName: "Main__Test_AtomicBoolean_Fence", scope: !4, file: !3, line: 23, type: !6, scopeLine: 23, unit: !2, spFlags: DISPFlagDefinition)
!18 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_CompareSwap", linkageName: "Main__Test_AtomicBoolean_CompareSwap", scope: !4, file: !3, line: 28, type: !6, scopeLine: 28, unit: !2, spFlags: DISPFlagDefinition)
!20 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_FetchAnd", linkageName: "Main__Test_AtomicBoolean_FetchAnd", scope: !4, file: !3, line: 33, type: !6, scopeLine: 33, unit: !2, spFlags: DISPFlagDefinition)
!22 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_FetchDec", linkageName: "Main__Test_AtomicBoolean_FetchDec", scope: !4, file: !3, line: 38, type: !6, scopeLine: 38, unit: !2, spFlags: DISPFlagDefinition)
!24 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_FetchInc", linkageName: "Main__Test_AtomicBoolean_FetchInc", scope: !4, file: !3, line: 43, type: !6, scopeLine: 43, unit: !2, spFlags: DISPFlagDefinition)
!26 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_FetchOr", linkageName: "Main__Test_AtomicBoolean_FetchOr", scope: !4, file: !3, line: 48, type: !6, scopeLine: 48, unit: !2, spFlags: DISPFlagDefinition)
!28 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_FetchXor", linkageName: "Main__Test_AtomicBoolean_FetchXor", scope: !4, file: !3, line: 53, type: !6, scopeLine: 53, unit: !2, spFlags: DISPFlagDefinition)
!30 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_IsLockFree", linkageName: "Main__Test_AtomicBoolean_IsLockFree", scope: !4, file: !3, line: 58, type: !6, scopeLine: 58, unit: !2, spFlags: DISPFlagDefinition)
!32 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_LoadStore", linkageName: "Main__Test_AtomicBoolean_LoadStore", scope: !4, file: !3, line: 63, type: !6, scopeLine: 63, unit: !2, spFlags: DISPFlagDefinition)
!34 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean_Swap", linkageName: "Main__Test_AtomicBoolean_Swap", scope: !4, file: !3, line: 74, type: !6, scopeLine: 74, unit: !2, spFlags: DISPFlagDefinition)
!36 = distinct !DISubprogram(name: "Main__Test_AtomicBoolean", linkageName: "Main__Test_AtomicBoolean", scope: !4, file: !3, line: 79, type: !6, scopeLine: 79, unit: !2, spFlags: DISPFlagDefinition)
!38 = distinct !DISubprogram(name: "Main__Test_AtomicChar_Fence", linkageName: "Main__Test_AtomicChar_Fence", scope: !4, file: !3, line: 95, type: !6, scopeLine: 95, unit: !2, spFlags: DISPFlagDefinition)
!40 = distinct !DISubprogram(name: "Main__Test_AtomicChar_CompareSwap", linkageName: "Main__Test_AtomicChar_CompareSwap", scope: !4, file: !3, line: 100, type: !6, scopeLine: 100, unit: !2, spFlags: DISPFlagDefinition)
!42 = distinct !DISubprogram(name: "Main__Test_AtomicChar_FetchAnd", linkageName: "Main__Test_AtomicChar_FetchAnd", scope: !4, file: !3, line: 105, type: !6, scopeLine: 105, unit: !2, spFlags: DISPFlagDefinition)
!44 = distinct !DISubprogram(name: "Main__Test_AtomicChar_FetchDec", linkageName: "Main__Test_AtomicChar_FetchDec", scope: !4, file: !3, line: 110, type: !6, scopeLine: 110, unit: !2, spFlags: DISPFlagDefinition)
!46 = distinct !DISubprogram(name: "Main__Test_AtomicChar_FetchInc", linkageName: "Main__Test_AtomicChar_FetchInc", scope: !4, file: !3, line: 115, type: !6, scopeLine: 115, unit: !2, spFlags: DISPFlagDefinition)
!48 = distinct !DISubprogram(name: "Main__Test_AtomicChar_FetchOr", linkageName: "Main__Test_AtomicChar_FetchOr", scope: !4, file: !3, line: 120, type: !6, scopeLine: 120, unit: !2, spFlags: DISPFlagDefinition)
!50 = distinct !DISubprogram(name: "Main__Test_AtomicChar_FetchXor", linkageName: "Main__Test_AtomicChar_FetchXor", scope: !4, file: !3, line: 125, type: !6, scopeLine: 125, unit: !2, spFlags: DISPFlagDefinition)
!52 = distinct !DISubprogram(name: "Main__Test_AtomicChar_IsLockFree", linkageName: "Main__Test_AtomicChar_IsLockFree", scope: !4, file: !3, line: 130, type: !6, scopeLine: 130, unit: !2, spFlags: DISPFlagDefinition)
!54 = distinct !DISubprogram(name: "Main__Test_AtomicChar_LoadStore", linkageName: "Main__Test_AtomicChar_LoadStore", scope: !4, file: !3, line: 135, type: !6, scopeLine: 135, unit: !2, spFlags: DISPFlagDefinition)
!56 = distinct !DISubprogram(name: "Main__Test_AtomicChar_Swap", linkageName: "Main__Test_AtomicChar_Swap", scope: !4, file: !3, line: 146, type: !6, scopeLine: 146, unit: !2, spFlags: DISPFlagDefinition)
!58 = distinct !DISubprogram(name: "Main__Test_AtomicChar", linkageName: "Main__Test_AtomicChar", scope: !4, file: !3, line: 151, type: !6, scopeLine: 151, unit: !2, spFlags: DISPFlagDefinition)
!60 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_Fence", linkageName: "Main__Test_AtomicWidechar_Fence", scope: !4, file: !3, line: 171, type: !6, scopeLine: 171, unit: !2, spFlags: DISPFlagDefinition)
!62 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_CompareSwap", linkageName: "Main__Test_AtomicWidechar_CompareSwap", scope: !4, file: !3, line: 176, type: !6, scopeLine: 176, unit: !2, spFlags: DISPFlagDefinition)
!64 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_FetchAnd", linkageName: "Main__Test_AtomicWidechar_FetchAnd", scope: !4, file: !3, line: 181, type: !6, scopeLine: 181, unit: !2, spFlags: DISPFlagDefinition)
!66 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_FetchDec", linkageName: "Main__Test_AtomicWidechar_FetchDec", scope: !4, file: !3, line: 186, type: !6, scopeLine: 186, unit: !2, spFlags: DISPFlagDefinition)
!68 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_FetchInc", linkageName: "Main__Test_AtomicWidechar_FetchInc", scope: !4, file: !3, line: 191, type: !6, scopeLine: 191, unit: !2, spFlags: DISPFlagDefinition)
!70 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_FetchOr", linkageName: "Main__Test_AtomicWidechar_FetchOr", scope: !4, file: !3, line: 196, type: !6, scopeLine: 196, unit: !2, spFlags: DISPFlagDefinition)
!72 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_FetchXor", linkageName: "Main__Test_AtomicWidechar_FetchXor", scope: !4, file: !3, line: 201, type: !6, scopeLine: 201, unit: !2, spFlags: DISPFlagDefinition)
!74 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_IsLockFree", linkageName: "Main__Test_AtomicWidechar_IsLockFree", scope: !4, file: !3, line: 206, type: !6, scopeLine: 206, unit: !2, spFlags: DISPFlagDefinition)
!76 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_LoadStore", linkageName: "Main__Test_AtomicWidechar_LoadStore", scope: !4, file: !3, line: 211, type: !6, scopeLine: 211, unit: !2, spFlags: DISPFlagDefinition)
!78 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar_Swap", linkageName: "Main__Test_AtomicWidechar_Swap", scope: !4, file: !3, line: 223, type: !6, scopeLine: 223, unit: !2, spFlags: DISPFlagDefinition)
!80 = distinct !DISubprogram(name: "Main__Test_AtomicWidechar", linkageName: "Main__Test_AtomicWidechar", scope: !4, file: !3, line: 228, type: !6, scopeLine: 228, unit: !2, spFlags: DISPFlagDefinition)
!82 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_Fence", linkageName: "Main__Test_AtomicRefany_Fence", scope: !4, file: !3, line: 246, type: !6, scopeLine: 246, unit: !2, spFlags: DISPFlagDefinition)
!84 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_CompareSwap", linkageName: "Main__Test_AtomicRefany_CompareSwap", scope: !4, file: !3, line: 251, type: !6, scopeLine: 251, unit: !2, spFlags: DISPFlagDefinition)
!86 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_FetchAnd", linkageName: "Main__Test_AtomicRefany_FetchAnd", scope: !4, file: !3, line: 256, type: !6, scopeLine: 256, unit: !2, spFlags: DISPFlagDefinition)
!88 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_FetchDec", linkageName: "Main__Test_AtomicRefany_FetchDec", scope: !4, file: !3, line: 261, type: !6, scopeLine: 261, unit: !2, spFlags: DISPFlagDefinition)
!90 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_FetchInc", linkageName: "Main__Test_AtomicRefany_FetchInc", scope: !4, file: !3, line: 266, type: !6, scopeLine: 266, unit: !2, spFlags: DISPFlagDefinition)
!92 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_FetchOr", linkageName: "Main__Test_AtomicRefany_FetchOr", scope: !4, file: !3, line: 271, type: !6, scopeLine: 271, unit: !2, spFlags: DISPFlagDefinition)
!94 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_FetchXor", linkageName: "Main__Test_AtomicRefany_FetchXor", scope: !4, file: !3, line: 276, type: !6, scopeLine: 276, unit: !2, spFlags: DISPFlagDefinition)
!96 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_IsLockFree", linkageName: "Main__Test_AtomicRefany_IsLockFree", scope: !4, file: !3, line: 281, type: !6, scopeLine: 281, unit: !2, spFlags: DISPFlagDefinition)
!98 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_LoadStore", linkageName: "Main__Test_AtomicRefany_LoadStore", scope: !4, file: !3, line: 286, type: !6, scopeLine: 286, unit: !2, spFlags: DISPFlagDefinition)
!100 = distinct !DISubprogram(name: "Main__Test_AtomicRefany_Swap", linkageName: "Main__Test_AtomicRefany_Swap", scope: !4, file: !3, line: 303, type: !6, scopeLine: 303, unit: !2, spFlags: DISPFlagDefinition)
!102 = distinct !DISubprogram(name: "Main__Test_AtomicRefany", linkageName: "Main__Test_AtomicRefany", scope: !4, file: !3, line: 308, type: !6, scopeLine: 308, unit: !2, spFlags: DISPFlagDefinition)
!104 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_Fence", linkageName: "Main__Test_AtomicAddress_Fence", scope: !4, file: !3, line: 326, type: !6, scopeLine: 326, unit: !2, spFlags: DISPFlagDefinition)
!106 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_CompareSwap", linkageName: "Main__Test_AtomicAddress_CompareSwap", scope: !4, file: !3, line: 331, type: !6, scopeLine: 331, unit: !2, spFlags: DISPFlagDefinition)
!108 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_FetchAnd", linkageName: "Main__Test_AtomicAddress_FetchAnd", scope: !4, file: !3, line: 336, type: !6, scopeLine: 336, unit: !2, spFlags: DISPFlagDefinition)
!110 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_FetchDec", linkageName: "Main__Test_AtomicAddress_FetchDec", scope: !4, file: !3, line: 341, type: !6, scopeLine: 341, unit: !2, spFlags: DISPFlagDefinition)
!112 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_FetchInc", linkageName: "Main__Test_AtomicAddress_FetchInc", scope: !4, file: !3, line: 346, type: !6, scopeLine: 346, unit: !2, spFlags: DISPFlagDefinition)
!114 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_FetchOr", linkageName: "Main__Test_AtomicAddress_FetchOr", scope: !4, file: !3, line: 351, type: !6, scopeLine: 351, unit: !2, spFlags: DISPFlagDefinition)
!116 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_FetchXor", linkageName: "Main__Test_AtomicAddress_FetchXor", scope: !4, file: !3, line: 356, type: !6, scopeLine: 356, unit: !2, spFlags: DISPFlagDefinition)
!118 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_IsLockFree", linkageName: "Main__Test_AtomicAddress_IsLockFree", scope: !4, file: !3, line: 361, type: !6, scopeLine: 361, unit: !2, spFlags: DISPFlagDefinition)
!120 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_LoadStore", linkageName: "Main__Test_AtomicAddress_LoadStore", scope: !4, file: !3, line: 366, type: !6, scopeLine: 366, unit: !2, spFlags: DISPFlagDefinition)
!122 = distinct !DISubprogram(name: "Main__Test_AtomicAddress_Swap", linkageName: "Main__Test_AtomicAddress_Swap", scope: !4, file: !3, line: 378, type: !6, scopeLine: 378, unit: !2, spFlags: DISPFlagDefinition)
!124 = distinct !DISubprogram(name: "Main__Test_AtomicAddress", linkageName: "Main__Test_AtomicAddress", scope: !4, file: !3, line: 383, type: !6, scopeLine: 383, unit: !2, spFlags: DISPFlagDefinition)
!126 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_Fence", linkageName: "Main__Test_AtomicInteger_Fence", scope: !4, file: !3, line: 402, type: !6, scopeLine: 402, unit: !2, spFlags: DISPFlagDefinition)
!128 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_CompareSwap", linkageName: "Main__Test_AtomicInteger_CompareSwap", scope: !4, file: !3, line: 407, type: !6, scopeLine: 407, unit: !2, spFlags: DISPFlagDefinition)
!130 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_FetchAnd", linkageName: "Main__Test_AtomicInteger_FetchAnd", scope: !4, file: !3, line: 413, type: !6, scopeLine: 413, unit: !2, spFlags: DISPFlagDefinition)
!132 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_FetchDec", linkageName: "Main__Test_AtomicInteger_FetchDec", scope: !4, file: !3, line: 418, type: !6, scopeLine: 418, unit: !2, spFlags: DISPFlagDefinition)
!134 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_FetchInc", linkageName: "Main__Test_AtomicInteger_FetchInc", scope: !4, file: !3, line: 423, type: !6, scopeLine: 423, unit: !2, spFlags: DISPFlagDefinition)
!136 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_FetchOr", linkageName: "Main__Test_AtomicInteger_FetchOr", scope: !4, file: !3, line: 428, type: !6, scopeLine: 428, unit: !2, spFlags: DISPFlagDefinition)
!138 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_FetchXor", linkageName: "Main__Test_AtomicInteger_FetchXor", scope: !4, file: !3, line: 433, type: !6, scopeLine: 433, unit: !2, spFlags: DISPFlagDefinition)
!140 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_IsLockFree", linkageName: "Main__Test_AtomicInteger_IsLockFree", scope: !4, file: !3, line: 438, type: !6, scopeLine: 438, unit: !2, spFlags: DISPFlagDefinition)
!142 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_LoadStore", linkageName: "Main__Test_AtomicInteger_LoadStore", scope: !4, file: !3, line: 443, type: !6, scopeLine: 443, unit: !2, spFlags: DISPFlagDefinition)
!144 = distinct !DISubprogram(name: "Main__Test_AtomicInteger_Swap", linkageName: "Main__Test_AtomicInteger_Swap", scope: !4, file: !3, line: 455, type: !6, scopeLine: 455, unit: !2, spFlags: DISPFlagDefinition)
!146 = distinct !DISubprogram(name: "Main__Test_AtomicInteger", linkageName: "Main__Test_AtomicInteger", scope: !4, file: !3, line: 460, type: !6, scopeLine: 460, unit: !2, spFlags: DISPFlagDefinition)
!148 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_Fence", linkageName: "Main__Test_AtomicLongint_Fence", scope: !4, file: !3, line: 477, type: !6, scopeLine: 477, unit: !2, spFlags: DISPFlagDefinition)
!150 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_CompareSwap", linkageName: "Main__Test_AtomicLongint_CompareSwap", scope: !4, file: !3, line: 482, type: !6, scopeLine: 482, unit: !2, spFlags: DISPFlagDefinition)
!152 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_FetchAnd", linkageName: "Main__Test_AtomicLongint_FetchAnd", scope: !4, file: !3, line: 487, type: !6, scopeLine: 487, unit: !2, spFlags: DISPFlagDefinition)
!154 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_FetchDec", linkageName: "Main__Test_AtomicLongint_FetchDec", scope: !4, file: !3, line: 492, type: !6, scopeLine: 492, unit: !2, spFlags: DISPFlagDefinition)
!156 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_FetchInc", linkageName: "Main__Test_AtomicLongint_FetchInc", scope: !4, file: !3, line: 497, type: !6, scopeLine: 497, unit: !2, spFlags: DISPFlagDefinition)
!158 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_FetchOr", linkageName: "Main__Test_AtomicLongint_FetchOr", scope: !4, file: !3, line: 502, type: !6, scopeLine: 502, unit: !2, spFlags: DISPFlagDefinition)
!160 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_FetchXor", linkageName: "Main__Test_AtomicLongint_FetchXor", scope: !4, file: !3, line: 507, type: !6, scopeLine: 507, unit: !2, spFlags: DISPFlagDefinition)
!162 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_IsLockFree", linkageName: "Main__Test_AtomicLongint_IsLockFree", scope: !4, file: !3, line: 512, type: !6, scopeLine: 512, unit: !2, spFlags: DISPFlagDefinition)
!164 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_Load", linkageName: "Main__Test_AtomicLongint_Load", scope: !4, file: !3, line: 517, type: !6, scopeLine: 517, unit: !2, spFlags: DISPFlagDefinition)
!166 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_Store", linkageName: "Main__Test_AtomicLongint_Store", scope: !4, file: !3, line: 522, type: !6, scopeLine: 522, unit: !2, spFlags: DISPFlagDefinition)
!168 = distinct !DISubprogram(name: "Main__Test_AtomicLongint_Swap", linkageName: "Main__Test_AtomicLongint_Swap", scope: !4, file: !3, line: 527, type: !6, scopeLine: 527, unit: !2, spFlags: DISPFlagDefinition)
!170 = distinct !DISubprogram(name: "Main__Test_AtomicLongint", linkageName: "Main__Test_AtomicLongint", scope: !4, file: !3, line: 533, type: !6, scopeLine: 533, unit: !2, spFlags: DISPFlagDefinition)
!17 = !DILocation(line: 0, column: 0, scope: !16)
!19 = !DILocation(line: 0, column: 0, scope: !18)
!21 = !DILocation(line: 0, column: 0, scope: !20)
!23 = !DILocation(line: 0, column: 0, scope: !22)
!25 = !DILocation(line: 0, column: 0, scope: !24)
!27 = !DILocation(line: 0, column: 0, scope: !26)
!29 = !DILocation(line: 0, column: 0, scope: !28)
!31 = !DILocation(line: 0, column: 0, scope: !30)
!33 = !DILocation(line: 0, column: 0, scope: !32)
!35 = !DILocation(line: 0, column: 0, scope: !34)
!37 = !DILocation(line: 0, column: 0, scope: !36)
!39 = !DILocation(line: 0, column: 0, scope: !38)
!41 = !DILocation(line: 0, column: 0, scope: !40)
!43 = !DILocation(line: 0, column: 0, scope: !42)
!45 = !DILocation(line: 0, column: 0, scope: !44)
!47 = !DILocation(line: 0, column: 0, scope: !46)
!49 = !DILocation(line: 0, column: 0, scope: !48)
!51 = !DILocation(line: 0, column: 0, scope: !50)
!53 = !DILocation(line: 0, column: 0, scope: !52)
!55 = !DILocation(line: 0, column: 0, scope: !54)
!57 = !DILocation(line: 0, column: 0, scope: !56)
!59 = !DILocation(line: 0, column: 0, scope: !58)
!61 = !DILocation(line: 0, column: 0, scope: !60)
!63 = !DILocation(line: 0, column: 0, scope: !62)
!65 = !DILocation(line: 0, column: 0, scope: !64)
!67 = !DILocation(line: 0, column: 0, scope: !66)
!69 = !DILocation(line: 0, column: 0, scope: !68)
!71 = !DILocation(line: 0, column: 0, scope: !70)
!73 = !DILocation(line: 0, column: 0, scope: !72)
!75 = !DILocation(line: 0, column: 0, scope: !74)
!77 = !DILocation(line: 0, column: 0, scope: !76)
!79 = !DILocation(line: 0, column: 0, scope: !78)
!81 = !DILocation(line: 0, column: 0, scope: !80)
!83 = !DILocation(line: 0, column: 0, scope: !82)
!85 = !DILocation(line: 0, column: 0, scope: !84)
!87 = !DILocation(line: 0, column: 0, scope: !86)
!89 = !DILocation(line: 0, column: 0, scope: !88)
!91 = !DILocation(line: 0, column: 0, scope: !90)
!93 = !DILocation(line: 0, column: 0, scope: !92)
!95 = !DILocation(line: 0, column: 0, scope: !94)
!97 = !DILocation(line: 0, column: 0, scope: !96)
!99 = !DILocation(line: 0, column: 0, scope: !98)
!101 = !DILocation(line: 0, column: 0, scope: !100)
!103 = !DILocation(line: 0, column: 0, scope: !102)
!105 = !DILocation(line: 0, column: 0, scope: !104)
!107 = !DILocation(line: 0, column: 0, scope: !106)
!109 = !DILocation(line: 0, column: 0, scope: !108)
!111 = !DILocation(line: 0, column: 0, scope: !110)
!113 = !DILocation(line: 0, column: 0, scope: !112)
!115 = !DILocation(line: 0, column: 0, scope: !114)
!117 = !DILocation(line: 0, column: 0, scope: !116)
!119 = !DILocation(line: 0, column: 0, scope: !118)
!121 = !DILocation(line: 0, column: 0, scope: !120)
!123 = !DILocation(line: 0, column: 0, scope: !122)
!125 = !DILocation(line: 0, column: 0, scope: !124)
!127 = !DILocation(line: 0, column: 0, scope: !126)
!129 = !DILocation(line: 0, column: 0, scope: !128)
!131 = !DILocation(line: 0, column: 0, scope: !130)
!133 = !DILocation(line: 0, column: 0, scope: !132)
!135 = !DILocation(line: 0, column: 0, scope: !134)
!137 = !DILocation(line: 0, column: 0, scope: !136)
!139 = !DILocation(line: 0, column: 0, scope: !138)
!141 = !DILocation(line: 0, column: 0, scope: !140)
!143 = !DILocation(line: 0, column: 0, scope: !142)
!145 = !DILocation(line: 0, column: 0, scope: !144)
!147 = !DILocation(line: 0, column: 0, scope: !146)
!149 = !DILocation(line: 0, column: 0, scope: !148)
!151 = !DILocation(line: 0, column: 0, scope: !150)
!153 = !DILocation(line: 0, column: 0, scope: !152)
!155 = !DILocation(line: 0, column: 0, scope: !154)
!157 = !DILocation(line: 0, column: 0, scope: !156)
!159 = !DILocation(line: 0, column: 0, scope: !158)
!161 = !DILocation(line: 0, column: 0, scope: !160)
!163 = !DILocation(line: 0, column: 0, scope: !162)
!165 = !DILocation(line: 0, column: 0, scope: !164)
!167 = !DILocation(line: 0, column: 0, scope: !166)
!169 = !DILocation(line: 0, column: 0, scope: !168)
!171 = !DILocation(line: 0, column: 0, scope: !170)
!172 = !DILocalVariable(name: "_result", scope: !30, file: !3, line: 58, type: !7)
!173 = !DILocalVariable(name: "_result", scope: !52, file: !3, line: 130, type: !7)
!174 = !DILocalVariable(name: "_result", scope: !74, file: !3, line: 206, type: !7)
!175 = !DILocalVariable(name: "integerC", scope: !76, file: !3, line: 211, type: !7)
!176 = !DILocalVariable(name: "_result", scope: !96, file: !3, line: 281, type: !7)
!177 = !DILocalVariable(name: "xxx", scope: !98, file: !3, line: 286, type: !15)
!178 = !DILocalVariable(name: "refanyC", scope: !98, file: !3, line: 286, type: !15)
!179 = !DILocalVariable(name: "_result", scope: !118, file: !3, line: 361, type: !7)
!180 = !DILocalVariable(name: "addressC", scope: !120, file: !3, line: 366, type: !15)
!181 = !DILocalVariable(name: "_result", scope: !140, file: !3, line: 438, type: !7)
!182 = !DILocalVariable(name: "integerC", scope: !142, file: !3, line: 443, type: !7)
!183 = !DILocalVariable(name: "_result", scope: !162, file: !3, line: 512, type: !7)
!184 = !DILocation(line: 25, column: 0, scope: !16)
!185 = !DILocation(line: 30, column: 0, scope: !18)
!186 = !DILocation(line: 35, column: 0, scope: !20)
!187 = !DILocation(line: 40, column: 0, scope: !22)
!188 = !DILocation(line: 45, column: 0, scope: !24)
!189 = !DILocation(line: 50, column: 0, scope: !26)
!190 = !DILocation(line: 55, column: 0, scope: !28)
!191 = !DILocation(line: 58, column: 0, scope: !30)
!192 = !DILocation(line: 60, column: 0, scope: !30)
!193 = !DILocation(line: 65, column: 0, scope: !32)
!194 = !DILocation(line: 66, column: 0, scope: !32)
!195 = !DILocation(line: 67, column: 0, scope: !32)
!196 = !DILocation(line: 68, column: 0, scope: !32)
!197 = !DILocation(line: 70, column: 0, scope: !32)
!198 = !DILocation(line: 71, column: 0, scope: !32)
!199 = !DILocation(line: 76, column: 0, scope: !34)
!200 = !DILocation(line: 81, column: 0, scope: !36)
!201 = !DILocation(line: 82, column: 0, scope: !36)
!202 = !DILocation(line: 83, column: 0, scope: !36)
!203 = !DILocation(line: 84, column: 0, scope: !36)
!204 = !DILocation(line: 85, column: 0, scope: !36)
!205 = !DILocation(line: 86, column: 0, scope: !36)
!206 = !DILocation(line: 87, column: 0, scope: !36)
!207 = !DILocation(line: 88, column: 0, scope: !36)
!208 = !DILocation(line: 89, column: 0, scope: !36)
!209 = !DILocation(line: 90, column: 0, scope: !36)
!210 = !DILocation(line: 97, column: 0, scope: !38)
!211 = !DILocation(line: 102, column: 0, scope: !40)
!212 = !DILocation(line: 107, column: 0, scope: !42)
!213 = !DILocation(line: 112, column: 0, scope: !44)
!214 = !DILocation(line: 117, column: 0, scope: !46)
!215 = !DILocation(line: 122, column: 0, scope: !48)
!216 = !DILocation(line: 127, column: 0, scope: !50)
!217 = !DILocation(line: 130, column: 0, scope: !52)
!218 = !DILocation(line: 132, column: 0, scope: !52)
!219 = !DILocation(line: 137, column: 0, scope: !54)
!220 = !DILocation(line: 138, column: 0, scope: !54)
!221 = !DILocation(line: 139, column: 0, scope: !54)
!222 = !DILocation(line: 140, column: 0, scope: !54)
!223 = !DILocation(line: 142, column: 0, scope: !54)
!224 = !DILocation(line: 143, column: 0, scope: !54)
!225 = !DILocation(line: 148, column: 0, scope: !56)
!226 = !DILocation(line: 153, column: 0, scope: !58)
!227 = !DILocation(line: 154, column: 0, scope: !58)
!228 = !DILocation(line: 155, column: 0, scope: !58)
!229 = !DILocation(line: 156, column: 0, scope: !58)
!230 = !DILocation(line: 157, column: 0, scope: !58)
!231 = !DILocation(line: 158, column: 0, scope: !58)
!232 = !DILocation(line: 159, column: 0, scope: !58)
!233 = !DILocation(line: 160, column: 0, scope: !58)
!234 = !DILocation(line: 161, column: 0, scope: !58)
!235 = !DILocation(line: 162, column: 0, scope: !58)
!236 = !DILocation(line: 173, column: 0, scope: !60)
!237 = !DILocation(line: 178, column: 0, scope: !62)
!238 = !DILocation(line: 183, column: 0, scope: !64)
!239 = !DILocation(line: 188, column: 0, scope: !66)
!240 = !DILocation(line: 193, column: 0, scope: !68)
!241 = !DILocation(line: 198, column: 0, scope: !70)
!242 = !DILocation(line: 203, column: 0, scope: !72)
!243 = !DILocation(line: 206, column: 0, scope: !74)
!244 = !DILocation(line: 208, column: 0, scope: !74)
!245 = !DILocation(line: 211, column: 0, scope: !76)
!246 = !DILocation(line: 214, column: 0, scope: !76)
!247 = !DILocation(line: 215, column: 0, scope: !76)
!248 = !DILocation(line: 216, column: 0, scope: !76)
!249 = !DILocation(line: 217, column: 0, scope: !76)
!250 = !DILocation(line: 219, column: 0, scope: !76)
!251 = !DILocation(line: 220, column: 0, scope: !76)
!252 = !DILocation(line: 225, column: 0, scope: !78)
!253 = !DILocation(line: 230, column: 0, scope: !80)
!254 = !DILocation(line: 231, column: 0, scope: !80)
!255 = !DILocation(line: 232, column: 0, scope: !80)
!256 = !DILocation(line: 233, column: 0, scope: !80)
!257 = !DILocation(line: 234, column: 0, scope: !80)
!258 = !DILocation(line: 235, column: 0, scope: !80)
!259 = !DILocation(line: 236, column: 0, scope: !80)
!260 = !DILocation(line: 237, column: 0, scope: !80)
!261 = !DILocation(line: 238, column: 0, scope: !80)
!262 = !DILocation(line: 239, column: 0, scope: !80)
!263 = !DILocation(line: 248, column: 0, scope: !82)
!264 = !DILocation(line: 253, column: 0, scope: !84)
!265 = !DILocation(line: 257, column: 0, scope: !86)
!266 = !DILocation(line: 262, column: 0, scope: !88)
!267 = !DILocation(line: 267, column: 0, scope: !90)
!268 = !DILocation(line: 272, column: 0, scope: !92)
!269 = !DILocation(line: 277, column: 0, scope: !94)
!270 = !DILocation(line: 281, column: 0, scope: !96)
!271 = !DILocation(line: 283, column: 0, scope: !96)
!272 = !DILocation(line: 286, column: 0, scope: !98)
!273 = !DILocation(line: 293, column: 0, scope: !98)
!274 = !DILocation(line: 294, column: 0, scope: !98)
!275 = !DILocation(line: 295, column: 0, scope: !98)
!276 = !DILocation(line: 296, column: 0, scope: !98)
!277 = !DILocation(line: 297, column: 0, scope: !98)
!278 = !DILocation(line: 299, column: 0, scope: !98)
!279 = !DILocation(line: 300, column: 0, scope: !98)
!280 = !DILocation(line: 305, column: 0, scope: !100)
!281 = !DILocation(line: 310, column: 0, scope: !102)
!282 = !DILocation(line: 311, column: 0, scope: !102)
!283 = !DILocation(line: 312, column: 0, scope: !102)
!284 = !DILocation(line: 313, column: 0, scope: !102)
!285 = !DILocation(line: 314, column: 0, scope: !102)
!286 = !DILocation(line: 315, column: 0, scope: !102)
!287 = !DILocation(line: 316, column: 0, scope: !102)
!288 = !DILocation(line: 317, column: 0, scope: !102)
!289 = !DILocation(line: 318, column: 0, scope: !102)
!290 = !DILocation(line: 319, column: 0, scope: !102)
!291 = !DILocation(line: 328, column: 0, scope: !104)
!292 = !DILocation(line: 333, column: 0, scope: !106)
!293 = !DILocation(line: 337, column: 0, scope: !108)
!294 = !DILocation(line: 342, column: 0, scope: !110)
!295 = !DILocation(line: 347, column: 0, scope: !112)
!296 = !DILocation(line: 352, column: 0, scope: !114)
!297 = !DILocation(line: 357, column: 0, scope: !116)
!298 = !DILocation(line: 361, column: 0, scope: !118)
!299 = !DILocation(line: 363, column: 0, scope: !118)
!300 = !DILocation(line: 366, column: 0, scope: !120)
!301 = !DILocation(line: 369, column: 0, scope: !120)
!302 = !DILocation(line: 370, column: 0, scope: !120)
!303 = !DILocation(line: 371, column: 0, scope: !120)
!304 = !DILocation(line: 372, column: 0, scope: !120)
!305 = !DILocation(line: 374, column: 0, scope: !120)
!306 = !DILocation(line: 375, column: 0, scope: !120)
!307 = !DILocation(line: 380, column: 0, scope: !122)
!308 = !DILocation(line: 385, column: 0, scope: !124)
!309 = !DILocation(line: 386, column: 0, scope: !124)
!310 = !DILocation(line: 387, column: 0, scope: !124)
!311 = !DILocation(line: 388, column: 0, scope: !124)
!312 = !DILocation(line: 389, column: 0, scope: !124)
!313 = !DILocation(line: 390, column: 0, scope: !124)
!314 = !DILocation(line: 391, column: 0, scope: !124)
!315 = !DILocation(line: 392, column: 0, scope: !124)
!316 = !DILocation(line: 393, column: 0, scope: !124)
!317 = !DILocation(line: 394, column: 0, scope: !124)
!318 = !DILocation(line: 404, column: 0, scope: !126)
!319 = !DILocation(line: 409, column: 0, scope: !128)
!320 = !DILocation(line: 410, column: 0, scope: !128)
!321 = !DILocation(line: 415, column: 0, scope: !130)
!322 = !DILocation(line: 420, column: 0, scope: !132)
!323 = !DILocation(line: 425, column: 0, scope: !134)
!324 = !DILocation(line: 430, column: 0, scope: !136)
!325 = !DILocation(line: 435, column: 0, scope: !138)
!326 = !DILocation(line: 438, column: 0, scope: !140)
!327 = !DILocation(line: 440, column: 0, scope: !140)
!328 = !DILocation(line: 443, column: 0, scope: !142)
!329 = !DILocation(line: 446, column: 0, scope: !142)
!330 = !DILocation(line: 447, column: 0, scope: !142)
!331 = !DILocation(line: 448, column: 0, scope: !142)
!332 = !DILocation(line: 449, column: 0, scope: !142)
!333 = !DILocation(line: 451, column: 0, scope: !142)
!334 = !DILocation(line: 452, column: 0, scope: !142)
!335 = !DILocation(line: 457, column: 0, scope: !144)
!336 = !DILocation(line: 462, column: 0, scope: !146)
!337 = !DILocation(line: 463, column: 0, scope: !146)
!338 = !DILocation(line: 464, column: 0, scope: !146)
!339 = !DILocation(line: 465, column: 0, scope: !146)
!340 = !DILocation(line: 466, column: 0, scope: !146)
!341 = !DILocation(line: 467, column: 0, scope: !146)
!342 = !DILocation(line: 468, column: 0, scope: !146)
!343 = !DILocation(line: 469, column: 0, scope: !146)
!344 = !DILocation(line: 470, column: 0, scope: !146)
!345 = !DILocation(line: 471, column: 0, scope: !146)
!346 = !DILocation(line: 479, column: 0, scope: !148)
!347 = !DILocation(line: 484, column: 0, scope: !150)
!348 = !DILocation(line: 489, column: 0, scope: !152)
!349 = !DILocation(line: 494, column: 0, scope: !154)
!350 = !DILocation(line: 499, column: 0, scope: !156)
!351 = !DILocation(line: 504, column: 0, scope: !158)
!352 = !DILocation(line: 509, column: 0, scope: !160)
!353 = !DILocation(line: 512, column: 0, scope: !162)
!354 = !DILocation(line: 514, column: 0, scope: !162)
!355 = !DILocation(line: 519, column: 0, scope: !164)
!356 = !DILocation(line: 524, column: 0, scope: !166)
!357 = !DILocation(line: 529, column: 0, scope: !168)
!358 = !DILocation(line: 535, column: 0, scope: !170)
!359 = !DILocation(line: 536, column: 0, scope: !170)
!360 = !DILocation(line: 537, column: 0, scope: !170)
!361 = !DILocation(line: 538, column: 0, scope: !170)
!362 = !DILocation(line: 539, column: 0, scope: !170)
!363 = !DILocation(line: 540, column: 0, scope: !170)
!364 = !DILocation(line: 541, column: 0, scope: !170)
!365 = !DILocation(line: 542, column: 0, scope: !170)
!366 = !DILocation(line: 543, column: 0, scope: !170)
!367 = !DILocation(line: 544, column: 0, scope: !170)
!368 = !DILocation(line: 545, column: 0, scope: !170)
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
