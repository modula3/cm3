; ModuleID = 'VBT'
source_filename = "VBT"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


define void @VBT__VBT_I3() personality ptr @__gxx_personality_v0 {
entry:
  ret void
}

; TypeCell / ObjectTypeCell globals
%TC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }
%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }
%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }
@tc_ref_-316869099 = internal global %TC_t {
  i64 0,
  i64 -316869099,
  i64 u0x11188180fc047795,
  i8 1,
  i8 1,
  i8 0,
  i8 2,
  [4 x i8] zeroinitializer,
  i64 2,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_1201135579
}
@tc_ref_1201135579 = internal global %TC_t {
  i64 0,
  i64 1201135579,
  i64 u0x1e69910059fe4edb,
  i8 1,
  i8 1,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 136,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_ref_-1238830020
}
@tc_ref_-1238830020 = internal global %TC_t {
  i64 0,
  i64 -1238830020,
  i64 u0x1b22b41bad0a4027,
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
  ptr @tc_ref_184915390
}
@tc_ref_184915390 = internal global %TC_t {
  i64 0,
  i64 184915390,
  i64 u0x0c9432f40791a74a,
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
  ptr @tc_ref_2135113721
}
@tc_ref_2135113721 = internal global %TC_t {
  i64 0,
  i64 2135113721,
  i64 u0x100940186f4a7fe1,
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
  ptr @tc_ref_-932085082
}
@tc_ref_-932085082 = internal global %TC_t {
  i64 0,
  i64 -932085082,
  i64 u0x1c5a3906d42bbba0,
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
  ptr @tc_obj_874473023
}
@tc_obj_874473023.tc_name = private unnamed_addr constant [10 x i8] c"VBT.VBT_T\00"
@tc_obj_874473023 = internal global %OTC_t {
  i64 0,
  i64 874473023,
  i64 u0x063694b03229f28f,
  i8 1,
  i8 2,
  i8 0,
  i8 8,
  [4 x i8] zeroinitializer,
  i64 0,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr null,
  ptr @tc_obj_874473023.tc_name,
  ptr null,
  i64 -1651526519,
  ptr null,
  i64 0,
  i64 0,
  i64 8,
  ptr null,
  ptr null
}

; RT0.ModuleInfo for VBT (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@VBT_M3_info = internal global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr @tc_ref_-316869099,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr null,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @VBT_I3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}

define ptr @VBT_I3(i64 %mode) personality ptr @__gxx_personality_v0 {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @VBT__VBT_I3()
  br label %done
done:
  ret ptr @VBT_M3_info
}
