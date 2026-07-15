; ModuleID = 'I60'
source_filename = "I60"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0"

@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c"6_M3Exc\00", align 1
@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64), i64 -9223372036854775808) to ptr) }, align 8
declare i32 @__gxx_personality_v0(...)


define void @I60__I60_I3() personality ptr @__gxx_personality_v0 {
entry:
  ret void
}

define ptr @I60_I3(i64 %mode) {
entry:
  %do_body = icmp ne i64 %mode, 0
  br i1 %do_body, label %run, label %done
run:
  call void @I60__I60_I3()
  br label %done
done:
  ret ptr @I60_M3_info
}

; RT0.ImportInfo chain for I60
declare ptr @Main_M3(i64)
@I60_M3_imp.0 = internal global { ptr, ptr, ptr } { ptr null, ptr @Main_M3, ptr null }

; RT0.ModuleInfo for I60 (13 fields, 104 bytes)
%RT0_ModuleInfo_t = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64 }
@I60_M3_info = internal global %RT0_ModuleInfo_t {
  ptr null,  ; file (+0)
  ptr null,  ; type_cells (+8)
  ptr null,  ; type_cell_ptrs (+16)
  ptr null,  ; full_rev (+24)
  ptr null,  ; part_rev (+32)
  ptr null,  ; proc_info (+40)
  ptr null,  ; try_scopes (+48)
  ptr null,  ; var_map (+56)
  ptr null,  ; gc_map (+64)
  ptr @I60_M3_imp.0,  ; imports (+72)
  i64 0,  ; link_state (+80)
  ptr @I60_I3,  ; binder (+88)
  i64 3  ; gc_flags (+96)
}
