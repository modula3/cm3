(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Hooks to various modules' init code... *)

UNSAFE INTERFACE RTLinkerX;

IMPORT RT0;

<*EXTERNAL*> PROCEDURE RT0_I3             (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTAllocCnts_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTAllocStats_I3    (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTAllocator_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTArgs_I3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTCollector_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTCollectorSRC_I3  (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTDebug_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTError_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTException_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeap_I3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapDebug_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapEvent_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapInfo_I3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapMap_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapRep_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapStats_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHooks_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTIO_I3            (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTLinker_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTLinkerX_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTMapOp_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTMisc_I3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTModule_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTOS_I3            (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTPacking_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTParams_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTProcedure_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTProcedureSRC_I3  (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTProcess_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTSignal_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTStack_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTThread_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTipe_I3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTType_I3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTypeFP_I3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTypeMap_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTypeSRC_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTWeakRef_I3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTutils_I3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE Word_I3            (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RT0_M3             (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTAllocStats_M3    (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTAllocator_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTCollector_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTDebug_M3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTError_M3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTException_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeap_M3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapDebug_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapInfo_M3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapMap_M3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapRep_M3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapStats_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHooks_M3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTIO_M3            (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTLinker_M3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTMapOp_M3         (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTMisc_M3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTModule_M3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTPacking_M3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTParams_M3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTProcedure_M3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTProcess_M3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTipe_M3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTType_M3          (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTypeFP_M3        (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTTypeMap_M3       (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTutils_M3         (mode: INTEGER): RT0.ModulePtr;

END RTLinkerX.
