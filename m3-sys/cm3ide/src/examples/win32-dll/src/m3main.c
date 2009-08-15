typedef long  _INTEGER;
typedef char* _ADDRESS;
typedef void* (*_BIND_PROC)();

typedef struct link_info {
  _INTEGER n_modules;
  _ADDRESS modules;
  _INTEGER argc;
  _ADDRESS argv;
  _ADDRESS envp;
  _ADDRESS instance;
  _ADDRESS bottom_of_stack;
  _ADDRESS top_of_stack;
} _LINK_INFO;


struct {
  struct { int Bundle; } BundleRep;
  struct { int DateWin32; } Date;
  struct { int FSWin32; int FS; } FS;
  struct { int FmtBuf; } FmtBufF;
  struct { int FmtBuf; } FmtBufTest;
  struct { int MProperty; } MPropertyF;
  struct { int MachineIDWin32; } MachineID;
  struct { int OSErrorWin32; } OSError;
  struct { int Path; } PathPrivate;
  struct { int PathnameWin32; } Pathname;
  struct { int Pickle2; } PickleRd;
  struct { int PipeWin32; int Pipe; } Pipe;
  struct { int ProcessWin32; } Process;
  struct { int Property; } PropertyF;
  struct { int RTCollector; } RTCollectorSRC;
  struct { int RTException; } RTExRep;
  struct { int RTHeapRep; int RTCollector; } RTHeapRep;
  struct { int ThreadWin32; int RTHooks; int RTAllocator; } RTHooks;
  struct { int RTProcedure; } RTProcedureSRC;
  struct { int ThreadWin32; } RTThreadInit;
  struct { int RTType; } RTTypeSRC;
  struct { int RTCollector; } RTWeakRef;
  struct { int RdMove; int RdImpl; } Rd;
  struct { int RdMove; } RdClass;
  struct { int ThreadWin32; } Scheduler;
  struct { int UnsafeHash; int Text; } Text;
  struct { int Text; } TextF;
  struct { int ThreadWin32; } Thread;
  struct { int ThreadWin32; } ThreadF;
  struct { int TickPortable; } Tick;
  struct { int TimeWin32; } Time;
  struct { int RdMove; } UnsafeRd;
  struct { int WrMove; } UnsafeWr;
  struct { int WrWin32; int WrMove; } Wr;
  struct { int WrMove; } WrClass;
} * _m3_exporters;

extern void* I_ASCII();
extern void* M_ASCII();
extern void* M_Atom();
extern void* I_Atom();
extern void* I_AtomAtomTbl();
extern void* M_AtomAtomTbl();
extern void* M_AtomIntTbl();
extern void* I_AtomIntTbl();
extern void* I_AtomList();
extern void* M_AtomList();
extern void* I_AtomListSort();
extern void* M_AtomListSort();
extern void* I_AtomPQ();
extern void* M_AtomPQ();
extern void* I_AtomPQRep();
extern void* M_AtomRefTbl();
extern void* I_AtomRefTbl();
extern void* I_AtomSeq();
extern void* M_AtomSeq();
extern void* I_AtomSeqRep();
extern void* M_AtomTextTbl();
extern void* I_AtomTextTbl();
extern void* I_AutoFlushWr();
extern void* M_AutoFlushWr();
extern void* M_Axis();
extern void* I_Axis();
extern void* I_BasicCtypes();
extern void* I_Boolean();
extern void* M_Boolean();
extern void* M_Bundle();
extern void* I_Bundle();
extern void* I_BundleRep();
extern void* I_CConvert();
extern void* M_CConvert();
extern void* M_Capability();
extern void* I_Capability();
extern void* I_Char();
extern void* M_Char();
extern void* M_Convert();
extern void* I_Convert();
extern void* M_ConvertPacking();
extern void* I_ConvertPacking();
extern void* I_Csetjmp();
extern void* I_Cstddef();
extern void* I_Cstdlib();
extern void* I_Cstring();
extern void* I_Ctypes();
extern void* I_Date();
extern void* M_DateWin32();
extern void* I_DragonInt();
extern void* M_DragonInt();
extern void* M_DragonT();
extern void* I_DragonT();
extern void* I_ETimer();
extern void* M_ETimer();
extern void* M_Env();
extern void* I_Env();
extern void* I_Extended();
extern void* M_Extended();
extern void* M_ExtendedFloat();
extern void* I_ExtendedFloat();
extern void* I_FPU();
extern void* M_FPU();
extern void* M_FS();
extern void* I_FS();
extern void* M_FSWin32();
extern void* I_File();
extern void* I_FileRd();
extern void* M_FileRd();
extern void* I_FileWin32();
extern void* M_FileWin32();
extern void* I_FileWr();
extern void* M_FileWr();
extern void* I_Fingerprint();
extern void* M_Fingerprint();
extern void* M_FloatMode();
extern void* I_FloatMode();
extern void* I_Fmt();
extern void* M_Fmt();
extern void* I_FmtBuf();
extern void* M_FmtBuf();
extern void* I_FmtBufF();
extern void* I_FmtBufTest();
extern void* M_Formatter();
extern void* I_Formatter();
extern void* M_IEEESpecial();
extern void* I_IEEESpecial();
extern void* I_IO();
extern void* M_IO();
extern void* M_Int32();
extern void* I_Int32();
extern void* I_IntArraySort();
extern void* M_IntArraySort();
extern void* M_IntAtomTbl();
extern void* I_IntAtomTbl();
extern void* I_IntIntTbl();
extern void* M_IntIntTbl();
extern void* M_IntList();
extern void* I_IntList();
extern void* M_IntListSort();
extern void* I_IntListSort();
extern void* M_IntPQ();
extern void* I_IntPQ();
extern void* I_IntPQRep();
extern void* I_IntRefTbl();
extern void* M_IntRefTbl();
extern void* M_IntSeq();
extern void* I_IntSeq();
extern void* I_IntSeqRep();
extern void* M_IntTextTbl();
extern void* I_IntTextTbl();
extern void* M_Integer();
extern void* I_Integer();
extern void* M_Interval();
extern void* I_Interval();
extern void* I_Lex();
extern void* M_Lex();
extern void* M_LongFloat();
extern void* I_LongFloat();
extern void* I_LongFloatExtras();
extern void* M_LongFloatExtras();
extern void* I_LongReal();
extern void* M_LongReal();
extern void* I_LongRealRep();
extern void* M_LongSqrt();
extern void* I_LongSqrt();
extern void* I_LongrealType();
extern void* M_LongrealType();
extern void* M_LowPerfTool();
extern void* I_LowPerfTool();
extern void* I_M3Config();
extern void* I_M3_BUILTIN();
extern void* M_M3toC();
extern void* I_M3toC();
extern void* I_MProperty();
extern void* M_MProperty();
extern void* I_MPropertyF();
extern void* I_MachineID();
extern void* M_MachineIDWin32();
extern void* I_Math();
extern void* I_MsgRd();
extern void* I_MsgWr();
extern void* I_NB30();
extern void* M_NB30();
extern void* I_NullRd();
extern void* M_NullRd();
extern void* M_NullWr();
extern void* I_NullWr();
extern void* I_OSError();
extern void* M_OSErrorWin32();
extern void* I_OSErrorWin32();
extern void* I_OSWin32();
extern void* M_OSWin32();
extern void* I_PackingTbl();
extern void* M_PackingTbl();
extern void* I_PackingTypeCode();
extern void* M_PackingTypeCode();
extern void* M_Params();
extern void* I_Params();
extern void* M_Path();
extern void* I_Path();
extern void* I_PathPrivate();
extern void* I_Pathname();
extern void* M_PathnameWin32();
extern void* M_PerfTool();
extern void* I_PerfTool();
extern void* I_Pickle();
extern void* M_Pickle();
extern void* M_Pickle2();
extern void* I_Pickle2();
extern void* I_PickleRd();
extern void* I_PickleStubs();
extern void* M_PickleStubs();
extern void* I_Pipe();
extern void* M_Pipe();
extern void* M_PipeWin32();
extern void* I_PklAction();
extern void* I_PklActionSeq();
extern void* M_PklActionSeq();
extern void* I_PklActionSeqRep();
extern void* I_PklTipeMap();
extern void* M_PklTipeMap();
extern void* I_Point();
extern void* M_Point();
extern void* I_Poly();
extern void* M_Poly();
extern void* I_PolyBasis();
extern void* M_PolyBasis();
extern void* I_PolyRegion();
extern void* M_PolyRegion();
extern void* I_Process();
extern void* M_ProcessWin32();
extern void* I_Property();
extern void* M_Property();
extern void* I_PropertyF();
extern void* I_PropertyV();
extern void* M_PropertyV();
extern void* M_RT0();
extern void* I_RT0();
extern void* M_RT0u();
extern void* I_RT0u();
extern void* M_RTAllocator();
extern void* I_RTAllocator();
extern void* I_RTArgs();
extern void* M_RTArgs();
extern void* I_RTCollector();
extern void* M_RTCollector();
extern void* I_RTCollectorSRC();
extern void* I_RTExRep();
extern void* M_RTException();
extern void* I_RTException();
extern void* M_RTHeap();
extern void* I_RTHeap();
extern void* M_RTHeapDep();
extern void* I_RTHeapDep();
extern void* I_RTHeapEvent();
extern void* I_RTHeapInfo();
extern void* M_RTHeapInfo();
extern void* I_RTHeapMap();
extern void* M_RTHeapMap();
extern void* M_RTHeapRep();
extern void* I_RTHeapRep();
extern void* M_RTHooks();
extern void* I_RTHooks();
extern void* I_RTIO();
extern void* M_RTIO();
extern void* I_RTLinker();
extern void* M_RTLinker();
extern void* I_RTMachine();
extern void* I_RTMain();
extern void* M_RTMain();
extern void* M_RTMapOp();
extern void* I_RTMapOp();
extern void* I_RTMisc();
extern void* M_RTMisc();
extern void* M_RTModule();
extern void* I_RTModule();
extern void* M_RTOS();
extern void* I_RTOS();
extern void* I_RTPacking();
extern void* M_RTPacking();
extern void* I_RTParams();
extern void* M_RTParams();
extern void* M_RTPerfTool();
extern void* I_RTPerfTool();
extern void* I_RTProcedure();
extern void* M_RTProcedure();
extern void* I_RTProcedureSRC();
extern void* I_RTProcess();
extern void* M_RTProcess();
extern void* M_RTSignal();
extern void* I_RTSignal();
extern void* I_RTThreadInit();
extern void* I_RTTipe();
extern void* M_RTTipe();
extern void* M_RTType();
extern void* I_RTType();
extern void* I_RTTypeFP();
extern void* M_RTTypeFP();
extern void* I_RTTypeMap();
extern void* M_RTTypeMap();
extern void* I_RTTypeSRC();
extern void* I_RTWeakRef();
extern void* I_Random();
extern void* M_Random();
extern void* I_RandomPerm();
extern void* M_RandomPerm();
extern void* I_RandomReal();
extern void* M_RandomReal();
extern void* I_Rd();
extern void* I_RdClass();
extern void* M_RdCopy();
extern void* I_RdCopy();
extern void* M_RdImpl();
extern void* M_RdMove();
extern void* I_RdUtils();
extern void* M_RdUtils();
extern void* I_Real();
extern void* M_Real();
extern void* M_RealFloat();
extern void* I_RealFloat();
extern void* M_RealFloatExtras();
extern void* I_RealFloatExtras();
extern void* I_RealRep();
extern void* M_RealSqrt();
extern void* I_RealSqrt();
extern void* I_RealType();
extern void* M_RealType();
extern void* I_Rect();
extern void* M_Rect();
extern void* M_RefAtomTbl();
extern void* I_RefAtomTbl();
extern void* I_RefIntTbl();
extern void* M_RefIntTbl();
extern void* I_RefList();
extern void* M_RefList();
extern void* I_RefListSort();
extern void* M_RefListSort();
extern void* I_RefPQ();
extern void* M_RefPQ();
extern void* I_RefPQRep();
extern void* M_RefRefTbl();
extern void* I_RefRefTbl();
extern void* I_RefSeq();
extern void* M_RefSeq();
extern void* I_RefSeqRep();
extern void* I_RefTextTbl();
extern void* M_RefTextTbl();
extern void* I_Refany();
extern void* M_Refany();
extern void* M_Region();
extern void* I_Region();
extern void* I_RegionRep();
extern void* I_RegularFile();
extern void* M_RegularFile();
extern void* M_Scan();
extern void* I_Scan();
extern void* I_Scheduler();
extern void* M_SortedAtomAtomTbl();
extern void* I_SortedAtomAtomTbl();
extern void* M_SortedAtomIntTbl();
extern void* I_SortedAtomIntTbl();
extern void* I_SortedAtomRefTbl();
extern void* M_SortedAtomRefTbl();
extern void* I_SortedAtomTextTbl();
extern void* M_SortedAtomTextTbl();
extern void* I_SortedIntAtomTbl();
extern void* M_SortedIntAtomTbl();
extern void* M_SortedIntIntTbl();
extern void* I_SortedIntIntTbl();
extern void* M_SortedIntRefTbl();
extern void* I_SortedIntRefTbl();
extern void* I_SortedIntTextTbl();
extern void* M_SortedIntTextTbl();
extern void* M_SortedRefAtomTbl();
extern void* I_SortedRefAtomTbl();
extern void* M_SortedRefIntTbl();
extern void* I_SortedRefIntTbl();
extern void* I_SortedRefRefTbl();
extern void* M_SortedRefRefTbl();
extern void* I_SortedRefTextTbl();
extern void* M_SortedRefTextTbl();
extern void* M_SortedTextAtomTbl();
extern void* I_SortedTextAtomTbl();
extern void* I_SortedTextIntTbl();
extern void* M_SortedTextIntTbl();
extern void* I_SortedTextRefTbl();
extern void* M_SortedTextRefTbl();
extern void* M_SortedTextTextTbl();
extern void* I_SortedTextTextTbl();
extern void* M_Stat();
extern void* I_Stat();
extern void* M_Stdio();
extern void* I_Stdio();
extern void* I_Swap();
extern void* M_Swap();
extern void* I_Sx();
extern void* M_Sx();
extern void* M_Terminal();
extern void* I_Terminal();
extern void* M_Text();
extern void* I_Text();
extern void* I_TextArraySort();
extern void* M_TextArraySort();
extern void* I_TextAtomTbl();
extern void* M_TextAtomTbl();
extern void* I_TextF();
extern void* M_TextIntTbl();
extern void* I_TextIntTbl();
extern void* M_TextList();
extern void* I_TextList();
extern void* I_TextListSort();
extern void* M_TextListSort();
extern void* M_TextPQ();
extern void* I_TextPQ();
extern void* I_TextPQRep();
extern void* M_TextRd();
extern void* I_TextRd();
extern void* I_TextRefTbl();
extern void* M_TextRefTbl();
extern void* M_TextSeq();
extern void* I_TextSeq();
extern void* I_TextSeqRep();
extern void* I_TextTextTbl();
extern void* M_TextTextTbl();
extern void* M_TextWr();
extern void* I_TextWr();
extern void* I_Thread();
extern void* I_ThreadContext();
extern void* I_ThreadF();
extern void* M_ThreadWin32();
extern void* I_Tick();
extern void* M_TickPortable();
extern void* I_Time();
extern void* M_TimeStamp();
extern void* I_TimeStamp();
extern void* I_TimeStampRep();
extern void* M_TimeWin32();
extern void* I_TimeWin32();
extern void* M_Transform();
extern void* I_Transform();
extern void* M_Trapezoid();
extern void* I_Trapezoid();
extern void* M_UnsafeHash();
extern void* I_UnsafeRd();
extern void* I_UnsafeWr();
extern void* I_WinBase();
extern void* I_WinBaseTypes();
extern void* I_WinCon();
extern void* M_WinDef();
extern void* I_WinDef();
extern void* I_WinError();
extern void* M_WinGDI();
extern void* I_WinGDI();
extern void* M_WinNT();
extern void* I_WinNT();
extern void* M_Word();
extern void* I_Word();
extern void* I_Wr();
extern void* I_WrClass();
extern void* M_WrMove();
extern void* M_WrWin32();

static _BIND_PROC _modules[] = {
  I_RTHeap,
  I_RTHeapInfo,
  I_RTSignal,
  I_RTAllocator,
  I_ThreadContext,
  I_WinGDI,
  I_RTPerfTool,
  I_RTTypeMap,
  I_RTMapOp,
  I_RTHeapMap,
  I_RTHeapEvent,
  I_RTWeakRef,
  I_RTCollectorSRC,
  I_RTCollector,
  I_RTArgs,
  I_RTParams,
  I_WinCon,
  I_RTOS,
  I_RTIO,
  I_RTExRep,
  I_RTException,
  I_RTProcess,
  I_RTModule,
  I_RTProcedureSRC,
  I_PolyBasis,
  I_Poly,
  I_Fingerprint,
  I_RTProcedure,
  I_RTMisc,
  I_RTTypeSRC,
  I_RTType,
  I_RT0u,
  I_Text,
  I_TextF,
  I_Cstring,
  I_Cstddef,
  I_Cstdlib,
  I_M3toC,
  I_WinDef,
  I_WinBaseTypes,
  I_WinNT,
  I_WinBase,
  I_BasicCtypes,
  I_Ctypes,
  I_Csetjmp,
  I_RTMachine,
  I_Word,
  I_RTHeapDep,
  I_RT0,
  I_RTHeapRep,
  I_RTThreadInit,
  I_ThreadF,
  I_Thread,
  I_Scheduler,
  I_RTHooks,
  I_RTLinker,
  I_RTMain,
  I_M3_BUILTIN,
  M_RTHeap,
  M_RTHeapInfo,
  M_RTSignal,
  M_RTLinker,
  M_RTMain,
  M_RTAllocator,
  M_RTHooks,
  M_WinGDI,
  M_RTPerfTool,
  M_RTMapOp,
  M_RTTypeMap,
  M_RTHeapMap,
  M_RTCollector,
  M_RTArgs,
  M_RTParams,
  M_RTOS,
  M_RTIO,
  M_RTException,
  M_RTProcess,
  M_RTModule,
  M_RTProcedure,
  M_PolyBasis,
  M_Poly,
  M_Fingerprint,
  M_RTMisc,
  M_RTType,
  M_RT0u,
  M_RTHeapRep,
  M_Text,
  M_UnsafeHash,
  M_M3toC,
  M_WinNT,
  M_WinDef,
  M_RTHeapDep,
  M_Word,
  M_RT0,
  M_ThreadWin32,

  I_ASCII,
  M_ASCII,

  I_AtomAtomTbl,
  I_Atom,
  M_AtomAtomTbl,
  M_Atom,

  I_Integer,
  M_Integer,

  I_AtomIntTbl,
  M_AtomIntTbl,

  I_AtomList,
  M_AtomList,

  I_AtomListSort,
  M_AtomListSort,

  I_AtomPQRep,
  I_AtomPQ,
  M_AtomPQ,

  I_Refany,
  M_Refany,

  I_AtomRefTbl,
  M_AtomRefTbl,

  I_AtomSeqRep,
  I_AtomSeq,
  M_AtomSeq,

  I_AtomTextTbl,
  M_AtomTextTbl,

  I_CConvert,
  M_CConvert,

  I_Convert,
  M_Convert,

  I_UnsafeWr,
  I_WrClass,
  I_Wr,
  M_WrMove,
  M_WrWin32,

  I_TimeWin32,
  I_Time,
  M_TimeWin32,

  I_Real,
  M_Real,

  I_LongReal,
  M_LongReal,

  I_Extended,
  M_Extended,

  I_FloatMode,
  M_FloatMode,

  I_RealRep,

  I_DragonInt,
  M_DragonInt,

  I_DragonT,
  M_DragonT,

  I_FPU,
  M_FPU,

  I_RealFloat,
  M_RealFloat,

  I_LongRealRep,

  I_LongFloat,
  M_LongFloat,

  I_ExtendedFloat,
  M_ExtendedFloat,

  I_TextSeqRep,
  I_TextSeq,
  M_TextSeq,

  I_Pathname,
  M_PathnameWin32,

  I_WinError,

  I_OSWin32,
  M_OSWin32,

  I_Terminal,
  I_RegularFile,
  I_Pipe,
  I_FileWin32,
  I_FmtBufTest,
  I_FmtBufF,
  I_FmtBuf,
  I_Fmt,
  I_OSErrorWin32,
  I_OSError,
  I_File,
  I_Process,
  M_Terminal,
  M_RegularFile,
  M_FileWin32,
  M_Pipe,
  M_PipeWin32,
  M_ProcessWin32,
  M_FmtBuf,
  M_Fmt,
  M_OSErrorWin32,

  I_AutoFlushWr,
  M_AutoFlushWr,

  I_Axis,
  M_Axis,

  I_Boolean,
  M_Boolean,

  I_BundleRep,
  I_Bundle,
  M_Bundle,

  I_Date,
  M_DateWin32,

  I_Swap,
  M_Swap,

  I_TimeStampRep,

  I_NB30,
  M_NB30,

  I_MachineID,
  M_MachineIDWin32,

  I_TimeStamp,
  M_TimeStamp,

  I_Tick,
  M_TickPortable,

  I_RandomReal,
  I_Random,
  M_RandomReal,
  M_Random,

  I_Capability,
  M_Capability,

  I_Char,
  M_Char,

  I_RTPacking,
  M_RTPacking,

  I_PklAction,

  I_UnsafeRd,
  I_RdClass,
  I_Rd,
  M_RdImpl,
  M_RdMove,

  I_RTTipe,
  M_RTTipe,

  I_PklActionSeqRep,
  I_PklActionSeq,
  M_PklActionSeq,

  I_FS,
  M_FS,
  M_FSWin32,

  I_FileRd,
  M_FileRd,

  I_FileWr,
  M_FileWr,

  I_Stdio,
  M_Stdio,

  I_IEEESpecial,
  M_IEEESpecial,

  I_Lex,
  M_Lex,

  I_IO,
  M_IO,

  I_PackingTypeCode,
  M_PackingTypeCode,

  I_PackingTbl,
  M_PackingTbl,

  I_ConvertPacking,
  M_ConvertPacking,

  I_ETimer,
  M_ETimer,

  I_Env,
  M_Env,

  I_Formatter,
  M_Formatter,

  I_Int32,
  M_Int32,

  I_IntArraySort,
  M_IntArraySort,

  I_IntAtomTbl,
  M_IntAtomTbl,

  I_IntIntTbl,
  M_IntIntTbl,

  I_IntList,
  M_IntList,

  I_IntListSort,
  M_IntListSort,

  I_IntPQRep,
  I_IntPQ,
  M_IntPQ,

  I_IntRefTbl,
  M_IntRefTbl,

  I_IntSeqRep,
  I_IntSeq,
  M_IntSeq,

  I_IntTextTbl,
  M_IntTextTbl,

  I_Interval,
  M_Interval,

  I_LongFloatExtras,
  M_LongFloatExtras,

  I_LongSqrt,
  M_LongSqrt,

  I_LongrealType,
  M_LongrealType,

  I_LowPerfTool,
  M_LowPerfTool,

  I_M3Config,

  I_PropertyF,
  I_Property,
  M_Property,

  I_MPropertyF,
  I_MProperty,
  M_MProperty,

  I_Math,

  I_MsgRd,

  I_MsgWr,

  I_NullRd,
  M_NullRd,

  I_NullWr,
  M_NullWr,

  I_Params,
  M_Params,

  I_Point,
  M_Point,

  I_Rect,
  M_Rect,

  I_PathPrivate,
  I_Path,
  M_Path,

  I_PerfTool,
  M_PerfTool,

  I_RTTypeFP,
  M_RTTypeFP,

  I_Pickle,
  M_Pickle,

  I_PklTipeMap,
  M_PklTipeMap,

  I_Pickle2,
  I_PickleRd,
  M_Pickle2,

  I_PickleStubs,
  M_PickleStubs,

  I_RegionRep,
  I_Region,
  M_Region,

  I_PolyRegion,
  M_PolyRegion,

  I_PropertyV,
  M_PropertyV,

  I_RandomPerm,
  M_RandomPerm,

  I_RdCopy,
  M_RdCopy,

  I_TextWr,
  M_TextWr,

  I_RdUtils,
  M_RdUtils,

  I_RealFloatExtras,
  M_RealFloatExtras,

  I_RealSqrt,
  M_RealSqrt,

  I_RealType,
  M_RealType,

  I_RefAtomTbl,
  M_RefAtomTbl,

  I_RefIntTbl,
  M_RefIntTbl,

  I_RefList,
  M_RefList,

  I_RefListSort,
  M_RefListSort,

  I_RefPQRep,
  I_RefPQ,
  M_RefPQ,

  I_RefRefTbl,
  M_RefRefTbl,

  I_RefSeqRep,
  I_RefSeq,
  M_RefSeq,

  I_RefTextTbl,
  M_RefTextTbl,

  I_TextRd,
  M_TextRd,

  I_Scan,
  M_Scan,

  I_SortedAtomAtomTbl,
  M_SortedAtomAtomTbl,

  I_SortedAtomIntTbl,
  M_SortedAtomIntTbl,

  I_SortedAtomRefTbl,
  M_SortedAtomRefTbl,

  I_SortedAtomTextTbl,
  M_SortedAtomTextTbl,

  I_SortedIntAtomTbl,
  M_SortedIntAtomTbl,

  I_SortedIntIntTbl,
  M_SortedIntIntTbl,

  I_SortedIntRefTbl,
  M_SortedIntRefTbl,

  I_SortedIntTextTbl,
  M_SortedIntTextTbl,

  I_SortedRefAtomTbl,
  M_SortedRefAtomTbl,

  I_SortedRefIntTbl,
  M_SortedRefIntTbl,

  I_SortedRefRefTbl,
  M_SortedRefRefTbl,

  I_SortedRefTextTbl,
  M_SortedRefTextTbl,

  I_TextAtomTbl,
  M_TextAtomTbl,

  I_SortedTextAtomTbl,
  M_SortedTextAtomTbl,

  I_TextIntTbl,
  M_TextIntTbl,

  I_SortedTextIntTbl,
  M_SortedTextIntTbl,

  I_TextRefTbl,
  M_TextRefTbl,

  I_SortedTextRefTbl,
  M_SortedTextRefTbl,

  I_TextTextTbl,
  M_TextTextTbl,

  I_SortedTextTextTbl,
  M_SortedTextTextTbl,

  I_Stat,
  M_Stat,

  I_Sx,
  M_Sx,

  I_TextArraySort,
  M_TextArraySort,

  I_TextList,
  M_TextList,

  I_TextListSort,
  M_TextListSort,

  I_TextPQRep,
  I_TextPQ,
  M_TextPQ,

  I_Transform,
  M_Transform,

  I_Trapezoid,
  M_Trapezoid,

  0
};

extern void RTMain__Init();

static _LINK_INFO _m3_link_info = {
  /* n_modules  */  sizeof(_modules) / sizeof(static _BIND_PROC) - 1,
  /* modules    */ (_ADDRESS)_modules,
  /* argc       */ 0,
  /* argv       */ 0,
  /* envp       */ 0,
  /* instance   */ 0,
  /* stack_bot  */ 0,
  /* stack_top  */ (_ADDRESS)0x400000
};

int m3main (argc, argv, envp)
int argc;
char **argv;
char **envp;
{
  { /* initialize the rest of the link info */
    _m3_link_info.argc = argc;
    _m3_link_info.argv = (_ADDRESS)(argv);
    _m3_link_info.envp = (_ADDRESS)(envp);
    _m3_link_info.instance = (_ADDRESS)(0);
    _m3_link_info.bottom_of_stack = (_ADDRESS)(&argc);
  };

  /* finally, start the Modula-3 program */
  RTMain__Init(&_m3_link_info);
  return 0;
}
