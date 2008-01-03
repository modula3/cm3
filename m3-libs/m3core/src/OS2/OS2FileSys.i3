INTERFACE OS2FileSys;

FROM Word IMPORT Shift;

FROM OS2Def IMPORT LONG, PLONG, ULONG, PULONG, BYTE, PBYTE, PVOID, CPVOID,
                   PCSZ, LHANDLE, BOOL32, PBOOL32, USHORT, PCHAR,
                   UCHAR, PUCHAR;

CONST
  CCHMAXPATH              = 260;
  CCHMAXPATHCOMP          = 256;

  DCPY_EXISTING           = 16_0001;
  DCPY_APPEND             = 16_0002;
  DCPY_FAILEAS            = 16_0004;

  DSPI_WRTTHRU            = 16_0010;

  EAT_BINARY              = 16_fffe;
  EAT_ASCII               = 16_fffd;
  EAT_BITMAP              = 16_fffb;
  EAT_METAFILE            = 16_fffa;
  EAT_ICON                = 16_fff9;
  EAT_EA                  = 16_ffee;
  EAT_MVMT                = 16_ffdf;
  EAT_MVST                = 16_ffde;
  EAT_ASN1                = 16_ffdd;

  ENUMEA_LEVEL_NO_VALUE   = 1;

  ENUMEA_REFTYPE_FHANDLE  = 0;
  ENUMEA_REFTYPE_PATH     = 1;
  ENUMEA_REFTYPE_MAX      = ENUMEA_REFTYPE_PATH;

  FEA_NEEDEA              = 16_80;

  FHB_DSKREMOTE           = 16_8000;
  FHB_CHRDEVREMOTE        = 16_8000;
  FHB_PIPEREMOTE          = 16_8000;

  FHT_DISKFILE            = 16_0000;
  FHT_CHRDEV              = 16_0001;
  FHT_PIPE                = 16_0002;

  FIL_STANDARD            = 1;
  FIL_QUERYEASIZE         = 2;
  FIL_QUERYEASFROMLIST    = 3;
  FIL_QUERYFULLNAME       = 5;    (* DosQueryPathInfo *)

  FILE_BEGIN              = 0;
  FILE_CURRENT            = 1;
  FILE_END                = 2;

  FILE_NORMAL             = 16_0000;
  FILE_READONLY           = 16_0001;
  FILE_HIDDEN             = 16_0002;
  FILE_SYSTEM             = 16_0004;
  FILE_DIRECTORY          = 16_0010;
  FILE_ARCHIVED           = 16_0020;

  FILE_IGNORE             = 16_10000;

  FILE_EXISTED            = 16_0001;
  FILE_CREATED            = 16_0002;
  FILE_TRUNCATED          = 16_0003;

  FILE_OPEN               = 16_0001;
  FILE_TRUNCATE           = 16_0002;
  FILE_CREATE             = 16_0010;

  FS_ATTACH               = 0;
  FS_DETACH               = 1;
  FS_SPOOLATTACH          = 2;
  FS_SPOOLDETACH          = 3;

  FSAIL_QUERYNAME         = 1;
  FSAIL_DEVNUMBER         = 2;
  FSAIL_DRVNUMBER         = 3;

  FSAT_CHARDEV            = 1;
  FSAT_PSEUDODEV          = 2;
  FSAT_LOCALDRV           = 3;
  FSAT_REMOTEDRV          = 4;

  FSCTL_HANDLE            = 1;
  FSCTL_PATHNAME          = 2;
  FSCTL_FSDNAME           = 3;

  FSCTL_ERROR_INFO        = 1;
  FSCTL_MAX_EASIZE        = 2;

  FSIL_ALLOC              = 1;
  FSIL_VOLSER             = 2;

  HANDTYPE_FILE           = 16_0000;
  HANDTYPE_DEVICE         = 16_0001;
  HANDTYPE_PIPE           = 16_0002;
  HANDTYPE_PROTECTED      = 16_4000;
  HANDTYPE_NETWORK        = 16_8000;

  HDIR_SYSTEM             = 1;
  HDIR_CREATE             = (-1);

  MUST_HAVE_READONLY      = (FILE_READONLY  + Shift (FILE_READONLY,  8));
  MUST_HAVE_HIDDEN        = (FILE_HIDDEN    + Shift (FILE_HIDDEN,    8));
  MUST_HAVE_SYSTEM        = (FILE_SYSTEM    + Shift (FILE_SYSTEM,    8));
  MUST_HAVE_DIRECTORY     = (FILE_DIRECTORY + Shift (FILE_DIRECTORY, 8));
  MUST_HAVE_ARCHIVED      = (FILE_ARCHIVED  + Shift (FILE_ARCHIVED,  8));

  OPEN_ACTION_FAIL_IF_EXISTS     = 16_0000;
  OPEN_ACTION_OPEN_IF_EXISTS     = 16_0001;
  OPEN_ACTION_REPLACE_IF_EXISTS  = 16_0002;
  OPEN_ACTION_FAIL_IF_NEW        = 16_0000;
  OPEN_ACTION_CREATE_IF_NEW      = 16_0010;

  OPEN_ACCESS_READONLY           = 16_0000;
  OPEN_ACCESS_WRITEONLY          = 16_0001;
  OPEN_ACCESS_READWRITE          = 16_0002;

  OPEN_SHARE_DENYREADWRITE       = 16_0010;
  OPEN_SHARE_DENYWRITE           = 16_0020;
  OPEN_SHARE_DENYREAD            = 16_0030;
  OPEN_SHARE_DENYNONE            = 16_0040;

  OPEN_FLAGS_NOINHERIT           = 16_0080;
  OPEN_FLAGS_NO_LOCALITY         = 16_0000;
  OPEN_FLAGS_SEQUENTIAL          = 16_0100;
  OPEN_FLAGS_RANDOM              = 16_0200;
  OPEN_FLAGS_RANDOMSEQUENTIAL    = 16_0300;
  OPEN_FLAGS_NO_CACHE            = 16_1000;
  OPEN_FLAGS_FAIL_ON_ERROR       = 16_2000;
  OPEN_FLAGS_WRITE_THROUGH       = 16_4000;
  OPEN_FLAGS_DASD                = 16_8000;
  OPEN_FLAGS_NONSPOOLED          = 16_40000;
  OPEN_FLAGS_PROTECTED_HANDLE    = 16_40000000;

  SEARCH_PATH             = 16_0000;
  SEARCH_CUR_DIRECTORY    = 16_0001;
  SEARCH_ENVIRONMENT      = 16_0002;
  SEARCH_IGNORENETERRS    = 16_0004;

TYPE
  HFILE   = LHANDLE;
  PHFILE  = UNTRACED REF HFILE;

  FHLOCK  = ULONG;
  PFHLOCK = UNTRACED REF FHLOCK;

  HDIR    = LHANDLE;
  PHDIR   = UNTRACED REF HDIR;

  FTIME = RECORD
    time: USHORT;   (* USHORT twosecs : 5, minutes : 6, hours : 5 bits *)
  END;
  PFTIME  = UNTRACED REF FTIME;

  FDATE = RECORD
    date: USHORT;  (* USHORT day : 5, month : 4, year  : 7 bits *)
  END;
  PFDATE = UNTRACED REF FDATE;

  FEA = RECORD
    fEA: BYTE;
    cbName: BYTE;
    cbValue: USHORT;
  END;
  PFEA = UNTRACED REF FEA;

  FEALIST = RECORD
    cbList: ULONG;
    list: PFEA;
  END;
  PFEALIST = UNTRACED REF FEALIST;

  GEA = RECORD
    cbName: BYTE;
    szName: PCHAR;
  END;
  PGEA = UNTRACED REF GEA;

  GEALIST = RECORD
    cbList: ULONG;
    list: PGEA;
  END;
  PGEALIST = UNTRACED REF GEALIST;

  EAOP = RECORD
    fpGEAList: PGEALIST;
    fpFEAList: PFEALIST;
    oError: ULONG;
  END;
  PEAOP = UNTRACED REF EAOP;

  FEA2 = RECORD
    oNextEntryOffset: ULONG;
    fEA: BYTE;
    cbName: BYTE;
    cbValue: USHORT;
    szName: PCHAR;
  END;
  PFEA2 = UNTRACED REF FEA2;

  FEA2LIST = RECORD
    cbList: ULONG;
    list: PFEA2;
  END;
  PFEA2LIST = UNTRACED REF FEA2LIST;

  GEA2 = RECORD
    oNextEntryOffset: ULONG;
    cbName: BYTE;
    szName: PCHAR;
  END;
  PGEA2 = UNTRACED REF GEA2;

  GEA2LIST = RECORD
    cbList: ULONG;
    list: PGEA2;
  END;
  PGEA2LIST = UNTRACED REF GEA2LIST;

  EAOP2 = RECORD
    fpGEA2List: PGEA2LIST;
    fpFEA2List: PFEA2LIST;
    oError: ULONG;
  END;
  PEAOP2 = UNTRACED REF EAOP2;

  DENA1 = RECORD
    reserved: UCHAR;
    cbName: UCHAR;
    cbValue: USHORT;
    szName: PUCHAR;
  END;
  PDENA1 = UNTRACED REF PDENA1;

  DENA2 = FEA2;
  PDENA2 = UNTRACED REF DENA2;

  EASIZEBUF = RECORD
    cbMaxEASize: USHORT;
    cbMaxEAListSize: ULONG;       (* Packed? *)
  END;
  PEASIZEBUF = UNTRACED REF EASIZEBUF;

  FILEFINDBUF = RECORD
    fdateCreation: FDATE;
    ftimeCreation: FTIME;
    fdateLastAccess: FDATE;
    ftimeLastAccess: FTIME;
    fdateLastWrite: FDATE;
    ftimeLastWrite: FTIME;
    cbFile: ULONG;
    cbFileAlloc: ULONG;
    attrFile: USHORT;
    cchName: UCHAR;
    achName: ARRAY [0..CCHMAXPATHCOMP - 1] OF CHAR;
  END;
  PFILEFINDBUF = UNTRACED REF FILEFINDBUF;

  FILEFINDBUF2 = RECORD
    fdateCreation:          FDATE;
    ftimeCreation:          FTIME;
    fdateLastAccess:        FDATE;
    ftimeLastAccess:        FTIME;
    fdateLastWrite:         FDATE;
    ftimeLastWrite:         FTIME;
    cbFile:                 ULONG;
    cbFileAlloc:            ULONG;
    attrFile:               USHORT;
    cbList:                 ULONG;
    cchName:                UCHAR;
    achName:                ARRAY [0..CCHMAXPATHCOMP - 1] OF CHAR;
  END;
  PFILEFINDBUF2 = UNTRACED REF FILEFINDBUF2;

  FILEFINDBUF3 = RECORD
    oNextEntryOffset:       ULONG;
    fdateCreation:          FDATE;
    ftimeCreation:          FTIME;
    fdateLastAccess:        FDATE;
    ftimeLastAccess:        FTIME;
    fdateLastWrite:         FDATE;
    ftimeLastWrite:         FTIME;
    cbFile:                 ULONG;
    cbFileAlloc:            ULONG;
    attrFile:               ULONG;
    cchName:                UCHAR;
    achName:                ARRAY [0..CCHMAXPATHCOMP - 1] OF CHAR;
  END;
  PFILEFINDBUF3 = UNTRACED REF FILEFINDBUF3;

  FILEFINDBUF4 = RECORD
    oNextEntryOffset:       ULONG;
    fdateCreation:          FDATE;
    ftimeCreation:          FTIME;
    fdateLastAccess:        FDATE;
    ftimeLastAccess:        FTIME;
    fdateLastWrite:         FDATE;
    ftimeLastWrite:         FTIME;
    cbFile:                 ULONG;
    cbFileAlloc:            ULONG;
    attrFile:               ULONG;
    cbList:                 ULONG;
    cchName:                UCHAR;
    achName:                ARRAY [0..CCHMAXPATHCOMP - 1] OF CHAR;
  END;
  PFILEFINDBUF4 = UNTRACED REF FILEFINDBUF4;

  FILELOCK = RECORD
    lOffset: LONG;
    lRange:  LONG;
  END;
  PFILELOCK = UNTRACED REF FILELOCK;

  FILESTATUS = RECORD
    fdateCreation:   FDATE;
    ftimeCreation:   FTIME;
    fdateLastAccess: FDATE;
    ftimeLastAccess: FTIME;
    fdateLastWrite:  FDATE;
    ftimeLastWrite:  FTIME;
    cbFile:          ULONG;
    cbFileAlloc:     ULONG;
    attrFile:        USHORT;
  END;
  PFILESTATUS = UNTRACED REF FILESTATUS;

  FILESTATUS2 = RECORD
    fdateCreation:   FDATE;
    ftimeCreation:   FTIME;
    fdateLastAccess: FDATE;
    ftimeLastAccess: FTIME;
    fdateLastWrite:  FDATE;
    ftimeLastWrite:  FTIME;
    cbFile:          ULONG;
    cbFileAlloc:     ULONG;
    attrFile:        USHORT;
    cbList:          ULONG;
  END;
  PFILESTATUS2 = UNTRACED REF FILESTATUS2;

  FILESTATUS3 = RECORD
    fdateCreation:   FDATE;
    ftimeCreation:   FTIME;
    fdateLastAccess: FDATE;
    ftimeLastAccess: FTIME;
    fdateLastWrite:  FDATE;
    ftimeLastWrite:  FTIME;
    cbFile:          ULONG;
    cbFileAlloc:     ULONG;
    attrFile:        ULONG;
  END;
  PFILESTATUS3 = UNTRACED REF FILESTATUS3;

  FILESTATUS4 = RECORD
    fdateCreation:   FDATE;
    ftimeCreation:   FTIME;
    fdateLastAccess: FDATE;
    ftimeLastAccess: FTIME;
    fdateLastWrite:  FDATE;
    ftimeLastWrite:  FTIME;
    cbFile:          ULONG;
    cbFileAlloc:     ULONG;
    attrFile:        ULONG;
    cbList:          ULONG;
  END;
  PFILESTATUS4 = UNTRACED REF FILESTATUS4;

  FSALLOCATE = RECORD
    idFileSystem: ULONG;
    cSectorUnit:  ULONG;
    cUnit:        ULONG;
    cUnitAvail:   ULONG;
    cbSector:     USHORT;
  END;
  PFSALLOCATE = UNTRACED REF FSALLOCATE;

  FSQBUFFER = RECORD
    iType:        USHORT;
    cbName:       USHORT;
    szName:       PUCHAR;
    cbFSDName:    USHORT;
    szFSDName:    PUCHAR;
    cbFSAData:    USHORT;
    rgFSAData:    PUCHAR;
  END;
  PFSQBUFFER = UNTRACED REF FSQBUFFER;

  FSQBUFFER2 = RECORD
    iType:        USHORT;
    cbName:       USHORT;
    cbFSDName:    USHORT;
    cbFSAData:    USHORT;
    szName:       PUCHAR;
    szFSDName:    PUCHAR;
    rgFSAData:    PUCHAR;
  END;
  PFSQBUFFER2 = UNTRACED REF FSQBUFFER2;

  SPOOLATTACH = RECORD
    hNmPipe: USHORT;
    ulKey:   ULONG;
  END;
  PSPOOLATTACH = UNTRACED REF SPOOLATTACH;

  VOLUMELABEL = RECORD
    cch:            BYTE;
    szVolLabel:     ARRAY [0..12 - 1] OF CHAR;
  END;
  PVOLUMELABEL = UNTRACED REF VOLUMELABEL;

  FSINFO = RECORD
    fdateCreation: FDATE;
    ftimeCreation: FTIME;
    vol: VOLUMELABEL;
  END;
  PFSINFO = UNTRACED REF FSINFO;

<* EXTERNAL *>
PROCEDURE DosCancelLockRequest (hFile: HFILE; (* const *) pfl: PFILELOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosClose (hFile: HFILE): ULONG;

<* EXTERNAL *>
PROCEDURE DosCopy (pszSource: PCSZ; pszTarget: PCSZ; ulOption: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosCreateDir (pszDirName: PCSZ; pEABuf: PEAOP2): ULONG;

<* EXTERNAL *>
PROCEDURE DosDelete (pszFileName: PCSZ): ULONG;

<* EXTERNAL *>
PROCEDURE DosDeleteDir (pszDirName: PCSZ): ULONG;

<* EXTERNAL *>
PROCEDURE DosDupHandle (hFile: HFILE; phFile: PHFILE): ULONG;

<* EXTERNAL *>
PROCEDURE DosEditName (ulLevel: ULONG; pszSource: PCSZ; pszEdit: PCSZ;
    pszTargetBuf: PBYTE; ulTargetBufLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosEnumAttribute (ulRefType: ULONG; pvFile: CPVOID; ulEntry: ULONG;
    pvBuf: PVOID; ulBufLength: ULONG; pulCount: PULONG; ulInfoLevel: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosFindClose (hDir: HDIR): ULONG;

<* EXTERNAL *>
PROCEDURE DosFindFirst (pszFileSpec: PCSZ; phDir: PHDIR; flAttribute: ULONG;
    pFindBuf: PVOID; ulFindBufLength: ULONG; pulFileNames: PULONG;
    ulInfoLevel: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosFindNext (hDir: HDIR; pFindBuf: PVOID; ulFindBufLength: ULONG;
    pulFileNames: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosForceDelete (pszFileName: PCSZ): ULONG;

<* EXTERNAL *>
PROCEDURE DosFSAttach (pszDevice: PCSZ; pszFilesystem: PCSZ;
    (* const *) pData: PVOID; ulDataLength: ULONG; ulFlag: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosFSCtl (pData: PVOID; ulDataLengthMax: ULONG; pulDataLength: PULONG;
    pParmList: UNTRACED REF PVOID; ulParmLengthMax: ULONG; pulParmLength: PULONG;
    ulFunction: ULONG; pszRouteName: PCSZ; hFile: HFILE; ulMethod: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosMove (pszOldName: PCSZ; pszNewName: PCSZ): ULONG;

<* EXTERNAL *>
PROCEDURE DosOpen (pszFileName: PCSZ; phFile: PHFILE; pulAction: PULONG;
    ulFileSize: ULONG; ulAttribute: ULONG; ulOpenFlags: ULONG; ulOpenMode: ULONG;
    pEABuf: PEAOP2): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectClose (hFile: HFILE; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectEnumAttribute (ulRefType: ULONG; pvFile: CPVOID;
    ulEntry: ULONG; pvBuf: PVOID; ulBufLength: ULONG; pulCount: PULONG;
    ulInfoLevel: ULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectOpen (pszFileName: PCSZ; phFile: PHFILE; pulAction: PULONG;
    ulFileSize: ULONG; ulAttribute: ULONG; ulOpenFlags: ULONG; ulOpenMode: ULONG;
    pEABuf: PEAOP2; pfhFileHandleLockID: PFHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectQueryFHState (hFile: HFILE; pulMode: PULONG;
    fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectQueryFileInfo (hFile: HFILE; ulInfoLevel: ULONG;
    pInfoBuffer: PVOID; ulInfoLength: ULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectRead (hFile: HFILE; pBuffer: PVOID; ulLength: ULONG;
    pulBytesRead: PULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectSetFHState (hFile: HFILE; ulMode: ULONG;
    fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectSetFileInfo (hFile: HFILE; ulInfoLevel: ULONG; pInfoBuffer: PVOID;
    ulInfoLength: ULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectSetFileLocks (hFile: HFILE; (* const *) pflUnlock: PFILELOCK;
    (* const *) pflLock: PFILELOCK; ulTimeout: ULONG; ulFlags: ULONG;
    fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectSetFilePtr (hFile: HFILE; lOffset: LONG; ulOrigin: ULONG;
    pulPos: PULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectSetFileSize (hFile: HFILE; ulSize: ULONG;
    fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosProtectWrite (hFile: HFILE; pBuffer: CPVOID; ulLength: ULONG;
    pulBytesWritten: PULONG; fhFileHandleLockID: FHLOCK): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryCurrentDir (ulDrive: ULONG; pPath: PBYTE; pulPathLength: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryCurrentDisk (pulDrive: PULONG; pulLogical: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryFHState (hFile: HFILE; pulMode: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryFileInfo (hFile: HFILE; ulInfoLevel: ULONG; pInfoBuffer: PVOID;
    ulInfoLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryFSAttach (pszDeviceName: PCSZ; ulOrdinal: ULONG;
    ulFSAInfoLevel: ULONG; pfsqb: PFSQBUFFER2; pulBufLength: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryFSInfo (ulDrive: ULONG; ulInfoLevel: ULONG; pBuf: PVOID;
    ulBufLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryHType (hFile: HFILE; pulType: PULONG; pulAttr: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryPathInfo (pszPathName: PCSZ; ulInfoLevel: ULONG;
    pInfoBuffer: PVOID; ulInfoLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosQueryVerify (pVerify: PBOOL32): ULONG;

<* EXTERNAL *>
PROCEDURE DosRead (hFile: HFILE; pBuffer: PVOID; ulLength: ULONG;
    pulBytesRead: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosResetBuffer (hf: HFILE): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetCurrentDir (pszDir: PCSZ): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetDefaultDisk (ulDrive: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFHState (hFile: HFILE; ulMode: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFileInfo (hFile: HFILE; ulInfoLevel: ULONG; pInfoBuffer: PVOID;
    ulInfoLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFileLocks (hFile: HFILE; (* const *) pflUnlock: PFILELOCK;
    (* const *) pflLock: PFILELOCK; ulTimeout: ULONG; ulFlags: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFilePtr (hFile: HFILE; lOffset: LONG; ulOrigin: ULONG; pulPos: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFileSize (hFile: HFILE; ulSize: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetFSInfo (ulDrive: ULONG; ulInfoLevel: ULONG; pBuf: PVOID;
    ulBufLength: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetMaxFH (ulCount: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetPathInfo (pszPathName: PCSZ; ulInfoLevel: ULONG; pInfoBuffer: PVOID;
    ulInfoLength: ULONG; ulOptions: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetRelMaxFH (pulReqCount: PLONG; pulCurMaxFH: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSetVerify (f32Verify: BOOL32): ULONG;

<* EXTERNAL *>
PROCEDURE DosShutdown (ulReserved: ULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosWrite (hFile: HFILE; pBuffer: CPVOID; ulLength: ULONG;
    pulBytesWritten: PULONG): ULONG;

<* EXTERNAL *>
PROCEDURE DosSearchPath (ulControl: ULONG; szPath: PCSZ; pszFilename: PCSZ;
    pBuf: PBYTE; ulBufLength: ULONG): ULONG;

END OS2FileSys.
