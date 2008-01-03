(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 13:49:20 PST 1994 by kalsow   *)
(*      modified on Mon Feb  8 19:07:44 PST 1993 by harrison *)

INTERFACE WinError;

(* Corresponds to build version 0001 of "winerror.h".
 * See that file for details.
 *
 *   This file contains the error code definitions for the Win32 API
 *   functions.
 *)

FROM WinDef IMPORT LONG;

(*  Values are 32 bit values layed out as follows:*)

(*   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1                      *)
(*   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0  *)
(*  +---+-+-+-----------------------+-------------------------------+ *)
(*  |Sev|C|R|     Facility          |               Code            | *)
(*  +---+-+-+-----------------------+-------------------------------+ *)
(*                                                                    *)
(*  where                                                             *)
(*                                                                    *)
(*      Sev - is the severity code                                    *)
(*                                                                    *)
(*          00 - Success                                              *)
(*          01 - Informational                                        *)
(*          10 - Warning                                              *)
(*          11 - Error                                                *)
(*                                                                    *)
(*      C - is the Customer code flag                                 *)
(*                                                                    *)
(*      R - is a reserved bit                                         *)
(*                                                                    *)
(*      Facility - is the facility code                               *)
(*                                                                    *)
(*      Code - is the facility's status code                          *)


(* Define the facility codes*)




(* Define the severity codes*)


CONST NO_ERROR: LONG = 0;    (* dderror*)
(* No error*)

CONST ERROR_SUCCESS: LONG = 0;
(* The configuration registry database operation completed successfully.*)

CONST ERROR_INVALID_FUNCTION: LONG = 1;    (* dderror*)
(* Incorrect function*)

CONST ERROR_FILE_NOT_FOUND: LONG = 2;
(* The system cannot find the file specified.*)

CONST ERROR_PATH_NOT_FOUND: LONG = 3;
(* The system cannot find the path specified.*)

CONST ERROR_TOO_MANY_OPEN_FILES: LONG = 4;
(* The system cannot open the file.*)

CONST ERROR_ACCESS_DENIED: LONG = 5;
(* Access Denied.*)

CONST ERROR_INVALID_HANDLE: LONG = 6;
(* The handle is invalid.*)

CONST ERROR_ARENA_TRASHED: LONG = 7;
(* The storage control blocks were destroyed.*)

CONST ERROR_NOT_ENOUGH_MEMORY: LONG = 8;    (* dderror*)
(* Not enough storage is available to process this command.*)

CONST ERROR_INVALID_BLOCK: LONG = 9;
(* The storage control block address is invalid.*)

CONST ERROR_BAD_ENVIRONMENT = 10;
(* The environment is incorrect.*)

CONST ERROR_BAD_FORMAT = 11;
(* An attempt was made to load a program with an incorrect format.*)

CONST ERROR_INVALID_ACCESS = 12;
(* The access code is invalid.*)

CONST ERROR_INVALID_DATA = 13;
(* The data is invalid.*)

CONST ERROR_OUTOFMEMORY = 14;
(* Not enough storage is available to complete this operation.*)

CONST ERROR_INVALID_DRIVE = 15;
(* The system cannot find the drive specified.*)

CONST ERROR_CURRENT_DIRECTORY = 16;
(* The directory cannot be removed.*)

CONST ERROR_NOT_SAME_DEVICE = 17;
(* The system cannot move the file to a different disk drive.*)

CONST ERROR_NO_MORE_FILES = 18;
(* There are no more files.*)

CONST ERROR_WRITE_PROTECT = 19;
(* The diskette is write protected.*)

CONST ERROR_BAD_UNIT = 20;
(* The system cannot find the device specified.*)

CONST ERROR_NOT_READY = 21;
(* The drive is not ready.*)

CONST ERROR_BAD_COMMAND = 22;
(* The device does not recognize the command.*)

CONST ERROR_CRC = 23;
(* Data error (cyclic redundancy check)*)

CONST ERROR_BAD_LENGTH = 24;
(* The program issued a command but the command length is incorrect.*)

CONST ERROR_SEEK = 25;
(* The drive cannot locate a specific area or track on the disk.*)

CONST ERROR_NOT_DOS_DISK = 26;
(* The specified disk or diskette cannot be accessed.*)

CONST ERROR_SECTOR_NOT_FOUND = 27;
(* The drive cannot find the sector requested.*)

CONST ERROR_OUT_OF_PAPER = 28;
(* The printer is out of paper.*)

CONST ERROR_WRITE_FAULT = 29;
(* The system cannot write to the specified device.*)

CONST ERROR_READ_FAULT = 30;
(* The system cannot read from the specified device.*)

CONST ERROR_GEN_FAILURE = 31;
(* A device attached to the system is not functioning.*)

CONST ERROR_SHARING_VIOLATION = 32;
(* The process cannot access the file because
    it is being used by another process.*)

CONST ERROR_LOCK_VIOLATION = 33;
(* The process cannot access the file because
    another process has locked a portion of the file.*)

CONST ERROR_WRONG_DISK = 34;
(* The wrong diskette is in the drive.*)
(* Insert %2 (Volume Serial Number: %3)*)
(* into drive %1.*)

CONST ERROR_SHARING_BUFFER_EXCEEDED = 36;
(* Too many files opened for sharing.*)

CONST ERROR_HANDLE_EOF = 38;
(* Reached End of File.*)

CONST ERROR_HANDLE_DISK_FULL = 39;
(* The disk is full.*)

CONST ERROR_NOT_SUPPORTED = 50;
(* The network request is not supported.*)

CONST ERROR_REM_NOT_LIST = 51;
(* The remote computer is not available.*)

CONST ERROR_DUP_NAME = 52;
(* A duplicate name exists on the network.*)

CONST ERROR_BAD_NETPATH = 53;
(* The network path was not found.*)

CONST ERROR_NETWORK_BUSY = 54;
(* The network is busy.*)

CONST ERROR_DEV_NOT_EXIST = 55;
(* The specified network resource is no longer*)
(* available.*)

CONST ERROR_TOO_MANY_CMDS = 56;
(* The network BIOS command limit has been reached.*)

CONST ERROR_ADAP_HDW_ERR = 57;
(* A network adapter hardware error occurred.*)

CONST ERROR_BAD_NET_RESP = 58;
(* The specified server cannot perform the requested*)
(* operation.*)

CONST ERROR_UNEXP_NET_ERR = 59;
(* An unexpected network error occurred.*)

CONST ERROR_BAD_REM_ADAP = 60;
(* The remote adapter is not compatible.*)

CONST ERROR_PRINTQ_FULL = 61;
(* The printer queue is full.*)

CONST ERROR_NO_SPOOL_SPACE = 62;
(* Space to store the file waiting to be printed is*)
(* not available on the server.*)

CONST ERROR_PRINT_CANCELLED = 63;
(* Your file waiting to be printed was deleted.*)

CONST ERROR_NETNAME_DELETED = 64;
(* The specified network name is no longer available.*)

CONST ERROR_NETWORK_ACCESS_DENIED = 65;
(* Network access is denied.*)

CONST ERROR_BAD_DEV_TYPE = 66;
(* The network resource type is not correct.*)

CONST ERROR_BAD_NET_NAME = 67;
(* The network name cannot be found.*)

CONST ERROR_TOO_MANY_NAMES = 68;
(* The name limit for the local computer network*)
(* adapter card was exceeded.*)

CONST ERROR_TOO_MANY_SESS = 69;
(* The network BIOS session limit was exceeded.*)

CONST ERROR_SHARING_PAUSED = 70;
(* The remote server has been paused or is in the*)
(* process of being started.*)

CONST ERROR_REQ_NOT_ACCEP = 71;
(* The network request was not accepted.*)

CONST ERROR_REDIR_PAUSED = 72;
(* The specified printer or disk device has been paused.*)

CONST ERROR_FILE_EXISTS = 80;
(* The file exists.*)

CONST ERROR_CANNOT_MAKE = 82;
(* The directory or file cannot be created.*)

CONST ERROR_FAIL_I24 = 83;
(* Fail on INT 24*)

CONST ERROR_OUT_OF_STRUCTURES = 84;
(* Storage to process this request is not available.*)

CONST ERROR_ALREADY_ASSIGNED = 85;
(* The local device name is already in use.*)

CONST ERROR_INVALID_PASSWORD = 86;
(* The specified network password is not correct.*)

CONST ERROR_INVALID_PARAMETER = 87;
(* The parameter is incorrect.*)    (* dderror*)

CONST ERROR_NET_WRITE_FAULT = 88;
(* A write fault occurred on the network.*)

CONST ERROR_NO_PROC_SLOTS = 89;
(* The system cannot start another process at*)
(* this time.*)

CONST ERROR_TOO_MANY_SEMAPHORES = 100;
(* Cannot create another system semaphore.*)

CONST ERROR_EXCL_SEM_ALREADY_OWNED = 101;
(* The exclusive semaphore is owned by another process.*)

CONST ERROR_SEM_IS_SET = 102;
(* The semaphore is set and cannot be closed.*)

CONST ERROR_TOO_MANY_SEM_REQUESTS = 103;
(* The semaphore cannot be set again.*)

CONST ERROR_INVALID_AT_INTERRUPT_TIME = 104;
(* Cannot request exclusive semaphores at interrupt time.*)

CONST ERROR_SEM_OWNER_DIED = 105;
(* The previous ownership of this semaphore has ended.*)

CONST ERROR_SEM_USER_LIMIT = 106;
(* Insert the diskette for drive %1.*)

CONST ERROR_DISK_CHANGE = 107;
(* Program stopped because alternate diskette was not inserted.*)

CONST ERROR_DRIVE_LOCKED = 108;
(* The disk is in use or locked by*)
(* another process.*)

CONST ERROR_BROKEN_PIPE = 109;
(* The pipe has been ended.*)

CONST ERROR_OPEN_FAILED = 110;
(* The system cannot open the*)
(* device or file specified.*)

CONST ERROR_BUFFER_OVERFLOW = 111;
(* The file name is too long.*)

CONST ERROR_DISK_FULL = 112;
(* There is not enough space on the disk.*)

CONST ERROR_NO_MORE_SEARCH_HANDLES = 113;
(* No more internal file identifiers available.*)

CONST ERROR_INVALID_TARGET_HANDLE = 114;
(* The target internal file identifier is incorrect.*)

CONST ERROR_INVALID_CATEGORY = 117;
(* The IOCTL call made by the application program is*)
(* not correct.*)

CONST ERROR_INVALID_VERIFY_SWITCH = 118;
(* The verify-on-write switch parameter value is not*)
(* correct.*)

CONST ERROR_BAD_DRIVER_LEVEL = 119;
(* The system does not support the command requested.*)

CONST ERROR_CALL_NOT_IMPLEMENTED = 120;
(* The Application Program Interface (API) entered*)
(* will only work in Windows/NT mode.*)

CONST ERROR_SEM_TIMEOUT = 121;
(* The semaphore timeout period has expired.*)

CONST ERROR_INSUFFICIENT_BUFFER = 122;
(* The data area passed to a system call is too*)
(* small.*)    (* dderror*)

CONST ERROR_INVALID_NAME = 123;
(* The file name, directory name, or volume label is not syntactically correct.*)

CONST ERROR_INVALID_LEVEL = 124;
(* The system call level is not correct.*)

CONST ERROR_NO_VOLUME_LABEL = 125;
(* The disk has no volume label.*)

CONST ERROR_MOD_NOT_FOUND = 126;
(* The specified module could not be found.*)

CONST ERROR_PROC_NOT_FOUND = 127;
(* The specified procedure could not be found.*)

CONST ERROR_WAIT_NO_CHILDREN = 128;
(* There are no child processes to wait for.*)

CONST ERROR_CHILD_NOT_COMPLETE = 129;
(* The %1 application cannot be run in Windows mode.*)

CONST ERROR_DIRECT_ACCESS_HANDLE = 130;
(* BUGBUG - message text missing.*)

CONST ERROR_NEGATIVE_SEEK = 131;
(* BUGBUG - message text missing.*)

CONST ERROR_SEEK_ON_DEVICE = 132;
(* BUGBUG - message text missing.*)

CONST ERROR_IS_JOIN_TARGET = 133;
(* A JOIN or SUBST command*)
(* cannot be used for a drive that*)
(* contains previously joined drives.*)

CONST ERROR_IS_JOINED = 134;
(* An attempt was made to use a*)
(* JOIN or SUBST command on a drive that has*)
(* already been joined.*)

CONST ERROR_IS_SUBSTED = 135;
(* An attempt was made to use a*)
(* JOIN or SUBST command on a drive that has*)
(* already been substituted.*)

CONST ERROR_NOT_JOINED = 136;
(* The system tried to delete*)
(* the JOIN of a drive that is not joined.*)

CONST ERROR_NOT_SUBSTED = 137;
(* The system tried to delete the*)
(* substitution of a drive that is not substituted.*)

CONST ERROR_JOIN_TO_JOIN = 138;
(* The system tried to join a drive*)
(* to a directory on a joined drive.*)

CONST ERROR_SUBST_TO_SUBST = 139;
(* The system tried to substitute a*)
(* drive to a directory on a substituted drive.*)

CONST ERROR_JOIN_TO_SUBST = 140;
(* The system tried to join a drive to*)
(* a directory on a substituted drive.*)

CONST ERROR_SUBST_TO_JOIN = 141;
(* The system tried to SUBST a drive*)
(* to a directory on a joined drive.*)

CONST ERROR_BUSY_DRIVE = 142;
(* The system cannot perform a JOIN or SUBST at this time.*)

CONST ERROR_SAME_DRIVE = 143;
(* The system cannot join or substitute a*)
(* drive to or for a directory on the same drive.*)

CONST ERROR_DIR_NOT_ROOT = 144;
(* The directory is not a subdirectory of the root directory.*)

CONST ERROR_DIR_NOT_EMPTY = 145;
(* The directory is not empty.*)

CONST ERROR_IS_SUBST_PATH = 146;
(* The path specified is being used in*)
(* a substitute.*)

CONST ERROR_IS_JOIN_PATH = 147;
(* Not enough resources are available to*)
(* process this command.*)

CONST ERROR_PATH_BUSY = 148;
(* The path specified cannot be used at this time.*)

CONST ERROR_IS_SUBST_TARGET = 149;
(* An attempt was made to join*)
(* or substitute a drive for which a directory*)
(* on the drive is the target of a previous*)
(* substitute.*)

CONST ERROR_SYSTEM_TRACE = 150;
(* System trace information was not specified in your*)
(* CONFIG.SYS file, or tracing is disallowed.*)

CONST ERROR_INVALID_EVENT_COUNT = 151;
(* The number of specified semaphore events for*)
(* DosMuxSemWait is not correct.*)

CONST ERROR_TOO_MANY_MUXWAITERS = 152;
(* DosMuxSemWait did not execute; too many semaphores*)
(* are already set.*)

CONST ERROR_INVALID_LIST_FORMAT = 153;
(* The DosMuxSemWait list is not correct.*)

CONST ERROR_LABEL_TOO_LONG = 154;
(* The volume label you entered exceeds the*)
(* 11 character limit.  The first 11 characters were written*)
(* to disk.  Any characters that exceeded the 11 character limit*)
(* were automatically deleted.*)

CONST ERROR_TOO_MANY_TCBS = 155;
(* Cannot create another thread.*)

CONST ERROR_SIGNAL_REFUSED = 156;
(* The recipient process has refused the signal.*)

CONST ERROR_DISCARDED = 157;
(* The segment is already discarded and cannot be locked.*)

CONST ERROR_NOT_LOCKED = 158;
(* The segment is already unlocked.*)

CONST ERROR_BAD_THREADID_ADDR = 159;
(* The address for the thread ID is not correct.*)

CONST ERROR_BAD_ARGUMENTS = 160;
(* The argument string passed to DosExecPgm is not correct.*)

CONST ERROR_BAD_PATHNAME = 161;
(* Invalid path name specified.*)

CONST ERROR_SIGNAL_PENDING = 162;
(* A signal is already pending.*)

CONST ERROR_MAX_THRDS_REACHED = 164;
(* No more threads can be created in the system.*)

CONST ERROR_LOCK_FAILED = 167;
(* Attempt to lock a region of a file failed.*)

CONST ERROR_BUSY = 170;
(* The requested resource is in use.*)

CONST ERROR_CANCEL_VIOLATION = 173;
(* A lock request was not outstanding for the supplied cancel region.*)

CONST ERROR_ATOMIC_LOCKS_NOT_SUPPORTED = 174;
(* The file system does not supporting atomic changing of the lock type.*)

CONST ERROR_INVALID_SEGMENT_NUMBER = 180;
(* The system detected a segment number that was not correct.*)

CONST ERROR_INVALID_ORDINAL = 182;
(* The operating system cannot run %1.*)

CONST ERROR_ALREADY_EXISTS = 183;
(* Attempt to create file that already exists.*)

CONST ERROR_INVALID_FLAG_NUMBER = 186;
(* The flag passed is not correct.*)

CONST ERROR_SEM_NOT_FOUND = 187;
(* The specified system semaphore name was not found.*)

CONST ERROR_INVALID_STARTING_CODESEG = 188;
(* The operating system cannot run %1.*)

CONST ERROR_INVALID_STACKSEG = 189;
(* The operating system cannot run %1.*)

CONST ERROR_INVALID_MODULETYPE = 190;
(* The operating system cannot run %1.*)

CONST ERROR_INVALID_EXE_SIGNATURE = 191;
(* %1 cannot be run in Windows/NT mode.*)

CONST ERROR_EXE_MARKED_INVALID = 192;
(* The operating system cannot run %1.*)

CONST ERROR_BAD_EXE_FORMAT = 193;
(* %1 is not a valid Windows application.*)

CONST ERROR_ITERATED_DATA_EXCEEDS_64k = 194;
(* The operating system cannot run %1.*)

CONST ERROR_INVALID_MINALLOCSIZE = 195;
(* The operating system cannot run %1.*)

CONST ERROR_DYNLINK_FROM_INVALID_RING = 196;
(* The operating system cannot run this*)
(* application program.*)

CONST ERROR_IOPL_NOT_ENABLED = 197;
(* The operating system is not presently*)
(* configured to run this application.*)

CONST ERROR_INVALID_SEGDPL = 198;
(* The operating system cannot run %1.*)

CONST ERROR_AUTODATASEG_EXCEEDS_64k = 199;
(* The operating system cannot run this*)
(* application program.*)

CONST ERROR_RING2SEG_MUST_BE_MOVABLE = 200;
(* The code segment cannot be greater than or equal to 64KB.*)

CONST ERROR_RELOC_CHAIN_XEEDS_SEGLIM = 201;
(* The operating system cannot run %1.*)

CONST ERROR_INFLOOP_IN_RELOC_CHAIN = 202;
(* The operating system cannot run %1.*)

CONST ERROR_ENVVAR_NOT_FOUND = 203;
(* The system could not find the environment*)
(* option that was entered.*)

CONST ERROR_NO_SIGNAL_SENT = 205;
(* No process in the command subtree has a*)
(* signal handler.*)

CONST ERROR_FILENAME_EXCED_RANGE = 206;
(* The file name or extension is too long.*)

CONST ERROR_RING2_STACK_IN_USE = 207;
(* The ring 2 stack is in use.*)

CONST ERROR_META_EXPANSION_TOO_LONG = 208;
(* The global file name characters, * or ? are entered*)
(* incorrectly or too many global file name characters are specified.*)

CONST ERROR_INVALID_SIGNAL_NUMBER = 209;
(* The signal being posted is not correct.*)

CONST ERROR_THREAD_1_INACTIVE = 210;
(* The signal handler cannot be set.*)

CONST ERROR_LOCKED = 212;
(* The segment is locked and cannot be reallocated.*)

CONST ERROR_TOO_MANY_MODULES = 214;
(* Too many dynamic link modules are attached to this*)
(* program or dynamic link module.*)

CONST ERROR_NESTING_NOT_ALLOWED = 215;
(* Can't nest calls to LoadModule.*)

CONST ERROR_BAD_PIPE = 230;
(* Invalid pipe state.*)

CONST ERROR_PIPE_BUSY = 231;
(* All pipe instances busy.*)

CONST ERROR_NO_DATA = 232;
(* Pipe close in progress.*)

CONST ERROR_PIPE_NOT_CONNECTED = 233;
(* No process on other end of pipe.*)

CONST ERROR_MORE_DATA = 234;
(* More data is available.*)    (* dderror*)

CONST ERROR_VC_DISCONNECTED = 240;
(* The session was cancelled.*)

CONST ERROR_INVALID_EA_NAME = 254;
(* The specified EA name was invalid.*)

CONST ERROR_EA_LIST_INCONSISTENT = 255;
(* The EAs are inconsistent.*)

CONST ERROR_NO_MORE_ITEMS = 259;
(* No more data is available.*)

CONST ERROR_CANNOT_COPY = 266;
(* The Copy API cannot be used.*)

CONST ERROR_DIRECTORY = 267;
(* Invalid directory name.*)

CONST ERROR_EAS_DIDNT_FIT = 275;
(* The EAs did not fit in the buffer.*)

CONST ERROR_EA_FILE_CORRUPT = 276;
(* The EA file on the mounted file system is corrupt.*)

CONST ERROR_EA_TABLE_FULL = 277;
(* The EA table in the EA file on the mounted file system is full.*)

CONST ERROR_INVALID_EA_HANDLE = 278;
(* The EA handle specified is invalid.*)

CONST ERROR_EAS_NOT_SUPPORTED = 282;
(* The mounted file system does not support extended attributes.*)

CONST ERROR_NOT_OWNER = 288;
(* Attempt to release mutex not owned by caller.*)

CONST ERROR_TOO_MANY_POSTS = 298;
(* Too many posts were made to a semaphore.*)

CONST ERROR_MR_MID_NOT_FOUND = 317;
(* The system cannot find message for message number 0x%1*)
(* in message file for %2.*)

CONST ERROR_INVALID_ADDRESS = 487;
(* Attempt to access invalid address.*)

CONST ERROR_ARITHMETIC_OVERFLOW = 534;
(* Arithmatic result exceeded 32-bits.*)

CONST ERROR_PIPE_CONNECTED = 535;
(* There is a process on other end of the pipe.*)

CONST ERROR_PIPE_LISTENING = 536;
(* Waiting for a process to open the other end of the pipe.*)

CONST ERROR_EA_ACCESS_DENIED = 994;
(* Access to the EA was denied.*)

CONST ERROR_OPERATION_ABORTED = 995;
(* The IO operation has been aborted due to either thread exit*)
(* or application request.*)

CONST ERROR_IO_INCOMPLETE = 996;
(* Overlapped IO event not in signalled state.*)

CONST ERROR_IO_PENDING = 997;
(* Overlapped IO operation in progress.*)    (* dderror*)

CONST ERROR_NOACCESS = 998;
(* Invalid access to memory location.*)

CONST ERROR_SWAPERROR = 999;
(* Error accessing paging file.*)

CONST ERROR_STACK_OVERFLOW = 1001;
(* Recursion too deep, stack overflowed.*)

CONST ERROR_INVALID_MESSAGE = 1002;
(* Window can't handle sent message.*)

CONST ERROR_CAN_NOT_COMPLETE = 1003;
(* Cannot complete function for some reason.  Like calling GetSelection()*)
(* when there is no selection.*)

CONST ERROR_INVALID_FLAGS = 1004;
(* Invalid flags.*)

CONST ERROR_UNRECOGNIZED_VOLUME = 1005;
(* The volume does not contain a recognized file system.*)
(* Please make sure that all required file system drivers are loaded and that the*)
(* volume is not corrupt.*)

CONST ERROR_FILE_INVALID = 1006;
(* The volume for a file has been externally altered such that the*)
(* opened file is no longer valid.*)

CONST ERROR_FULLSCREEN_MODE = 1007;
(* The requested operation cannot be performed in fullscreen mode.*)

CONST ERROR_NO_TOKEN = 1008;
(* An attempt was made to reference a token that does not exist.*)

CONST ERROR_BADDB = 1009;
(* The configuration registry database is corrupt.*)

CONST ERROR_BADKEY = 1010;
(* The configuration registry key is invalid.*)

CONST ERROR_CANTOPEN = 1011;
(* The configuration registry key could not be opened.*)

CONST ERROR_CANTREAD = 1012;
(* The configuration registry key could not be read.*)

CONST ERROR_CANTWRITE = 1013;
(* The configuration registry key could not be written.*)

CONST ERROR_REGISTRY_RECOVERED = 1014;
(* One of the files containing the system's Registry data had to be recovered*)
(* by use of a log or alternate copy.  The recovery was successful.*)

CONST ERROR_REGISTRY_CORRUPT = 1015;
(* The Registry is corrupt.*)
(* The structure of one of the files that contains Registry data is corrupt, or*)
(* the system's in memory image of the file is corrupt, or the file could not*)
(* be recovered because its alternate copy or log was absent or corrupt.*)

CONST ERROR_REGISTRY_IO_FAILED = 1016;
(* An I/O operation initiated by the Registry failed unrecoverably.*)
(* The Registry could not read in, or write out, or flush, one of the files*)
(* that contain the system's image of the Registry.*)

CONST ERROR_NOT_REGISTRY_FILE = 1017;
(* The system has attempted to load or restore a file into the registry, and the*)
(* specified file is not in the format of a registry file.*)

CONST ERROR_KEY_DELETED = 1018;
(* Illegal operation attempted on a registry key which has been marked for deletion.*)

CONST ERROR_NO_LOG_SPACE = 1019;
(* System could not allocate required space in a registry log.*)

CONST ERROR_KEY_HAS_CHILDREN = 1020;
(* An attempt was made to create a symbolic link in a registry key that already*)
(* has subkeys or values.*)

CONST ERROR_CHILD_MUST_BE_VOLATILE = 1021;
(* An attempt was made to create a Stable subkey under a Volatile parent key.*)

CONST ERROR_NOTIFY_ENUM_DIR = 1022;
(* This indicates that a notify change request is being completed and that the*)
(* information is not being returned in the caller's buffer.  The caller now*)
(* needs to enumerate the files to find the changes.*)

CONST ERROR_DEPENDENT_SERVICES_RUNNING = 1051;
(* A stop control has been sent to a service which other running services*)
(* are dependent on.*)

CONST ERROR_INVALID_SERVICE_CONTROL = 1052;
(* The requested control is not valid for this service*)

CONST ERROR_SERVICE_REQUEST_TIMEOUT = 1053;
(* The service did not respond to the start or control request in a timely*)
(* fashion.*)

CONST ERROR_SERVICE_NO_THREAD = 1054;
(* A thread could not be created for the Win32 service.*)

CONST ERROR_SERVICE_DATABASE_LOCKED = 1055;
(* The service database is locked.*)

CONST ERROR_SERVICE_ALREADY_RUNNING = 1056;
(* An instance of the service is already running.*)

CONST ERROR_INVALID_SERVICE_ACCOUNT = 1057;
(* The account name is invalid or does not exist.*)

CONST ERROR_SERVICE_DISABLED = 1058;
(* The specified service is disabled and cannot be started.*)

CONST ERROR_CIRCULAR_DEPENDENCY = 1059;
(* Circular service dependency was specified.*)

CONST ERROR_SERVICE_DOES_NOT_EXIST = 1060;
(* The specified service does not exist as an installed service.*)

CONST ERROR_SERVICE_CANNOT_ACCEPT_CTRL = 1061;
(* The service cannot accept control messages at this time.*)

CONST ERROR_SERVICE_NOT_ACTIVE = 1062;
(* The service has not been started.*)

CONST ERROR_FAILED_SERVICE_CONTROLLER_CONNECT = 1063;
(* The service process could not connect to the service controller.*)

CONST ERROR_EXCEPTION_IN_SERVICE = 1064;
(* An exception occurred in the service when handling the control request.*)

CONST ERROR_DATABASE_DOES_NOT_EXIST = 1065;
(* The database specified does not exist.*)

CONST ERROR_SERVICE_SPECIFIC_ERROR = 1066;
(* The service has returned a service specific error code.*)

CONST ERROR_PROCESS_ABORTED = 1067;
(* The process terminated unexpectedly.*)

CONST ERROR_SERVICE_DEPENDENCY_FAIL = 1068;
(* The dependency service or group failed to start.*)

CONST ERROR_SERVICE_LOGON_FAILED = 1069;
(* The service failed to be logged on.*)

CONST ERROR_SERVICE_START_HANG = 1070;
(* The service hung on starting.*)

CONST ERROR_INVALID_SERVICE_LOCK = 1071;
(* The specified service database lock is invalid.*)

CONST ERROR_SERVICE_MARKED_FOR_DELETE = 1072;
(* The specified service has been marked for deletion.*)

CONST ERROR_SERVICE_EXISTS = 1073;
(* The specified service already exists.*)

CONST ERROR_ALREADY_RUNNING_LKG = 1074;
(* The system is currently booted using the last-known-good configuration.*)

CONST ERROR_SERVICE_DEPENDENCY_DELETED = 1075;
(* The dependency service does not exist or has been marked for*)
(* deletion.*)

CONST ERROR_BOOT_ALREADY_ACCEPTED = 1076;
(* The current boot has already been accepted for use as the*)
(* last-known-good control set.*)

CONST ERROR_END_OF_MEDIA = 1100;
(* End of tape mark has been reached during an operation.*)

CONST ERROR_SHORT_FILEMARK_DETECTED = 1101;
(* A tape access reached a short filemark.*)

CONST ERROR_LONG_FILEMARK_DETECTED = 1102;
(* A tape access reached a long filemark.*)

CONST ERROR_SETMARK_DETECTED = 1103;
(* A tape access reached a setmark.*)

CONST ERROR_NO_DATA_DETECTED = 1104;
(* During a tape access, the end of the data written is reached.*)

CONST ERROR_PARTITION_FAILURE = 1105;
(* Tape could not be partitioned.*)

CONST ERROR_INVALID_BLOCK_LENGTH = 1106;
(* When accessing a new tape of a multivolume partition, the current*)
(* blocksize is incorrect.*)

CONST ERROR_DEVICE_NOT_PARTITIONED = 1107;
(* Tape partition information could not be found when loading a tape.*)

CONST ERROR_UNABLE_TO_LOCK_MEDIA = 1108;
(* Attempt to lock the eject media mechansism fails.*)

CONST ERROR_UNABLE_TO_UNLOAD_MEDIA = 1109;
(* Unload media fails.*)

CONST ERROR_TAPE_WRITE_PROTECTED = 1110;
(* Attempt to write to or erase a write protected tape.*)

CONST ERROR_TAPE_OPERATION_NOT_SUPPORTED = 1111;
(* Attempted operation not supported on drive.*)

CONST ERROR_NO_TAPE_IN_DRIVE = 1112;
(* Tape query failed because of no tape in drive.*)

CONST ERROR_NO_UNICODE_TRANSLATION = 1113;
(* No mapping for the Unicode character exists in the target multi-byte code page.*)

CONST ERROR_DLL_INIT_FAILED = 1114;
(* A DLL initialization routine failed.*)

CONST ERROR_SHUTDOWN_IN_PROGRESS = 1115;
(* A system shutdown is in progress.*)

CONST ERROR_NO_SHUTDOWN_IN_PROGRESS = 1116;
(* An attempt to abort the shutdown of the system failed because no shutdown*)
(* was in progress.*)

CONST ERROR_IO_DEVICE = 1117;
(* The request could not be performed because of an I/O device error.*)

CONST ERROR_SERIAL_NO_DEVICE = 1118;
(* No serial device was successfully initialized.  The serial driver will unload.*)

CONST ERROR_IRQ_BUSY = 1119;
(* An attempt was made to open a device that was sharing an IRQ with other devices.*)
(* At least one other device that uses that IRQ was already opened.  Two concurrent*)
(* opens of devices that share an IRQ and only work via interrupts is not supported*)
(* for the particular bus type that the devices use.*)

CONST ERROR_MORE_WRITES = 1120;
(* This informational status message indicates that the serial io control*)
(* IOCTL_SERIAL_XOFF_COUNTER completed due to another write to the serial*)
(* port.*)

CONST ERROR_COUNTER_TIMEOUT = 1121;
(* This informational status message indicates that the serial io control*)
(* IOCTL_SERIAL_XOFF_COUNTER completed due to timer expiring before the*)
(* counter reached 0.*)

CONST ERROR_FLOPPY_ID_MARK_NOT_FOUND = 1122;
(* While accessing the floppy an id address mark was not found.*)

CONST ERROR_FLOPPY_WRONG_CYLINDER = 1123;
(* While accessing the floppy the track address from the sector id field was found*)
(* to be different than the track address maintained by the controller.*)

CONST ERROR_FLOPPY_UNKNOWN_ERROR = 1124;
(* The floppy controller reported an error that is not recognized by the floppy driver.*)

CONST ERROR_FLOPPY_BAD_REGISTERS = 1125;
(* While accessing the floppy, the controller returned inconsistant results via its registers.*)

CONST ERROR_DISK_RECALIBRATE_FAILED = 1126;
(* While accessing the hard disk, a recalibrate operation failed, even after retries.*)

CONST ERROR_DISK_OPERATION_FAILED = 1127;
(* While accessing the hard disk, a disk operation failed even after retries.*)

CONST ERROR_DISK_RESET_FAILED = 1128;
(* While accessing the hard disk, a disk controller reset was needed, but even that failed.*)
(* It would seem that some diagnostics should be run.*)

(*///////////////////////*)
(*                       *)
(* Winnet32 Status Codes *)
(*                       *)
(*///////////////////////*)

CONST ERROR_BAD_USERNAME = 2202;
(* The specified user name is invalid.*)

CONST ERROR_NO_NETWORK = 2138;
(* The network is not present or not started.*)

CONST ERROR_NOT_CONNECTED = 2250;
(* This network connection does not exist.*)

CONST ERROR_OPEN_FILES = 2401;
(* There are open files or requests pending on this connection.*)

CONST ERROR_DEVICE_IN_USE = 2404;
(* The device is in use by an active process and cannot be disconnected.*)

CONST ERROR_BAD_DEVICE = 1200;
(* The specified device name is invalid.*)

CONST ERROR_CONNECTION_UNAVAIL = 1201;
(* The device is not currently connected but it is a remembered connection.*)

CONST ERROR_DEVICE_ALREADY_REMEMBERED = 1202;
(* An attempt was made to remember a device that had previously been remembered.*)

CONST ERROR_NO_NET_OR_BAD_PATH = 1203;
(* No network provider accepted the given network path.*)

CONST ERROR_BAD_PROVIDER = 1204;
(* The specified network provider name is invalid.*)

CONST ERROR_CANNOT_OPEN_PROFILE = 1205;
(* Unable to open the network connection profile.*)

CONST ERROR_BAD_PROFILE = 1206;
(* The network connection profile is corrupt.*)

CONST ERROR_NOT_CONTAINER = 1207;
(* Cannot enumerate a non-container.*)

CONST ERROR_EXTENDED_ERROR = 1208;
(* An extended error has occurred.*)

CONST ERROR_INVALID_GROUPNAME = 1209;
(* The format of the specified group name is invalid.*)

CONST ERROR_INVALID_COMPUTERNAME = 1210;
(* The format of the specified computer name is invalid.*)

CONST ERROR_INVALID_EVENTNAME = 1211;
(* The format of the specified event name is invalid.*)

CONST ERROR_INVALID_DOMAINNAME = 1212;
(* The format of the specified domain name is invalid.*)

CONST ERROR_INVALID_SERVICENAME = 1213;
(* The format of the specified service name is invalid.*)

CONST ERROR_INVALID_NETNAME = 1214;
(* The format of the specified network name is invalid.*)

CONST ERROR_INVALID_SHARENAME = 1215;
(* The format of the specified share name is invalid.*)

CONST ERROR_INVALID_PASSWORDNAME = 1216;
(* The format of the specified password is invalid.*)

CONST ERROR_INVALID_MESSAGENAME = 1217;
(* The format of the specified message name is invalid.*)

CONST ERROR_INVALID_MESSAGEDEST = 1218;
(* The format of the specified message destination is invalid.*)

(*///////////////////////*)
(*                       *)
(* Security Status Codes *)
(*                       *)
(*///////////////////////*)

CONST ERROR_NOT_ALL_ASSIGNED = 1300;
(* Indicates not all privileges referenced are assigned to the caller.*)
(* This allows, for example, all privileges to be disabled without having*)
(* to know exactly which privileges are assigned.*)

CONST ERROR_SOME_NOT_MAPPED = 1301;
(* Some of the information to be translated has not been translated.*)

CONST ERROR_NO_QUOTAS_FOR_ACCOUNT = 1302;
(* No system quota limits are specifically set for this Account.*)

CONST ERROR_LOCAL_USER_SESSION_KEY = 1303;
(* A user session key was requested for a local rpc connection. The session key*)
(* returned is a constant value and not unique to this connection. There is*)
(* little to be gained by encrypting data on such a connection.*)

CONST ERROR_NULL_LM_PASSWORD = 1304;
(* The NT password is too complex to be converted to a Lan-Manager password.*)
(* The Lan-Manager password returned is a NULL string.*)

CONST ERROR_UNKNOWN_REVISION = 1305;
(* Indicates a revision number encountered or specified is not one*)
(* known by the service.  It may be a more recent revision than the*)
(* service is aware of.*)

CONST ERROR_REVISION_MISMATCH = 1306;
(* Indicates two revision levels are incompatible.*)

CONST ERROR_INVALID_OWNER = 1307;
(* Indicates a particular Security ID may not be assigned as the*)
(* owner of an object.*)

CONST ERROR_INVALID_PRIMARY_GROUP = 1308;
(* Indicates a particular Security ID may not be assigned as the*)
(* primary group of an object.*)

CONST ERROR_NO_IMPERSONATION_TOKEN = 1309;
(* An attempt has been made to operate on an impersonation token*)
(* by a thread that is not currently impersonating a client.*)

CONST ERROR_CANT_DISABLE_MANDATORY = 1310;
(* A mandatory group may not be disabled.*)

CONST ERROR_NO_LOGON_SERVERS = 1311;
(* There are currently no logon servers available to service the logon*)
(* request.*)

CONST ERROR_NO_SUCH_LOGON_SESSION = 1312;
(* A specified logon session does not exist.  It may already have*)
(* been terminated.*)

CONST ERROR_NO_SUCH_PRIVILEGE = 1313;
(* A specified privilege does not exist.*)

CONST ERROR_PRIVILEGE_NOT_HELD = 1314;
(* A required privilege is not held by the client.*)

CONST ERROR_INVALID_ACCOUNT_NAME = 1315;
(* The name provided is not a properly formed account name.*)

CONST ERROR_USER_EXISTS = 1316;
(* The specified user already exists.*)

CONST ERROR_NO_SUCH_USER = 1317;
(* The specified user does not exist.*)

CONST ERROR_GROUP_EXISTS = 1318;
(* The specified group already exists.*)

CONST ERROR_NO_SUCH_GROUP = 1319;
(* The specified group does not exist.*)

CONST ERROR_MEMBER_IN_GROUP = 1320;
(* The specified user account is already in the specified group account.*)
(* Also used to indicate a group can not be deleted because it contains*)
(* a member.*)

CONST ERROR_MEMBER_NOT_IN_GROUP = 1321;
(* The specified user account is not a member of the specified group account.*)

CONST ERROR_LAST_ADMIN = 1322;
(* Indicates the requested operation would disable or delete the last*)
(* remaining administration account.  This is not allowed to prevent*)
(* creating a situation in which the system will not be administratable.*)

CONST ERROR_WRONG_PASSWORD = 1323;
(* When trying to update a password, this return status indicates that*)
(* the value provided as the current password is not correct.*)

CONST ERROR_ILL_FORMED_PASSWORD = 1324;
(* When trying to update a password, this return status indicates that*)
(* the value provided for the new password contains values that are*)
(* not allowed in passwords.*)

CONST ERROR_PASSWORD_RESTRICTION = 1325;
(* When trying to update a password, this status indicates that some*)
(* password update rule has been violated.  For example, the password*)
(* may not meet length criteria.*)

CONST ERROR_LOGON_FAILURE = 1326;
(* The attempted logon is invalid.  This is either due to a bad username*)
(* or authentication information.*)

CONST ERROR_ACCOUNT_RESTRICTION = 1327;
(* Indicates a referenced user name and authentication information are*)
(* valid, but some user account restriction has prevented successful*)
(* authentication (such as time-of-day restrictions).*)

CONST ERROR_INVALID_LOGON_HOURS = 1328;
(* The user account has time restrictions and may not be logged onto*)
(* at this time.*)

CONST ERROR_INVALID_WORKSTATION = 1329;
(* The user account is restricted such that it may not be used to*)
(* log on from the source workstation.*)

CONST ERROR_PASSWORD_EXPIRED = 1330;
(* The user account's password has expired.*)

CONST ERROR_ACCOUNT_DISABLED = 1331;
(* The referenced account is currently disabled and may not be logged on to.*)

CONST ERROR_NONE_MAPPED = 1332;
(* None of the information to be translated has been translated.*)

CONST ERROR_TOO_MANY_LUIDS_REQUESTED = 1333;
(* The number of LUIDs requested may not be allocated with a single*)
(* allocation.*)

CONST ERROR_LUIDS_EXHAUSTED = 1334;
(* Indicates there are no more LUIDs to allocate.*)

CONST ERROR_INVALID_SUB_AUTHORITY = 1335;
(* Indicates the sub-authority value is invalid for the particular use.*)

CONST ERROR_INVALID_ACL = 1336;
(* Indicates the ACL structure is not valid.*)

CONST ERROR_INVALID_SID = 1337;
(* Indicates the SID structure is not valid.*)

CONST ERROR_INVALID_SECURITY_DESCR = 1338;
(* Indicates the SECURITY_DESCRIPTOR structure is not valid.*)

CONST ERROR_BAD_INHERITANCE_ACL = 1340;
(* Indicates that an attempt to build either an inherited ACL or ACE*)
(* was not successful.*)
(* This can be caused by a number of things.  One of the more probable*)
(* causes is the replacement of a CreatorId with an SID that didn't fit*)
(* into the ACE or ACL.*)

CONST ERROR_SERVER_DISABLED = 1341;
(* The GUID allocation server is [already] disabled at the moment.*)

CONST ERROR_SERVER_NOT_DISABLED = 1342;
(* The GUID allocation server is [already] enabled at the moment.*)

CONST ERROR_INVALID_ID_AUTHORITY = 1343;
(* The value provided was an invalid value for an identifier authority.*)

CONST ERROR_ALLOTTED_SPACE_EXCEEDED = 1344;
(* When a block of memory is allotted for future updates, such as the memory*)
(* allocated to hold discretionary access control and primary group information,*)
(* successive updates may exceed the amount of memory originally allotted.*)
(* Since quota may already have been charged to several processes which have*)
(* handles to the object, it is not reasonable to alter the size of the*)
(* allocated memory.  Instead, a request that requires more memory than has*)
(* been allotted must fail and the ERROR_ALLOTTED_SPACE_EXCEEDED error returned.*)

CONST ERROR_INVALID_GROUP_ATTRIBUTES = 1345;
(* The specified attributes are invalid, or incompatible with the*)
(* attributes for the group as a whole.*)

CONST ERROR_BAD_IMPERSONATION_LEVEL = 1346;
(* A specified impersonation level is invalid.*)
(* Also used to indicate a required impersonation level was not provided.*)

CONST ERROR_CANT_OPEN_ANONYMOUS = 1347;
(* An attempt was made to open an Anonymous level token.*)
(* Anonymous tokens may not be opened.*)

CONST ERROR_BAD_VALIDATION_CLASS = 1348;
(* The validation information class requested was invalid.*)

CONST ERROR_BAD_TOKEN_TYPE = 1349;
(* The type of a token object is inapropriate for its attempted use.*)

CONST ERROR_NO_SECURITY_ON_OBJECT = 1350;
(* Indicates an attempt was made to operate on the security of*)
(* an object that does not have security associated with it.*)

CONST ERROR_CANT_ACCESS_DOMAIN_INFO = 1351;
(* Indicates a domain controller could not be contacted or that*)
(* objects within the domain are protected such that necessary*)
(* information could not be retrieved.*)

CONST ERROR_INVALID_SERVER_STATE = 1352;
(* Indicates the Sam Server was in the wrong state to*)
(* perform the desired operation.*)

CONST ERROR_INVALID_DOMAIN_STATE = 1353;
(* Indicates the Domain was in the wrong state to*)
(* perform the desired operation.*)

CONST ERROR_INVALID_DOMAIN_ROLE = 1354;
(* Indicates the desired operation cannot be completed*)
(* with the domain in its present role.*)

CONST ERROR_NO_SUCH_DOMAIN = 1355;
(* The specified Domain did not exist.*)

CONST ERROR_DOMAIN_EXISTS = 1356;
(* The specified Domain already exists.*)

CONST ERROR_DOMAIN_LIMIT_EXCEEDED = 1357;
(* An attempt to exceed the limit on the number of domains per server*)
(* for this release.*)

CONST ERROR_INTERNAL_DB_CORRUPTION = 1358;
(* This error indicates that the requested operation cannot be*)
(* completed due to a catastrophic media failure or on-disk data*)
(* structure corruption*)

CONST ERROR_INTERNAL_ERROR = 1359;
(* This error indicates that the SAM server has encounterred an*)
(* internal consistency error in its database.  This catastrophic*)
(* failure will prevent further operation of SAM*)

CONST ERROR_GENERIC_NOT_MAPPED = 1360;
(* Indicates generic access types were contained in an access mask*)
(* which should already be mapped to non-generic access types.*)

CONST ERROR_BAD_DESCRIPTOR_FORMAT = 1361;
(* Indicates a security descriptor is not in the necessary format (absolute*)
(* or self-relative).*)

CONST ERROR_NOT_LOGON_PROCESS = 1362;
(* The requested action is restricted for use by logon processes*)
(* only.  The calling process has not registered as a logon process.*)

CONST ERROR_LOGON_SESSION_EXISTS = 1363;
(* A attempt has been made to start a new session manager or*)
(* LSA logon session with an ID that is alread in use.*)

CONST ERROR_NO_SUCH_PACKAGE = 1364;
(* A specified authentication package is unknown*)

CONST ERROR_BAD_LOGON_SESSION_STATE = 1365;
(* The logon session is not in a state the is consistent with the*)
(* requested operation.*)

CONST ERROR_LOGON_SESSION_COLLISION = 1366;
(* The logon session is not in a state the is consistent with the*)
(* requested operation.  BUG, BUG - fix this message*)

CONST ERROR_INVALID_LOGON_TYPE = 1367;
(* Indicates an invalid value has been provided for LogonType has been*)
(* requested.*)

CONST ERROR_CANNOT_IMPERSONATE = 1368;
(* Indicates that an attempt has been made to impersonate via a named*)
(* pipe that has not yet been read from.*)

CONST ERROR_RXACT_INVALID_STATE = 1369;
(* Indicates that the transaction state of a registry sub-tree is*)
(* incompatible with the requested operation.*)
(* For example, a request has been made to start a new transaction with*)
(* one already in progress, or a request to apply a transaction when one*)
(* is not currently in progress.*)
(* This status value is returned by the runtime library (RTL) registry*)
(* transaction package (RXact).*)

CONST ERROR_RXACT_COMMIT_FAILURE = 1370;
(* Indicates an error has occured during a registry transaction commit.*)
(* The database has been left in an unknown, but probably inconsistent,*)
(* state.  The state of the registry transaction is left as COMMITTING.*)
(* This status value is returned by the runtime library (RTL) registry*)
(* transaction package (RXact).*)

CONST ERROR_SPECIAL_ACCOUNT = 1371;
(* Indicates an operation has been attempted on a built-in (special)*)
(* SAM account which is incompatible with built-in accounts.  For*)
(* example, built-in accounts can not be re-named or deleted.*)

CONST ERROR_SPECIAL_GROUP = 1372;
(* The operation requested may not be performed on the specified*)
(* group because it is a built-in special group.*)

CONST ERROR_SPECIAL_USER = 1373;
(* The operation requested may not be performed on the specified*)
(* user  because it is a built-in special user.*)

CONST ERROR_MEMBERS_PRIMARY_GROUP = 1374;
(* Indicates a member can not be removed from a group because the*)
(* group is currently the member's primary group.*)

CONST ERROR_TOKEN_ALREADY_IN_USE = 1375;
(* An attempt was made to establish a token for use as a primary token*)
(* but the token is already in use.  A token can only be the primary token*)
(* of one process at a time.*)

CONST ERROR_NO_SUCH_ALIAS = 1376;
(* The specified alias does not exist.*)

CONST ERROR_MEMBER_NOT_IN_ALIAS = 1377;
(* The specified account name is not a member of the alias.*)

CONST ERROR_MEMBER_IN_ALIAS = 1378;
(* The specified account name is already a member of the alias.*)

CONST ERROR_ALIAS_EXISTS = 1379;
(* The specified alias already exists.*)

CONST ERROR_LOGON_NOT_GRANTED = 1380;
(* A requested type of logon (e.g., Interactive, Network, Service) is not*)
(* granted by the target system's local security policy.  Please ask the*)
(* system administrator to grant the necessary form of logon.*)

CONST ERROR_TOO_MANY_SECRETS = 1381;
(* The maximum number of secrets that may be stored in a single system has been*)
(* exceeded.  The length and number of secrets is limited to satisfy United*)
(* States State Department export restrictions.*)

CONST ERROR_SECRET_TOO_LONG = 1382;
(* The length of an secret exceeds the maximum length allowed.  The length and*)
(* number of secrets is limited to satisfy United States State Department*)
(* export restrictions.*)

CONST ERROR_INTERNAL_DB_ERROR = 1383;
(* The Local Security Authority (LSA) database contains in internal inconsistency.*)

CONST ERROR_TOO_MANY_CONTEXT_IDS = 1384;
(* During a logon attempt, the user's security context accumulated too many*)
(* security IDs.  This is a very unusual situation.  Remove the user from*)
(* some groups or aliases to reduce the number of security ids to incorporate*)
(* into the security context.*)

CONST ERROR_LOGON_TYPE_NOT_GRANTED = 1385;
(* A user has requested a type of logon (e.g., interactive or network) that*)
(* has not be granted.  An administrator has control over who may logon*)
(* interactively and through the network.*)

CONST ERROR_NT_CROSS_ENCRYPTION_REQUIRED = 1386;
(* An attempt was made to change a user password in the security account manager*)
(* without providing the necessary NT cross-encrypted password.*)

CONST ERROR_NO_SUCH_MEMBER = 1387;
(* A new member could not be added to an alias because the member does not exist.*)

CONST ERROR_INVALID_MEMBER = 1388;
(* A new member could not be added to an alias because the member has the*)
(* wrong account type.*)

CONST ERROR_TOO_MANY_SIDS = 1389;
(* Too many Sids have been specified.*)

CONST ERROR_LM_CROSS_ENCRYPTION_REQUIRED = 1390;
(* An attempt was made to change a user password in the security account manager*)
(* without providing the necessary LM cross-encrypted password.*)

CONST ERROR_NO_INHERITANCE = 1391;
(* Indicates an ACL contains no inheritable components*)

CONST ERROR_FILE_CORRUPT = 1392;
(* The file or directory is corrupt and non-readable.*)

CONST ERROR_DISK_CORRUPT = 1393;
(* The disk structure is corrupt and non-readable.*)

(* End of security error codes*)

(*/////////////////////*)
(*                     *)
(* WinUser Error Codes *)
(*                     *)
(*/////////////////////*)

CONST ERROR_INVALID_WINDOW_HANDLE = 1400;
(* Invalid window handle.*)

CONST ERROR_INVALID_MENU_HANDLE = 1401;
(* Invalid menu handle.*)

CONST ERROR_INVALID_CURSOR_HANDLE = 1402;
(* Invalid cursor handle.*)

CONST ERROR_INVALID_ACCEL_HANDLE = 1403;
(* Invalid accelerator-table handle.*)

CONST ERROR_INVALID_HOOK_HANDLE = 1404;
(* Invalid hook handle.*)

CONST ERROR_INVALID_DWP_HANDLE = 1405;
(* Invalid DeferWindowPos handle.*)

CONST ERROR_TLW_WITH_WSCHILD = 1406;
(* CreateWindow() failed, creating top-level window with WS_CHILD style.*)

CONST ERROR_CANNOT_FIND_WND_CLASS = 1407;
(* Cannot find window class.*)

CONST ERROR_WINDOW_OF_OTHER_THREAD = 1408;
(* Invalid window, belongs to other thread.*)

CONST ERROR_HOTKEY_ALREADY_REGISTERED = 1409;
(* Hotkey already registered.*)

CONST ERROR_CLASS_ALREADY_EXISTS = 1410;
(* Class already exists.*)

CONST ERROR_CLASS_DOES_NOT_EXIST = 1411;
(* Class does not exist.*)

CONST ERROR_CLASS_HAS_WINDOWS = 1412;
(* Class still has open windows.*)

CONST ERROR_INVALID_INDEX = 1413;
(* Invalid index.*)

CONST ERROR_INVALID_ICON_HANDLE = 1414;
(* Invalid icon handle.*)

CONST ERROR_PRIVATE_DIALOG_INDEX = 1415;
(* Using private DIALOG window words.*)

CONST ERROR_LISTBOX_ID_NOT_FOUND = 1416;
(* Listbox ID not found.*)

CONST ERROR_NO_WILDCARD_CHARACTERS = 1417;
(* No wildcard characters found.*)

CONST ERROR_CLIPBOARD_NOT_OPEN = 1418;
(* Thread doesn't have clipboard open.*)

CONST ERROR_HOTKEY_NOT_REGISTERED = 1419;
(* Hotkey not registered.*)

CONST ERROR_WINDOW_NOT_DIALOG = 1420;
(* The window is not a valid dialog window.*)

CONST ERROR_CONTROL_ID_NOT_FOUND = 1421;
(* Control ID not found.*)

CONST ERROR_INVALID_COMBOBOX_MESSAGE = 1422;
(* Invalid Message, combobox doesn't have an edit control.*)

CONST ERROR_WINDOW_NOT_COMBOBOX = 1423;
(* The window is not a combobox.*)

CONST ERROR_INVALID_EDIT_HEIGHT = 1424;
(* Height must be less than 256.*)

CONST ERROR_DC_NOT_FOUND = 1425;
(* Invalid HDC passed to ReleaseDC.*)

CONST ERROR_INVALID_HOOK_FILTER = 1426;
(* Invalid hook filter type.*)

CONST ERROR_INVALID_FILTER_PROC = 1427;
(* Invalid filter proc.*)

CONST ERROR_HOOK_NEEDS_HMOD = 1428;
(* Cannot set non-local hook without an module handle.*)

CONST ERROR_GLOBAL_ONLY_HOOK = 1429;
(* This hook can only be set globally.*)

CONST ERROR_JOURNAL_HOOK_SET = 1430;
(* The journal hook is already installed.*)

CONST ERROR_HOOK_NOT_INSTALLED = 1431;
(* Hook not installed.*)

CONST ERROR_INVALID_LB_MESSAGE = 1432;
(* Invalid message for single-selection listbox.*)

CONST ERROR_SETCOUNT_ON_BAD_LB = 1433;
(* LB_SETCOUNT sent to non-lazy listbox.*)

CONST ERROR_LB_WITHOUT_TABSTOPS = 1434;
(* This listbox doesn't support tab stops.*)

CONST ERROR_DESTROY_OBJECT_OF_OTHER_THREAD = 1435;
(* Cannot destroy object created by another thread.*)

CONST ERROR_CHILD_WINDOW_MENU = 1436;
(* Child windows can't have menus.*)

CONST ERROR_NO_SYSTEM_MENU = 1437;
(* Window doesn't have system menu.*)

CONST ERROR_INVALID_MSGBOX_STYLE = 1438;
(* Invalid message box style.*)

CONST ERROR_INVALID_SPI_VALUE = 1439;
(* Invalid SPI_* parameter.*)

CONST ERROR_SCREEN_ALREADY_LOCKED = 1440;
(* Screen already locked.*)

CONST ERROR_HWNDS_HAVE_DIFF_PARENT = 1441;
(* All DeferWindowPos() HWNDs must have same parent.*)

CONST ERROR_NOT_CHILD_WINDOW = 1442;
(* Window is not a child window.*)

CONST ERROR_INVALID_GW_COMMAND = 1443;
(* Invalid GW_* command.*)

CONST ERROR_INVALID_THREAD_ID = 1444;
(* Invalid thread ID.*)

CONST ERROR_NON_MDICHILD_WINDOW = 1445;
(* DefMDIChildProc() called with a non-MDIChild window.*)

CONST ERROR_POPUP_ALREADY_ACTIVE = 1446;
(* Popup menu already active.*)

CONST ERROR_NO_SCROLLBARS = 1447;
(* Window does not have scrollbars.*)

CONST ERROR_INVALID_SCROLLBAR_RANGE = 1448;
(* Scrollbar range greater than 0x7FFF.*)

CONST ERROR_INVALID_SHOWWIN_COMMAND = 1449;
(* Invalid ShowWindow() command.*)

(* End of WinUser error codes*)

(*///////////////////////*)
(*                       *)
(* Eventlog Status Codes *)
(*                       *)
(*///////////////////////*)

CONST ERROR_EVENTLOG_FILE_CORRUPT = 1500;
(* The Eventlog logfile %1 is corrupt.*)

CONST ERROR_EVENTLOG_CANT_START = 1501;
(* No Eventlog log file could be opened.  The Eventlog service did not start.*)

(* End of eventlog error codes*)

(*//////////////////*)
(*                  *)
(* RPC Status Codes *)
(*                  *)
(*//////////////////*)

CONST RPC_S_INVALID_STRING_BINDING = 1700;
(* The string binding is invalid.*)

CONST RPC_S_WRONG_KIND_OF_BINDING = 1701;
(* The binding handle is not the correct type.*)

CONST RPC_S_INVALID_BINDING = 1702;
(* The binding handle is invalid.*)

CONST RPC_S_PROTSEQ_NOT_SUPPORTED = 1703;
(* The Rpc protocol sequence is not supported.*)

CONST RPC_S_INVALID_RPC_PROTSEQ = 1704;
(* The rpc protocol sequence is invalid.*)

CONST RPC_S_INVALID_STRING_UUID = 1705;
(* The string uuid is invalid.*)

CONST RPC_S_INVALID_ENDPOINT_FORMAT = 1706;
(* The endpoint format is invalid.*)

CONST RPC_S_INVALID_NET_ADDR = 1707;
(* The network address is invalid.*)

CONST RPC_S_NO_ENDPOINT_FOUND = 1708;
(* No endpoint was found.*)

CONST RPC_S_INVALID_TIMEOUT = 1709;
(* The timeout value is invalid.*)

CONST RPC_S_OBJECT_NOT_FOUND = 1710;
(* The object uuid was not found.*)

CONST RPC_S_ALREADY_REGISTERED = 1711;
(* The object uuid has already been registered.*)

CONST RPC_S_TYPE_ALREADY_REGISTERED = 1712;
(* The type uuid has already been registered.*)

CONST RPC_S_ALREADY_LISTENING = 1713;
(* The server is already listening.*)

CONST RPC_S_NO_PROTSEQS_REGISTERED = 1714;
(* No protocol sequences have been registered.*)

CONST RPC_S_NOT_LISTENING = 1715;
(* The server is not listening.*)

CONST RPC_S_UNKNOWN_MGR_TYPE = 1716;
(* The manager type is unknown.*)

CONST RPC_S_UNKNOWN_IF = 1717;
(* The interface is unknown.*)

CONST RPC_S_NO_BINDINGS = 1718;
(* There are no bindings.*)

CONST RPC_S_NO_PROTSEQS = 1719;
(* There are no protocol sequences.*)

CONST RPC_S_CANT_CREATE_ENDPOINT = 1720;
(* The endpoint can not be created.*)

CONST RPC_S_OUT_OF_RESOURCES = 1721;
(* Not enough resources are available to complete this operation.*)

CONST RPC_S_SERVER_UNAVAILABLE = 1722;
(* The server is unavailable.*)

CONST RPC_S_SERVER_TOO_BUSY = 1723;
(* The server is too busy to complete this operation.*)

CONST RPC_S_INVALID_NETWORK_OPTIONS = 1724;
(* The network options are invalid.*)

CONST RPC_S_NO_CALL_ACTIVE = 1725;
(* There is not a remote procedure call active in this thread.*)

CONST RPC_S_CALL_FAILED = 1726;
(* The remote procedure call failed.*)

CONST RPC_S_CALL_FAILED_DNE = 1727;
(* The remote procedure call failed and did not execute.*)

CONST RPC_S_PROTOCOL_ERROR = 1728;
(* An rpc protocol error occured.*)

CONST RPC_S_CANNOT_BIND = 1729;
(* An attempt to bind with the server failed.*)

CONST RPC_S_UNSUPPORTED_TRANS_SYN = 1730;
(* The transfer syntax is not supported by the server.*)

CONST RPC_S_SERVER_OUT_OF_MEMORY = 1731;
(* The server has insufficient memory to complete this operation.*)

CONST RPC_S_UNSUPPORTED_TYPE = 1732;
(* The type uuid is not supported.*)

CONST RPC_S_INVALID_TAG = 1733;
(* The tag is invalid.*)

CONST RPC_S_INVALID_BOUND = 1734;
(* The array bounds are invalid.*)

CONST RPC_S_NO_ENTRY_NAME = 1735;
(* The binding does not contain an entry name.*)

CONST RPC_S_INVALID_NAME_SYNTAX = 1736;
(* The name syntax is invalid.*)

CONST RPC_S_UNSUPPORTED_NAME_SYNTAX = 1737;
(* The name syntax is not supported.*)

CONST RPC_S_SERVER_NOT_LISTENING = 1738;
(* The server is not listening for remote procedure calls.*)

CONST RPC_S_UUID_NO_ADDRESS = 1739;
(* No network address is available to use to construct a uuid.*)

CONST RPC_S_DUPLICATE_ENDPOINT = 1740;
(* The endpoint is a duplicate.*)

CONST RPC_S_UNKNOWN_AUTHN_TYPE = 1741;
(* The authentication type is unknown.*)

CONST RPC_S_MAX_CALLS_TOO_SMALL = 1742;
(* The maximum number of calls is too small.*)

CONST RPC_S_STRING_TOO_LONG = 1743;
(* The string is too long.*)

CONST RPC_S_PROTSEQ_NOT_FOUND = 1744;
(* The rpc protocol sequence was not found.*)

CONST RPC_S_PROCNUM_OUT_OF_RANGE = 1745;
(* The procedure number is out of range.*)

CONST RPC_S_BINDING_HAS_NO_AUTH = 1746;
(* The binding does not contain any authentication information.*)

CONST RPC_S_UNKNOWN_AUTHN_SERVICE = 1747;
(* The authentication service is unknown.*)

CONST RPC_S_UNKNOWN_AUTHN_LEVEL = 1748;
(* The authentication level is unknown.*)

CONST RPC_S_INVALID_AUTH_IDENTITY = 1749;
(* The security context is invalid.*)

CONST RPC_S_UNKNOWN_AUTHZ_SERVICE = 1750;
(* The authorization service is unknown.*)

CONST EPT_S_INVALID_ENTRY = 1751;
(* The entry is invalid.*)

CONST EPT_S_CANT_PERFORM_OP = 1752;
(* The operation can not be performed.*)

CONST EPT_S_NOT_REGISTERED = 1753;
(* There are not more endpoints available from the endpoint mapper.*)

CONST RPC_S_NO_INTERFACES_EXPORTED = 1754;
(* No interfaces have been exported.*)

CONST RPC_S_INCOMPLETE_NAME = 1755;
(* The entry name is incomplete.*)

CONST RPC_S_INVALID_VERS_OPTION = 1756;
(* The version option is invalid.*)

CONST RPC_S_NO_MORE_MEMBERS = 1757;
(* There are no more members.*)

CONST RPC_S_NOTHING_TO_UNEXPORT = 1758;
(* There is nothing to unexport.*)

CONST RPC_S_INTERFACE_NOT_FOUND = 1759;
(* The interface was not found.*)

CONST RPC_S_ENTRY_ALREADY_EXISTS = 1760;
(* The entry already exists.*)

CONST RPC_S_ENTRY_NOT_FOUND = 1761;
(* The entry is not found.*)

CONST RPC_S_NAME_SERVICE_UNAVAILABLE = 1762;
(* The name service is unavailable.*)

CONST RPC_S_INVALID_NAF_IF = 1763;
(* The network address family is invalid.*)

CONST RPC_S_CANNOT_SUPPORT = 1764;
(* The requested operation is not supported.*)

CONST RPC_S_NO_CONTEXT_AVAILABLE = 1765;
(* No security context is available to allow impersonation.*)

CONST RPC_S_INTERNAL_ERROR = 1766;
(* An internal error occured in rpc.*)

CONST RPC_S_ZERO_DIVIDE = 1767;
(* The server attempted an integer divide by zero.*)

CONST RPC_S_ADDRESS_ERROR = 1768;
(* An addressing error occured in the server.*)

CONST RPC_S_FP_DIV_ZERO = 1769;
(* A floating point operation at the server caused a divide by zero.*)

CONST RPC_S_FP_UNDERFLOW = 1770;
(* A floating point underflow occured at the server.*)

CONST RPC_S_FP_OVERFLOW = 1771;
(* A floating point overflow occured at the server.*)

CONST RPC_X_NO_MORE_ENTRIES = 1772;
(* The list of servers available for auto_handle binding has been exhausted.*)

CONST RPC_X_SS_CHAR_TRANS_OPEN_FAIL = 1773;
(* The file designated by DCERPCCHARTRANS cannot be opened.*)

CONST RPC_X_SS_CHAR_TRANS_SHORT_FILE = 1774;
(* The file containing the character translation table has fewer than*)
(* 512 bytes.*)

CONST RPC_X_SS_IN_NULL_CONTEXT = 1775;
(* A null context handle is passed as an [in] parameter.*)

CONST RPC_X_SS_CONTEXT_MISMATCH = 1776;
(* The context handle does not match any known context handles.*)

CONST RPC_X_SS_CONTEXT_DAMAGED = 1777;
(* The context handle changed during a call.*)

CONST RPC_X_SS_HANDLES_MISMATCH = 1778;
(* The binding handles passed to a remote procedure call do not match.*)

CONST RPC_X_SS_CANNOT_GET_CALL_HANDLE = 1779;
(* The stub is unable to get the call handle.*)

CONST RPC_X_NULL_REF_POINTER = 1780;
(* A null reference pointer was passed to the stub.*)

CONST RPC_X_ENUM_VALUE_OUT_OF_RANGE = 1781;
(* The enumeration value is out of range.*)

CONST RPC_X_BYTE_COUNT_TOO_SMALL = 1782;
(* The byte count is too small*)

CONST RPC_X_BAD_STUB_DATA = 1783;
(* The stub received bad data.*)

CONST ERROR_INVALID_USER_BUFFER = 1784;
(* The supplied user buffer is not valid for the requested operation.*)

CONST ERROR_UNRECOGNIZED_MEDIA = 1785;
(* The disk media is not recognized.  It may not be formatted.*)

CONST ERROR_NO_TRUST_LSA_SECRET = 1786;
(* The workstation does not have a trust secret.*)

CONST ERROR_NO_TRUST_SAM_ACCOUNT = 1787;
(* The domain controller does not have an account for this workstation.*)

CONST ERROR_TRUSTED_DOMAIN_FAILURE = 1788;
(* The trust relationship between the primary domain and the trusted*)
(* domain failed.*)

CONST ERROR_TRUSTED_RELATIONSHIP_FAILURE = 1789;
(* The trust relationship between this workstation and the primary*)
(* domain failed.*)

CONST ERROR_TRUST_FAILURE = 1790;
(* The network logon failed. This may be due the validation authority can't*)
(* be reached*)

CONST RPC_S_CALL_IN_PROGRESS = 1791;
(* A remote procedure call is already in progress for this thread.*)

CONST ERROR_NETLOGON_NOT_STARTED = 1792;
(* An attempt was made to logon, but the netlogon service was not started.*)

CONST ERROR_ACCOUNT_EXPIRED = 1793;
(* The user's account has expired.*)

CONST ERROR_REDIRECTOR_HAS_OPEN_HANDLES = 1794;
(* The redirector is in use and  cannot be unloaded.*)

CONST ERROR_PRINTER_DRIVER_ALREADY_INSTALLED = 1795;
(* The specified Printer Driver is already installed.*)

CONST ERROR_UNKNOWN_PORT = 1796;
(* The specified Port is unknown.*)

CONST ERROR_UNKNOWN_PRINTER_DRIVER = 1797;
(* The Printer Driver is unknown.*)

CONST ERROR_UNKNOWN_PRINTPROCESSOR = 1798;
(* The Print Processor is unknown.*)

CONST ERROR_INVALID_SEPARATOR_FILE = 1799;
(* The specified separator file is invalid.*)

CONST ERROR_INVALID_PRIORITY = 1800;
(* The specified Priority is invalid.*)

CONST ERROR_INVALID_PRINTER_NAME = 1801;
(* The Printer Name is invalid.*)

CONST ERROR_PRINTER_ALREADY_EXISTS: LONG = 1802;
(* The Printer already exists.*)

CONST ERROR_INVALID_PRINTER_COMMAND = 1803;
(* The Printer Command is invalid.*)

CONST ERROR_INVALID_DATATYPE = 1804;
(* The specified datatype is invalid.*)

CONST ERROR_INVALID_ENVIRONMENT = 1805;
(* The Environment specified is invalid.*)

END WinError.
 
