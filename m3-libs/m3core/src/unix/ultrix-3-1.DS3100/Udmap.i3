(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Mar  4 11:53:08 PST 1992 by muller        *)

INTERFACE Udmap;

FROM Ctypes IMPORT int, int_star;
FROM Utypes IMPORT swblk_t;

(*** <sys/dmap.h> ***)

TYPE
  struct_dmap = RECORD
    dm_last: int;        (* last element in dm_map *)
    dm_cnt: int;         (* count of elements  with swap *)
    dm_ptdaddr: int_star; (* address of disk address of page table *)
    dm_map: ARRAY [0..0] OF swblk_t ; (* first disk block number in each chunk *) 
    END;
  struct_dmap_star = UNTRACED REF struct_dmap;

  struct_dblock = RECORD
    db_base: swblk_t;        (* base of physical contig drum block *)
    db_size: swblk_t;        (* size of block *)
    END;

  struct_swapu_t = RECORD
    txt: int;        (* swap space for text *)
    smem: int;       (* swap space for shared memory *)
    total_used: int; (* total amount of swap space used *)
    wasted: int;     (* amount of swap space wasted *) 
    END;

<*EXTERNAL*> VAR swapu: struct_swapu_t;

TYPE
  struct_swfail_stat = RECORD
    data_ex_fail: int;
    stack_ex_fail: int;
    fork_fail: int;
    exec_fail: int;
    uarea_fail: int;
    lowswap_fail: int;
    frag_fail: int;
    text_dmap_fail: int;
    text_swap_fail: int;
    shm_dmap_fail: int;
    shm_swap_fail: int;
    END;

<*EXTERNAL*> VAR
  maxtsiz, maxdsiz, maxssiz: int;
  availswap, availvas: int;
  swapfrag: int;
  maxretry: int;

END Udmap.
