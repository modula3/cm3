/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

@UnbalancedTest
void UnbalancedTest(insCnt, delCnt, insKey, delKey)@
  int insCnt, delCnt;
  Key insKey[], delKey[];
{
  Tree t;
  Node *n;

  (* Insert "insCnt keys *)
  @1 t.root = NULL;@
  @2 for (i=0; i<insCnt; i++)@ {
    @3 n = malloc(sizeof(Node)); n->key = insKey[i];@
    @4 Insert(&t, n);@
  }
}
