(*--------------------------------------------------------------------------*)
MODULE DependencyGraph;

IMPORT Pathname, Text, Pickle, Rd, Wr, FileRd, FileWr, TextRd, 
       Thread;
IMPORT TextUtils, TextReadingUtils, SMsg AS Msg;
IMPORT StdDepGraph, StdDepGraphNode, StdDepGraphNodeSeq, 
       StdDepGraphEdge, StdNameNodeTbl;

(*--------------------------------------------------------------------------*)
REVEAL T = Public BRANDED "DependencyGraph 0.0 Object" OBJECT
    dgraph : StdDepGraph.T;
    table  : StdNameNodeTbl.T;
    updateClosure : UpdateClosure;
  METHODS
    addNodeToTable(name : TEXT; n : StdDepGraphNode.T) := AddNodeToTable;
  OVERRIDES
    init := Init;
    save := Save;
    load := Load;
    saveAsText := SaveAsText;
    loadAsText := LoadAsText;
    dump := Dump;
    reset := Reset;
    addElem := AddElem;
    addDependency := AddDependency;
    removeDependency := RemoveDependency;
    addMakefileDependencies := AddMakefileDependencies;
    addFromDependFile := AddFromDependFile;
    nodes := Nodes;
    nodeExists := NodeExists;
    getNode := GetNode;
    topologicalSort := TopologicalSort;
    nodesToBeUpdated := NodesToBeUpdated;
    dependingNodes := DependingNodes;
    nodeDependencies := NodeDependencies;
    setUpdateClosure := SetUpdateClosure;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE New(udc : UpdateClosure) : T =
  BEGIN
    RETURN NEW(T).init(udc);
  END New;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; udc : UpdateClosure) : T=
  BEGIN
    self.dgraph := NEW(StdDepGraph.T).init();
    self.table  := NEW(StdNameNodeTbl.Default).init(200);
    self.updateClosure := udc;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE Nodes(self : T) : StdDepGraphNodeSeq.T =
  VAR
    res  := NEW(StdDepGraphNodeSeq.T).init(self.table.size());
    iter := self.table.iterate();
    name : TEXT;
    node : StdDepGraphNode.T;
  BEGIN
    WHILE iter.next(name, node) DO
      res.addhi(node);
    END;
    RETURN res;
  END Nodes;

(*--------------------------------------------------------------------------*)
PROCEDURE SetUpdateClosure(self : T; udc : UpdateClosure) =
  BEGIN
    self.updateClosure := udc;
  END SetUpdateClosure;

(*--------------------------------------------------------------------------*)
PROCEDURE Save(self : T; fn : Pathname.T) : BOOLEAN =
  VAR
    wr : FileWr.T;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
    EXCEPT 
    ELSE
      Msg.Error2("DependencyGraph.Save()", "cannot open writer: " & fn);
      RETURN FALSE;
    END;
    TRY
      Pickle.Write(wr, self);
    EXCEPT
      Pickle.Error   => Msg.Error2("DependencyGraph.Save()",
                                   "pickle error: " & fn); RETURN FALSE;
    | Wr.Failure     => Msg.Error2("DependencyGraph.Save()",
                                   "writer failure: " & fn); RETURN FALSE;
    | Thread.Alerted => Msg.Error2("DependencyGraph.Save()",
                                   "thread alerted: " & fn); RETURN FALSE;
    END;
    TRY 
      Wr.Close(wr);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.Save()", "cannot close writer: " & fn);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Save;

(*--------------------------------------------------------------------------*)
PROCEDURE Load(self : T; fn : Pathname.T) : BOOLEAN =
  VAR
    ret    := TRUE;
    rd     :  FileRd.T;
    ngraph :  T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT 
    ELSE
      Msg.Error2("DependencyGraph.Load()", "cannot open reader: " & fn);
      RETURN FALSE;
    END;
    TRY
      ngraph := Pickle.Read(rd);
      self.dgraph := ngraph.dgraph;
      self.table := ngraph.table;
    EXCEPT
      Pickle.Error   => Msg.Error2("DependencyGraph.Load()",
                                   "pickle error: " & fn); ret := FALSE;
    | Rd.Failure     => Msg.Error2("DependencyGraph.Load()",
                                   "reader failure: " & fn); ret := FALSE;
    | Rd.EndOfFile   => Msg.Error2("DependencyGraph.Load()",
                                   "end of file: " & fn); ret := FALSE;
    | Thread.Alerted => Msg.Error2("DependencyGraph.Load()",
                                   "thread alerted: " & fn); ret := FALSE;
    END;
    TRY 
      Rd.Close(rd);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.Load()", "cannot close reader: " & fn);
      RETURN FALSE;
    END;
    RETURN ret;
  END Load;

(*--------------------------------------------------------------------------*)
PROCEDURE SaveAsText(self : T; fn : Pathname.T) : BOOLEAN =
  BEGIN
    (* open writer *)
    TRY
      dumpWr := FileWr.Open(fn);
    EXCEPT 
    ELSE
      Msg.Error2("DependencyGraph.SaveAsText()", "cannot open writer: " & fn);
      RETURN FALSE;
    END;
    dgraph := self.dgraph;
    (* save it *)
    TRY
      self.dgraph.mapOverNodes(SaveNodeAsText);
      Wr.PutText(dumpWr, "eof\n");
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.SaveAsText()", 
                 "error writing graph contents: " & fn);
    END;
    (* close writer *)
    TRY 
      Wr.Close(dumpWr);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.SaveAsText()", "cannot close writer: " & fn);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END SaveAsText;

(*--------------------------------------------------------------------------*)
PROCEDURE SaveNodeAsText(n1 : StdDepGraphNode.T) =
  VAR deps := "";
  BEGIN
    TRY
      FOR i := dgraph.nPred(n1) - 1 TO 0 BY -1 DO
        WITH pred = dgraph.getPredN(n1, i) DO
          deps := deps & " " & pred.name();
        END;
      END;
    EXCEPT ELSE
      Msg.Error2("DependencyGraph.SaveNodeAsText()", 
                 "cannot get predecessors of node " & n1.name());
    END;
    TRY
      IF n1.phony() THEN
        Wr.PutText(dumpWr, "phony " & n1.name() & "\n");
      ELSE
        Wr.PutText(dumpWr, "node " & n1.name() & "\n");
      END;
      Wr.PutText(dumpWr, n1.name() & ":" & deps & "\n");
      IF n1.action() = NIL THEN
        Wr.PutText(dumpWr, "	no_action\n");
      ELSE
        Wr.PutText(dumpWr, "	action " & n1.action() & "\n");
      END;
      Wr.PutText(dumpWr, "\n");
    EXCEPT ELSE
      Msg.Error2("DependencyGraph.SaveNodeAsText()", "error writing node " & 
        n1.name());
    END;
  END SaveNodeAsText; 

(*--------------------------------------------------------------------------*)
PROCEDURE LoadAsText(self : T; fn : Pathname.T) : BOOLEAN =
  VAR
    rd  :  FileRd.T;
    ret := TRUE;
    eof := FALSE;

  (*------------------------------------------------------------------------*)
  PROCEDURE NextToken() : TEXT 
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
    VAR token : TEXT;
    BEGIN
      REPEAT
        token := TextReadingUtils.GetToken(rd);
      UNTIL NOT Text.Empty(token);
      RETURN token;
    END NextToken;

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadNode() 
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
    VAR
      token  : TEXT;
      name   : TEXT;
      phony  : BOOLEAN;
      node   : BOOLEAN;
      deps   : TEXT;
      action : TEXT;

    (*----------------------------------------------------------------------*)
    PROCEDURE ReadStart() 
      RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted} =
      BEGIN
	token := NextToken();
	phony := Text.Equal(token, "phony");
	node  := Text.Equal(token, "node");
	eof   := Text.Equal(token, "eof");
      END ReadStart;

    (*----------------------------------------------------------------------*)
    BEGIN (* ReadNode *)
      ReadStart();
      WHILE NOT node AND NOT eof AND NOT phony DO
        Msg.Error2("DependencyGraph.ReadNode()", 
                   "skipped unexpected token " & token);
        ReadStart();
      END;
      IF eof THEN RETURN END;
      name := NextToken();
      REPEAT
        deps := TextUtils.Compress(Rd.GetLine(rd));
      UNTIL NOT Text.Empty(deps);
      token := NextToken();
      IF Text.Equal(token, "no_action") THEN
        action := NIL;
        EVAL Rd.GetLine(rd);
      ELSIF Text.Equal(token, "action") THEN
        action := TextUtils.Compress(Rd.GetLine(rd));
      ELSE
        Msg.Error2("DependencyGraph.ReadNode()", 
                   "skipped unexpected token " & token & "\n   " &
                   "not adding node " & name);
        RETURN;
      END;
      IF Msg.dFlag AND Msg.vFlag THEN
        VAR
          actionT := "NIL";
          phonyT  := "FALSE";
	BEGIN
	  IF action # NIL THEN actionT := action END;
	  IF phony THEN phonyT := "TRUE" END;
	  Msg.D("AddElem " & name & " phony = " & phonyT & " action = " &
	    actionT);
	  Msg.D("AddDeps " & deps);
        END;
      END;
      AddElem(self, name, action, phony);
      AddMakefileDependencies(self, deps);
    END ReadNode;

  (*------------------------------------------------------------------------*)
  BEGIN (* LoadAsText *)
    TRY
      rd := FileRd.Open(fn);
    EXCEPT 
    ELSE
      Msg.Error2("DependencyGraph.LoadAsText()", "cannot open reader: " & fn);
      RETURN FALSE;
    END;
    TRY
      WHILE NOT Rd.EOF(rd) AND NOT eof DO
        ReadNode();
      END;
    EXCEPT
      Rd.Failure     => Msg.Error2("DependencyGraph.Load()",
                                   "reader failure: " & fn); ret := FALSE;
    | Rd.EndOfFile   => Msg.Error2("DependencyGraph.Load()",
                                   "premature end of file: " & fn); 
                        ret := FALSE;
    | Thread.Alerted => Msg.Error2("DependencyGraph.Load()",
                                   "thread alerted: " & fn); ret :=FALSE;
    END;
    TRY 
      Rd.Close(rd);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.Load()", "cannot close reader: " & fn);
      RETURN FALSE;
    END;
    RETURN ret;
  END LoadAsText;

(*--------------------------------------------------------------------------*)
PROCEDURE ResetNode(n1 : StdDepGraphNode.T) =
  BEGIN
    n1.reset();
  END ResetNode;

(*--------------------------------------------------------------------------*)
PROCEDURE DumpNode(n1 : StdDepGraphNode.T) =
  VAR phony := "";
  BEGIN
    IF n1.phony() THEN
      phony := " (phony)";
    END;
    TRY
      Wr.PutText(dumpWr, n1.name() & phony & ":\n");
      IF n1.action() = NIL THEN
        Wr.PutText(dumpWr, "    no action\n");
      ELSE
        Wr.PutText(dumpWr, "    <=== " & n1.action() & "\n");
      END;
    EXCEPT ELSE
    END;
  END DumpNode;

(*--------------------------------------------------------------------------*)
PROCEDURE DumpEdge(n1 : StdDepGraphNode.T; <* UNUSED *>e : StdDepGraphEdge.T; 
                   n2 : StdDepGraphNode.T) =
  BEGIN
    TRY
      Wr.PutText(dumpWr, n2.name() & " <--- " & 
        n1.name() & "\n");
    EXCEPT ELSE
    END;
  END DumpEdge;

(*--------------------------------------------------------------------------*)
PROCEDURE Dump(self : T; wr : Wr.T) =
  BEGIN
    dumpWr := wr;
    TRY
      self.dgraph.mapOverNodes(DumpNode);
      self.dgraph.mapOverEdges(DumpEdge);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.Dump()", "unwanted exception");
    END;
  END Dump;

(*--------------------------------------------------------------------------*)
PROCEDURE AddElem(self : T; fn: TEXT; action : TEXT := NIL; phony := FALSE) =
  VAR
    node : StdDepGraphNode.T;
  BEGIN
    IF paranoid AND fn # NIL AND 
      Text.GetChar(fn, Text.Length(fn) - 1) = ':' THEN
      Msg.Fatal("AddElem " & fn);
    END;
    IF NodeExists(self, fn) THEN
      node := GetNode(self, fn);
      IF action = NIL THEN
        action := node.action();
      END;
      IF NOT phony THEN
        phony := node.phony();
      END;
      EVAL node.init(fn, action, phony);
      RETURN;
    END;
    node := StdDepGraphNode.New(fn, action, phony);
    IF self.dgraph.nodeExists(node) THEN
      Msg.Error("node for " & fn & " already exists in graph");
    ELSE
      AddNodeToTable(self, fn, node);
      self.dgraph.addNode(node); <* NOWARN *>
    END;
  END AddElem;

(*--------------------------------------------------------------------------*)
PROCEDURE AddDependency(self : T; source, dest: TEXT) =
  VAR
    s := GetNode(self, source);
    d := GetNode(self, dest);
    e := StdDepGraphEdge.New(1);
  BEGIN
    IF s = NIL THEN
      AddElem(self, source, NIL);
      s := GetNode(self, source);
    END;
    IF d = NIL THEN
      AddElem(self, dest, NIL);
      d := GetNode(self, dest);
    END;
    IF NOT self.dgraph.edgeExists(s, d) THEN
      TRY
        self.dgraph.setEdge(s, e, d);
      EXCEPT
        StdDepGraph.NoSuchNode => Msg.Error("cannot add dependency from " &
          source & " to " & dest);
      END;
    END;
  END AddDependency;

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveDependency(self : T; source, dest : TEXT) =
  VAR
    s := GetNode(self, source);
    d := GetNode(self, dest);
  BEGIN
    IF s = NIL THEN
      RETURN;
    END;
    IF d = NIL THEN
      RETURN;
    END;
    IF self.dgraph.edgeExists(s, d) THEN
      TRY
        self.dgraph.deleteEdge(s, d);
      EXCEPT
        StdDepGraph.NoSuchNode => Msg.Error("cannot remove dependency from " &
          source & " to " & dest & " no such node");
      | StdDepGraph.NoSuchEdge => Msg.Error("cannot remove dependency from " &
          source & " to " & dest & " no such edge");
      END;
    END;
  END RemoveDependency;

(*--------------------------------------------------------------------------*)
PROCEDURE AddMakefileDependencies(self : T; deps : TEXT) =
  VAR
    lrd, rd : TextRd.T;
    line, dest, rest, source : TEXT;
    i : INTEGER;
  BEGIN
    deps := TextUtils.Substitute(deps, "\\\r\n", "");
    deps := TextUtils.Substitute(deps, "\\\n", "");
    (* all line continuation sequences removed from deps *)
    lrd := TextRd.New(deps);
    TRY
      WHILE NOT Rd.EOF(lrd) DO
        line := Rd.GetLine(lrd);
        i := Text.FindChar(line, ':');
        IF i > 0 THEN 
          dest := TextUtils.Compress(Text.Sub(line, 0, i));
          rest := Text.Sub(line, i + 1,  Text.Length(line) - i);
          rd := TextRd.New(rest);
          TRY
            WHILE NOT Rd.EOF(rd) DO
              source := TextReadingUtils.GetToken(rd);
              IF paranoid AND TextUtils.Contains(source, ":") THEN
                Msg.Error2("DependencyGraph.AddMakefileDependencies()",
                           "element name with colon: " & source);
              ELSE
                AddDependency(self, source, dest);
              END;
            END;
          EXCEPT
            Rd.EndOfFile => (* skip *)
          ELSE
            Msg.Error2("DependencyGraph.AddMakefileDependencies()",
                       "error reading from text: " & line);
          END;
        ELSE
          IF NOT Text.Empty(TextUtils.Compress(line)) THEN
            Msg.Warning2("DependencyGraph.AddMakefileDependencies()",
                         "ignoring line " & line);
          END;
        END;
      END;
    EXCEPT 
      Rd.EndOfFile => (* skip *)
    ELSE
      Msg.Error2("DependencyGraph.AddMakefileDependencies()",
                 "error reading from text: " & deps);
    END;
  END AddMakefileDependencies;

(*--------------------------------------------------------------------------*)
PROCEDURE AddFromDependFile(self : T; fn : Pathname.T) : BOOLEAN =
  VAR 
    rd   : FileRd.T;
    deps : TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT 
    ELSE
      Msg.Error2("DependencyGraph.AddFromDependFile()", 
	"cannot open reader: " & fn);
      RETURN FALSE;
    END;
    TRY
      deps := Rd.GetText(rd, LAST(CARDINAL));
    EXCEPT
      Rd.Failure     => Msg.Error2("DependencyGraph.AddFromDependFile()",
                                   "reader failure: " & fn); RETURN FALSE;
    | Thread.Alerted => Msg.Error2("DependencyGraph.Alerted()",
                                   "thread alerted: " & fn); RETURN FALSE;
    END;
    AddMakefileDependencies(self, deps);
    TRY 
      Rd.Close(rd);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.AddFromDependFile()",
                 "cannot close reader: " & fn);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END AddFromDependFile;

(*--------------------------------------------------------------------------*)
PROCEDURE Reset(self : T) =
  BEGIN
    TRY
      self.dgraph.mapOverNodes(ResetNode);
    EXCEPT
    ELSE
      Msg.Error2("DependencyGraph.Reset()", "unwanted exception");
    END;
  END Reset;

(*--------------------------------------------------------------------------*)
PROCEDURE Trace(msg : TEXT; veryVerbose := FALSE) =
  BEGIN
    IF veryVerbose THEN
      IF Msg.dFlag AND Msg.vFlag THEN
        Msg.D("  " & msg);
      END;
    ELSE
      IF Msg.dFlag THEN
	Msg.D("  " & msg);
      ELSE
	Msg.V("  " & msg);
      END;
    END;
  END Trace;

(*--------------------------------------------------------------------------*)
PROCEDURE NeedsUpdate(self : T; node : StdDepGraphNode.T) : BOOLEAN =
  <* FATAL StdDepGraph.NoSuchNode, StdDepGraph.RangeFault *>
  BEGIN
    IF node.phony() THEN 
      Trace(node.name() & " is phony", TRUE);
      RETURN TRUE; 
    END;
    IF NOT self.updateClosure.exists(node) THEN 
      Trace(node.name() & " does not exist");
      RETURN TRUE;
    END;
    IF NOT NodeExists(self, node.name()) THEN
      Msg.Error("node " & node.name() &
        " missing in dependency graph");
      RETURN FALSE;
    END;
    FOR i := 0 TO self.dgraph.nPred(node) - 1 DO
      WITH pred = self.dgraph.getPredN(node, i) DO
	IF self.updateClosure.newer(pred, node) THEN
          Trace(pred.name() & " is newer than " & 
            node.name());
	  RETURN TRUE;
	END;
        IF pred.updated() THEN
          Trace(pred.name() & " has been updated, update " & 
            node.name());
          RETURN TRUE;
        END;
      END;
    END;
    Trace("no need to rebuild " & node.name(), TRUE);
    RETURN FALSE;
  END NeedsUpdate;

(*--------------------------------------------------------------------------*)
PROCEDURE NodesToBeUpdated(self : T; target : TEXT) : StdDepGraphNodeSeq.T =
  VAR
    nodes := NEW(StdDepGraphNodeSeq.T).init(self.dgraph.nodeSize());

  (*------------------------------------------------------------------------*)
  PROCEDURE Traverse(node : StdDepGraphNode.T) =
    <* FATAL StdDepGraph.NoSuchNode, StdDepGraph.RangeFault *>
    BEGIN
      IF NOT NodeExists(self, node.name()) THEN
        Msg.Error(node.name() & " missing in dependency graph");
        RETURN;
      END;
      IF node.visited() THEN
        RETURN;
      END;
      FOR i := 0 TO self.dgraph.nPred(node) - 1 DO
        WITH pred = self.dgraph.getPredN(node, i) DO
          Traverse(pred);
        END;
      END;
      IF NeedsUpdate(self, node) THEN
        IF node.action() # NIL THEN
          Trace("--> " & node.action());
          nodes.addhi(node);
        END;
        node.touch();
      END;
      node.seen();
    END Traverse;

  BEGIN (* NodesToBeUpdated *)
    IF NodeExists(self, target) THEN
      Reset(self);
      Traverse(GetNode(self, target));
    ELSE
      Msg.Error("unknown target node: " & target);
    END;
    RETURN nodes;
  END NodesToBeUpdated;

(*--------------------------------------------------------------------------*)
PROCEDURE TopologicalSort(self : T) : StdDepGraphNodeSeq.T =
  VAR
    nodes := NEW(StdDepGraphNodeSeq.T).init(self.dgraph.nodeSize());
    array :  REF ARRAY OF StdDepGraphNode.T;
    msg   := "The dependency graph contains a cycle:\n";
  BEGIN
    IF self.dgraph.topSort(array) THEN
      FOR i := FIRST(array^) TO LAST(array^) DO
        nodes.addhi(array[i]);
      END;
    ELSE
      (* dgraph is cyclic *)
      FOR i := FIRST(array^) TO LAST(array^) DO
        WITH nodeName = array[i].mName DO
          IF nodeName = NIL THEN
            msg := msg & " <unnamed node> ";
          ELSE
            msg := msg & nodeName & " ";
          END;
        END;
      END;
      Msg.Fatal(msg);
    END;
    RETURN nodes;
  END TopologicalSort;

(*--------------------------------------------------------------------------*)
PROCEDURE DependingNodes(self : T; start : TEXT) : StdDepGraphNodeSeq.T =
  VAR
    nodes := NEW(StdDepGraphNodeSeq.T).init(self.dgraph.nodeSize());

  (*------------------------------------------------------------------------*)
  PROCEDURE Traverse(node : StdDepGraphNode.T) =
    <* FATAL StdDepGraph.NoSuchNode, StdDepGraph.RangeFault *>
    BEGIN
      IF NOT NodeExists(self, node.name()) THEN
        Msg.Error(node.name() & " missing in dependency graph");
        RETURN;
      END;
      IF node.visited() THEN
        RETURN;
      END;
      node.seen();
      nodes.addhi(node);
      FOR i := 0 TO self.dgraph.nSucc(node) - 1 DO
        WITH succ = self.dgraph.getSuccN(node, i) DO
          Traverse(succ);
        END;
      END;
    END Traverse;

  BEGIN (* DependingNodes *)
    IF NodeExists(self, start) THEN
      Reset(self);
      Traverse(GetNode(self, start));
      IF nodes.size() > 0 THEN
        EVAL nodes.remlo();
      END;
    ELSE
      Msg.Error("unknown node: " & start);
    END;
    RETURN nodes;
  END DependingNodes;

(*--------------------------------------------------------------------------*)
PROCEDURE NodeDependencies(self : T; start : TEXT) : StdDepGraphNodeSeq.T =
  VAR
    deps := NEW(StdDepGraphNodeSeq.T).init(self.dgraph.nodeSize());
    n1 : StdDepGraphNode.T;
  BEGIN
    IF NodeExists(self, start) THEN
      n1 := GetNode(self, start);
    ELSE
      Msg.Error("unknown node: " & start);
      RETURN NIL;
    END;
    TRY
      FOR i := dgraph.nPred(n1) - 1 TO 0 BY -1 DO
        WITH pred = dgraph.getPredN(n1, i) DO
          deps.addhi(pred);
        END;
      END;
    EXCEPT ELSE
      Msg.Error2("DependencyGraph.SaveNodeAsText()", 
                 "cannot get predecessors of node " & n1.name());
    END;
    RETURN deps;
  END NodeDependencies;

(*--------------------------------------------------------------------------*)
PROCEDURE NodeExists(self : T; f : TEXT) : BOOLEAN =
  VAR n : StdDepGraphNode.T;
  BEGIN
    RETURN self.table.get(f, n);
  END NodeExists;

(*--------------------------------------------------------------------------*)
PROCEDURE GetNode(self : T; f : TEXT) : StdDepGraphNode.T =
  VAR n : StdDepGraphNode.T;
  BEGIN
    IF self.table.get(f, n) THEN
      RETURN n;
    ELSE
      RETURN NIL;
    END;
  END GetNode;

(*--------------------------------------------------------------------------*)
PROCEDURE AddNodeToTable(self : T; f : TEXT; n : StdDepGraphNode.T) =
  BEGIN
    EVAL self.table.put(f, n);
  END AddNodeToTable;

(*--------------------------------------------------------------------------*)
VAR
  dumpWr : Wr.T; (* used for dump and save operations *)
  dgraph : StdDepGraph.T; (* used for dump and save operations *)
BEGIN
END DependencyGraph.
