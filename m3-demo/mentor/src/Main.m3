(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue May  3 17:36:41 PDT 1994 by najork     *)
(*      modified on Thu Sep 24 16:12:53 PDT 1992 by mhb        *)
(*      modified on Thu Sep  3 18:04:23 PDT 1992 by johnh      *)
(*      modified on Thu Jul 23 00:51:45 1992 by steveg         *)

MODULE Main;

IMPORT MentorBundle, Rsrc, ZeusPanel;
IMPORT AlgFF;  <*NOWARN*>
IMPORT AlgFFCodeView;  <*NOWARN*>
IMPORT AlgFFPromptForWeights;  <*NOWARN*>
IMPORT AlgFFSimple;  <*NOWARN*>
IMPORT PackingView1;  <*NOWARN*>
IMPORT PackingView2;  <*NOWARN*>
IMPORT PackingView3;  <*NOWARN*>
IMPORT PackingView4;  <*NOWARN*>
IMPORT AlgBresenham;  <*NOWARN*>
IMPORT ViewError;  <*NOWARN*>
IMPORT ViewIncrementalReal;  <*NOWARN*>
IMPORT ViewLine;  <*NOWARN*>
IMPORT ViewOnlyLine;  <*NOWARN*>
IMPORT CPAlg;  <*NOWARN*>
IMPORT CPView;  <*NOWARN*>
IMPORT AdjMatrixView;  <*NOWARN*>
IMPORT AdjMatrixView01;  <*NOWARN*>
IMPORT AdjMatrixView01_K;  <*NOWARN*>
IMPORT DFS;  <*NOWARN*>
IMPORT DFSTC;  <*NOWARN*>
IMPORT DFSTreeView;  <*NOWARN*>
IMPORT GraphView;  <*NOWARN*>
IMPORT Warshall;  <*NOWARN*>
IMPORT Euclid;  <*NOWARN*>
IMPORT HashAlgs;  <*NOWARN*>
IMPORT HashStatsView;  <*NOWARN*>
IMPORT HashViews;  <*NOWARN*>
IMPORT ArrayView;  <*NOWARN*>
IMPORT GeomView;  <*NOWARN*>
IMPORT RubberAlg;  <*NOWARN*>
IMPORT WrapAlg;  <*NOWARN*>
IMPORT ShortestPath;  <*NOWARN*>
IMPORT Logo;  <*NOWARN*>
IMPORT AlgMatch;  <*NOWARN*>
IMPORT ViewMatch;  <*NOWARN*>
IMPORT MFAlgs;  <*NOWARN*>
IMPORT MFBarView;  <*NOWARN*>
IMPORT MFEdgeView;  <*NOWARN*>
IMPORT MFLegendView;  <*NOWARN*>
IMPORT MFViews;  <*NOWARN*>
IMPORT MFViews2;  <*NOWARN*>
IMPORT AlgMinimax;  <*NOWARN*>
IMPORT ViewGameBoard;  <*NOWARN*>
IMPORT A_BottomUp;  <*NOWARN*>
IMPORT A_RecDescent;  <*NOWARN*>
IMPORT A_TopDown;  <*NOWARN*>
IMPORT V_Input;  <*NOWARN*>
IMPORT V_Stack;  <*NOWARN*>
IMPORT V_Tree;  <*NOWARN*>
IMPORT AlgGreedy;  <*NOWARN*>
IMPORT AlgTwoPhase;  <*NOWARN*>
IMPORT PQAlgs;  <*NOWARN*>
IMPORT PQBarView;  <*NOWARN*>
IMPORT PQViews;  <*NOWARN*>
IMPORT BSTView;  <*NOWARN*>
IMPORT RedBlackAlg;  <*NOWARN*>
IMPORT SkelView;  <*NOWARN*>
IMPORT UnbalancedAlg;  <*NOWARN*>
IMPORT ChipsView;  <*NOWARN*>
IMPORT DotsView;  <*NOWARN*>
IMPORT SortAlgs;  <*NOWARN*>
IMPORT SticksView;  <*NOWARN*>
IMPORT AutoView;  <*NOWARN*>
IMPORT BM;  <*NOWARN*>
IMPORT BruteForce;  <*NOWARN*>
IMPORT KMP;  <*NOWARN*>
IMPORT PrefixView;  <*NOWARN*>
IMPORT SliderView;  <*NOWARN*>
IMPORT TextView;  <*NOWARN*>
IMPORT TouchView;  <*NOWARN*>
IMPORT AlgSubtype;  <*NOWARN*>
IMPORT AlgBoth;  <*NOWARN*>
IMPORT AlgCompress;  <*NOWARN*>
IMPORT AlgList;  <*NOWARN*>
IMPORT AlgQuickFind;  <*NOWARN*>
IMPORT AlgSimple;  <*NOWARN*>
IMPORT AlgUnionByRank;  <*NOWARN*>
IMPORT Wheeler;  <*NOWARN*>
(*
IMPORT WheelerDDO;  <*NOWARN*>
*)
IMPORT AlgFn;  <*NOWARN*>
IMPORT ViewFn;  <*NOWARN*>
IMPORT ViewHisto;  <*NOWARN*>
IMPORT ViewMFn;  <*NOWARN*>

IMPORT BinpackDataView;  <*NOWARN*>
IMPORT BinpackTranscriptView;  <*NOWARN*>
IMPORT BinpackmyviewObliqView;  <*NOWARN*>
IMPORT BresenhamDataView;  <*NOWARN*>
IMPORT BresenhamTranscriptView;  <*NOWARN*>
IMPORT CPDataView;  <*NOWARN*>
IMPORT CPTranscriptView;  <*NOWARN*>
IMPORT DGraphDataView;  <*NOWARN*>
IMPORT DGraphTranscriptView;  <*NOWARN*>
IMPORT EuclidDataView;  <*NOWARN*>
IMPORT EuclidTranscriptView;  <*NOWARN*>
IMPORT HashDataView;  <*NOWARN*>
IMPORT HashTranscriptView;  <*NOWARN*>
IMPORT HullDataView;  <*NOWARN*>
IMPORT HullTranscriptView;  <*NOWARN*>
IMPORT MatchDataView;  <*NOWARN*>
IMPORT MatchTranscriptView;  <*NOWARN*>
IMPORT MaxflowDataView;  <*NOWARN*>
IMPORT MaxflowTranscriptView;  <*NOWARN*>
IMPORT MinimaxDataView;  <*NOWARN*>
IMPORT MinimaxTranscriptView;  <*NOWARN*>
IMPORT MinimaxViewColorScaleObliqView;  <*NOWARN*>
IMPORT MinimaxViewGameTreeAObliqView;  <*NOWARN*>
IMPORT MinimaxViewGameTreeBObliqView;  <*NOWARN*>
IMPORT PQueueDataView;  <*NOWARN*>
IMPORT PQueueTranscriptView;  <*NOWARN*>
IMPORT ParseDataView;  <*NOWARN*>
IMPORT ParseTranscriptView;  <*NOWARN*>
IMPORT PktRouteDataView;  <*NOWARN*>
IMPORT PktRouteManyPacketsMoveObliqView;  <*NOWARN*>
IMPORT PktRouteManyPacketsStepObliqView;  <*NOWARN*>
IMPORT PktRoutePacketMoveObliqView;  <*NOWARN*>
IMPORT PktRoutePacketRouteObliqView;  <*NOWARN*>
IMPORT PktRoutePacketStepObliqView;  <*NOWARN*>
IMPORT PktRoutePacketTraceObliqView;  <*NOWARN*>
IMPORT PktRouteQueueSizesObliqView;  <*NOWARN*>
IMPORT PktRouteTranscriptView;  <*NOWARN*>
IMPORT SearchTreeDataView;  <*NOWARN*>
IMPORT SearchTreeTranscriptView;  <*NOWARN*>
IMPORT ShortestPathDataView;  <*NOWARN*>
IMPORT ShortestPathSPGraphObliqView;  <*NOWARN*>
IMPORT ShortestPathTranscriptView;  <*NOWARN*>
IMPORT SortDataView;  <*NOWARN*>
IMPORT SortTranscriptView;  <*NOWARN*>
IMPORT StringSearchDataView;  <*NOWARN*>
IMPORT StringSearchTranscriptView;  <*NOWARN*>
IMPORT SubtypeDataView;  <*NOWARN*>
IMPORT SubtypeTranscriptView;  <*NOWARN*>
IMPORT Subtypeview1ObliqView;  <*NOWARN*>
IMPORT UnionFindAverageDepthObliqView;  <*NOWARN*>
IMPORT UnionFindBigTreeObliqView;  <*NOWARN*>
IMPORT UnionFindChangeParentObliqView;  <*NOWARN*>
IMPORT UnionFindDataView;  <*NOWARN*>
IMPORT UnionFindFindLengthObliqView;  <*NOWARN*>
IMPORT UnionFindNodeDepthObliqView;  <*NOWARN*>
IMPORT UnionFindTranscriptView;  <*NOWARN*>
IMPORT UnionFindTreeObliqView;  <*NOWARN*>
IMPORT WheelerCompressObliqView;  <*NOWARN*>
IMPORT WheelerDataView;  <*NOWARN*>
IMPORT WheelerDecompressObliqView;  <*NOWARN*>
IMPORT WheelerTranscriptView;  <*NOWARN*>
IMPORT WheelerWhyDecompressWorksObliqView;  <*NOWARN*>
IMPORT ZPaperDataView;  <*NOWARN*>
IMPORT ZPaperTranscriptView;  <*NOWARN*>
IMPORT logoDataView;  <*NOWARN*>
IMPORT logoTranscriptView;  <*NOWARN*>

BEGIN
  ZeusPanel.Interact(
    "MENTOR: SRC Algorithm Animations",
    Rsrc.BuildPath ("$MENTORPATH", MentorBundle.Get()));
END Main.
