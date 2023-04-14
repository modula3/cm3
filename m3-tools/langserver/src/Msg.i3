(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

INTERFACE Msg;

IMPORT RefSeq;

PROCEDURE ReadMessages();
PROCEDURE BuildInitReply(id : INTEGER) : TEXT;
PROCEDURE BuildShutdownReply(id : INTEGER) : TEXT;
PROCEDURE BuildErrorReply(id,error : INTEGER) : TEXT;
PROCEDURE BuildChangeReply(module,uri : TEXT; seq : RefSeq.T) : TEXT;
PROCEDURE BuildFoldingRangeReply(id : INTEGER) : TEXT;
PROCEDURE BuildHoverReply(id : INTEGER; hover : TEXT;
                          line, col, len : INTEGER) : TEXT;
PROCEDURE BuildDeclarationReply(id : INTEGER; uri : TEXT;
                                line, col, len : INTEGER) : TEXT;
PROCEDURE BuildTypeDefinitionReply(id : INTEGER; uri : TEXT;
                                   line, col, len : INTEGER) : TEXT;
(* server initiates *)
PROCEDURE BuildRegistration(id,method : TEXT) : TEXT;
PROCEDURE BuildUnregistration(id,method : TEXT) : TEXT;

END Msg.

