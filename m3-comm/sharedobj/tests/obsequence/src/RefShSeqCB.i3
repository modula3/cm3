INTERFACE RefShSeqCB;

IMPORT RefShSeq, SharedObj;

TYPE
  T <: Public;
  Public = SharedObj.Callback OBJECT 
    METHODS
      pre_init(READONLY obj: RefShSeq.T; 
               READONLY sizeHint: CARDINAL := 5): BOOLEAN;
      pre_fromArray(READONLY obj: RefShSeq.T; 
                    READONLY a: ARRAY OF Elem.T): BOOLEAN;
      pre_addhi(READONLY obj: RefShSeq.T; READONLY x: Elem.T): BOOLEAN;
      pre_addlo(READONLY obj: RefShSeq.T; READONLY x: Elem.T): BOOLEAN;
      pre_remhi(READONLY obj: RefShSeq.T): BOOLEAN;
      pre_remlo(READONLY obj: RefShSeq.T): BOOLEAN;
      pre_put(READONLY obj: RefShSeq.T; READONLY i: CARDINAL; 
              READONLY x: Elem.T): BOOLEAN;
      pre_anyChange(READONLY obj: RefShSeq.T);

      post_init(READONLY obj: RefShSeq.T; 
               READONLY sizeHint: CARDINAL := 5): BOOLEAN;
      post_fromArray(READONLY obj: RefShSeq.T; 
                    READONLY a: ARRAY OF Elem.T): BOOLEAN;
      post_addhi(READONLY obj: RefShSeq.T; READONLY x: Elem.T): BOOLEAN;
      post_addlo(READONLY obj: RefShSeq.T; READONLY x: Elem.T): BOOLEAN;
      post_remhi(READONLY obj: RefShSeq.T): BOOLEAN;
      post_remlo(READONLY obj: RefShSeq.T): BOOLEAN;
      post_put(READONLY obj: RefShSeq.T; READONLY i: CARDINAL; 
              READONLY x: Elem.T): BOOLEAN;
      post_anyChange(READONLY obj: RefShSeq.T);
    END;

END RefShSeqCB.
