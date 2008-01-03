MODULE main EXPORTS Main;

IMPORT CSSAsync;
IMPORT CSSSerial;
IMPORT CSSAsyncMsg;
IMPORT Thread;


TYPE MonitorThread = Thread.Closure OBJECT
  OVERRIDES
    apply := MonitorThreadExec;
END;

PROCEDURE MonitorThreadExec(<*UNUSED*>self:MonitorThread): REFANY =
  VAR
    msg:CSSAsyncMsg.T;
  BEGIN
    msg := NEW(CSSAsyncMsg.T);
    msg.s := "ewqtrwrwettq";
    LOOP
      Thread.Pause(2.0D0);
      async.send(msg);
    END;
    
  END MonitorThreadExec;


VAR
  async: CSSAsync.T;
  hb : CSSAsyncMsg.T;
  
  
BEGIN
    hb := NEW(CSSAsyncMsg.T);
    hb.s := "+B";
    async := NEW(CSSAsync.T).init (hb,"/dev/cua0",CSSSerial.BaudRate.BR9600,
                                  CSSSerial.DataBits.DB7,
                                  CSSSerial.Parity.EVEN,
                                  CSSSerial.StopBits.SB2 );
  EVAL(Thread.Fork(NEW(MonitorThread)));

  LOOP
  
  END;

END main.
