INTERFACE Builder;

IMPORT Pathname, Wr, Thread;
IMPORT Pkg, Wx;

PROCEDURE InterruptBuild (root: Pkg.T);

PROCEDURE Build (root: Pkg.T;  pkg_dir: Pathname.T;  args: TEXT;  wx: Wx.T) 
                RAISES {Thread.Alerted, Wr.Failure};

PROCEDURE Clean (root: Pkg.T;  pkg_dir: Pathname.T;  wx: Wx.T) 
                RAISES {Thread.Alerted, Wr.Failure};

PROCEDURE Ship (root: Pkg.T;  pkg_dir: Pathname.T;  wx: Wx.T) 
               RAISES {Thread.Alerted, Wr.Failure};

PROCEDURE Run (root: Pkg.T;  prog, wd: Pathname.T;  wx: Wx.T) 
              RAISES {Thread.Alerted, Wr.Failure};

END Builder.
