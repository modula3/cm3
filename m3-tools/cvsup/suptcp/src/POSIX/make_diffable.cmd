
@rem make this directory diffable with cm3/m3-comm/tcp/src

for %%a in (TCPPosix.i3 TCPHackNull.m3 TCPHack.i3 TCPHack.m3 TCP.m3) do (
  del %%a
  copy Sup%%a %%a
)
