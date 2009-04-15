
@rem make this directory diffable with cm3/m3-comm/tcp/src

for %%a in (ConnFD.i3 ConnRW.i3 ConnRW.m3 Errno.i3 ErrnoC.c TCP.i3) do (
  del %%a
  copy Sup%%a %%a
)
