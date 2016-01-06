call ..\env\clean2.cmd
call ..\env\winddk\3790\wxp\x86.cmd

rd /q/s \cm3.3
xcopy /fivery \cm3-5.8.6-min \cm3.3
.\make-dist-cfg.py
.\upgrade-full.cmd
