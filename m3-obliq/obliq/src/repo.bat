@echo off

set arg=%1
if "%arg%"=="-min"  goto min
if "%arg%"=="-std"  goto std
if "%arg%"=="-http"  goto http
if "%arg%"=="-trk"  goto trk
if "%arg%"=="-vr"  goto vr
if "%arg%"==""  goto default
goto error
:min
shift
repo-min %*
goto end
:std
shift
:default
repo-std %*
goto end
:trk
shift
repo-trk %*
goto end
:vr
shift
coterie %*
goto end
:error
echo Bad flag for repo: "%1"
echo Usage: "repo [ -min | -std | -http | -trk | -vr ] ..."
:end
