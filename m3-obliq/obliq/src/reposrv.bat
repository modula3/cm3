@echo off

set arg=%1
if "%arg%"=="-std"  goto std
if "%arg%"=="-http"  goto http
if "%arg%"=="-trk"  goto trk
if "%arg%"=="-vr"  goto vr
if "%arg%"==""  goto default
goto error
:min
shift
repo-min -RepoSrv %*
goto end
:std
shift
:default
repo-std -RepoSrv %*
goto end
:trk
shift
repo-trk -RepoSrv %*
goto end
:vr
shift
coterie -RepoSrv %*
goto end
:error
echo Bad flag for repo: "%1"
echo "Usage: reposrv [ -std | -http | -trk | -vr ] ..."
:end
