git show | findstr /v /e /i cmd | findstr /b /c:"--- a/" | findstr /v git | wsl.exe perl -p -e "s/^--- a\///" | wsl.exe xargs dos2unix
