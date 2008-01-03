#!/bin/sh
OLD="CVS src bindir.sh"
rm -rf .hidden
mkdir .hidden
mv ${OLD} .hidden
rm -rf *
mv .hidden/* .
m3build >/dev/null 2>&1
ls | grep -v CVS | grep -v src | grep -v bindir.sh
