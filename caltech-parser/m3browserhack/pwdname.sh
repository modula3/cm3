#!/bin/sh
echo "echo _unused_ `pwd | sed s/\\\//\;echo\ /g`" | sh | tail -n 1
