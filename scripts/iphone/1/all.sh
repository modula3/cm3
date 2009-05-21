set -e
set -x

sh -c ./1iphonefilesystem.sh
sh -c ./trimfilesystem.sh
sh -c ./2csu.sh
sh -c ./3cctools.sh
sh -c ./4headers.sh
sh -c ./5llvm.sh
sh -c ./6hack.sh
