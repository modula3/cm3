cd ..
tar32 cvf m3na.tar m3na
gzip32 m3na.tar
mv m3na.tar.gz m3na.tgz
rem after moving the file to UNIX space:
rem   gzip -d m3na.tgz
rem   tar xvf m3na.tar
rem   gzip m3na.tar
rem   mv m3na.tar.gz m3na
rem   chmod -R a+rx m3na
