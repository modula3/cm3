# Automation of miscellaneous administrative tasks on the web server

ARCHIVES := ${wildcard *.tgz}

BASES := ${basename ${ARCHIVES}}
CKSUMS := ${addsuffix .cksum, ${BASES}}
MD5S := ${addsuffix .md5, ${BASES}}

.PHONY: all clean

all: ${CKSUMS} ${MD5S} archive.list archive.md5 archive.cksum
#       echo ${CKSUMS}

%.cksum: %.tgz
        cksum $< >$@

%.md5: %.tgz
        md5sum $< >$@

archive.cksum: ${CKSUMS}
        cat ${CKSUMS} >$@

archive.md5: ${MD5S}
        cat ${MD5S} >$@

archive.list: ${ARCHIVES}
        ls -l ${ARCHIVES} > $@

clean:
        rm -f ${MD5S}
        rm -f ${CKSUMS}
        rm -f archive.list archive.md5 archive.cksum

# memo
#       cksum $< |awk '{print $$1 $$2}' >$@
#       md5sum $< |awk '{print $$1}' >$@
