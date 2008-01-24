# Automation of miscellaneous administrative tasks on the web server

ARCHIVES := ${wildcard *.tgz}
ARCHIVES += ${wildcard *.zip}
ARCHIVES += ${wildcard *.bz2}
ARCHIVES += ${wildcard snaps/*.tgz}
ARCHIVES += ${wildcard snaps/*.zip}
ARCHIVES += ${wildcard snaps/*.bz2}

BASES := ${basename ${ARCHIVES}}
CKSUMS := ${addsuffix .cksum, ${BASES}}
MD5S := ${addsuffix .md5, ${BASES}}

.PHONY: all clean

all: ${CKSUMS} ${MD5S} archive.list archive.md5 archive.cksum
#       echo ${CKSUMS}

%.cksum: %.tgz
	cksum $< >$@

%.cksum: %.bz2
	cksum $< >$@

%.cksum: snaps/%.tgz
	cksum $< >$@

%.cksum: snaps/%.bz2
	cksum $< >$@

%.md5: %.tgz
	md5sum $< >$@

%.md5: %.bz2
	md5sum $< >$@

%.md5: snaps%.tgz
	md5sum $< >$@

%.md5: snaps%.bz2
	md5sum $< >$@

%.cksum: %.zip
	cksum $< >$@

%.cksum: snaps/%.zip
	cksum $< >$@

%.md5: %.zip
	md5sum $< >$@

%.md5: snaps/%.zip
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
