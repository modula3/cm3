filelist="
dbdsqr.f
dgebak.f
dgebal.f
dgebd2.f
dgebrd.f
dgecon.f
dgeequ.f
dgees.f
dgeesx.f
dgegs.f
dgehd2.f
dgehrd.f
dgelq2.f
dgelqf.f
dgels.f
dgelsx.f
dgeqpf.f
dgeqr2.f
dgeqrf.f
dgerfs.f
dgerq2.f
dgerqf.f
dgesvx.f
dgetf2.f
dgetrf.f
dgetri.f
dgetrs.f
dggbak.f
dggbal.f
dgghrd.f
dhgeqz.f
dhseqr.f
dlabad.f
dlabrd.f
dlacon.f
dlacpy.f
dladiv.f
dlae2.f
dlaev2.f
dlaexc.f
dlag2.f
dlahqr.f
dlahrd.f
dlaic1.f
dlaln2.f
dlange.f
dlanhs.f
dlansp.f
dlanst.f
dlansy.f
dlantr.f
dlanv2.f
dlapmt.f
dlapy2.f
dlapy3.f
dlaqge.f
dlarf.f
dlarfb.f
dlarfg.f
dlarft.f
dlarfx.f
dlartg.f
dlas2.f
dlascl.f
dlaset.f
dlasq1.f
dlasq2.f
dlasq3.f
dlasq4.f
dlasr.f
dlasrt.f
dlassq.f
dlasv2.f
dlaswp.f
dlasy2.f
dlasyf.f
dlatrs.f
dlatzm.f
dopgtr.f
dorg2l.f
dorg2r.f
dorgbr.f
dorghr.f
dorgl2.f
dorglq.f
dorgql.f
dorgqr.f
dorgr2.f
dorgrq.f
dorgtr.f
dorm2r.f
dormbr.f
dormhr.f
dorml2.f
dormlq.f
dormqr.f
dormr2.f
dormrq.f
dpocon.f
dpotrs.f
dpptrf.f
drscl.f
dspev.f
dspgst.f
dspgv.f
dsptrd.f
dsptrf.f
dsteqr.f
dsterf.f
dsycon.f
dsyev.f
dsytf2.f
dsytrf.f
dsytri.f
dsytrs.f
dtgevc.f
dtrcon.f
dtrexc.f
dtrsen.f
dtrsyl.f
dtrti2.f
dtrtri.f
dtzrqf.f
"

# dgeqlf.f
# dgesvd.f
# dgges.f
# dlamch.f
# dormql.f
# dpotrf.f
# dsytrd.f


(
echo "INTERFACE LongRealMatrixLapack;" ;
for file in $filelist; do echo $file >&2; \
echo ; \
f2i3.sh /usr/lib/scilab/routines/lapack/$file; \
done ;
echo "END FloatMatrixLapack."
) | m3pp > LongRealMatrixLapackTest.i3
