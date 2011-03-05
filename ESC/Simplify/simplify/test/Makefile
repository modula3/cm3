# Simp=../AOSF/Simplify
# Simp=../LINUXLIBC6/Simplify
# Simp=../PPC_DARWIN/Simplify
# Simp=/proj/m3/pkg/simplify/AOSF/Simplify
# Simp=/proj/m3/pkg/simplify/LINUXLIBC6/Simplify

Simp = ../$(OUTPUTDIR)/Simplify

test:
	@echo "The following should be valid:" > /tmp/out
	$(Simp) Examples.valid >> /tmp/out
	(export AXIOMDIR; AXIOMDIR=.; \
            $(Simp) -ax escv1 Examples.esc.valid >> /tmp/out)
	(export AXIOMDIR; AXIOMDIR=.; \
            $(Simp) -ax escv3 Examples.esc3.valid >> /tmp/out)
	(export AXIOMDIR; AXIOMDIR=.; \
	    $(Simp) -ax necula1 necula1.sx >> /tmp/out) 
	@echo "" >> /tmp/out
	@echo "The following should be invalid:" >> /tmp/out
	$(Simp) Examples.invalid >> /tmp/out
	(export AXIOMDIR; AXIOMDIR=.; \
            $(Simp) -ax escv1 Examples.esc.invalid >> /tmp/out)
	(export AXIOMDIR; AXIOMDIR=.; \
	    $(Simp) -ax escv2 Examples.esc2.invalid >> /tmp/out)
	(export AXIOMDIR; AXIOMDIR=.; \
	    $(Simp) -ax saxe1 saxe1.input >> /tmp/out)
	@echo "These should be mixed:" >> /tmp/out
	$(Simp) PredTest.sx >> /tmp/out
	$(Simp) -ax esc PredTest.esc.sx >> /tmp/out
	$(Simp) -ax esc OrdersTest.sx >> /tmp/out
	@echo "" >> /tmp/out
	@echo Following diffs may not be empty, if satisfying contexts changed
	(diff /tmp/out test.ans || exit 0)
	@echo Following diffs should be empty.
	(diff /tmp/out test.ans | egrep "[:] Valid|[:] Invalid|[:] Unknown[.]" || exit 0)
	mv /tmp/out test.out

