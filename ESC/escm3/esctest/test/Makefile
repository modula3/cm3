# ESC/Modula-3 test suite
#
# OUTPUTDIR must be set to the CM3 target (e.g., ARM64_DARWIN)

OUTPUTDIR ?= ARM64_DARWIN
ESCM3 = ../../esctool/$(OUTPUTDIR)/escm3
SIMPLIFY = ../../../Simplify/$(OUTPUTDIR)/Simplify
TFILE = m3imptab.txt

# Use installed package sources
M3PKG = $(shell dirname $(shell dirname $(shell which cm3)))/pkg

.PHONY: test test-fmt test-list test-seq test-golden clean

test: test-fmt

test-fmt:
	@echo "=== Phase 0: Fmt.i3 pragma extraction ==="
	$(ESCM3) -i "Fmt" -T$(TFILE)
	@echo ""

test-list:
	@echo "=== Phase 2: List.ig interface checking ==="
	$(ESCM3) -i "List" -T$(TFILE)
	@echo ""

test-seq:
	@echo "=== Phase 3: Sequence.addhi VC generation ==="
	$(ESCM3) -i "Sequence" "SequenceRep" -T$(TFILE)
	@echo ""

test-golden:
	@echo "=== Golden VC comparison ==="
	@echo "TODO: compare generated addhi VC against golden/addhi.sx"

clean:
	rm -f *.out *.diff
