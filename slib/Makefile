SCHEME=gosh -u slib

all:
	$(SCHEME) wttree-test.scm

old:
	sed 's/wttree.scm/wttree-old.scm/' wttree-test.scm | $(SCHEME)

orig:
	$(SCHEME) wttest.scm
