SCHEME=gosh

all:
	$(SCHEME) wttree-test.scm

old:
	sed 's/wttree.scm/wttree-old.scm/' wttree-test.scm | $(SCHEME)

orig:
	sed "s/(require 'wt-tree)/(load \".\/wttree.scm\")/" wttest.scm | $(SCHEME)
