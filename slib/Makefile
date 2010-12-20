SCHEME=gosh -u slib

all:
	$(SCHEME) wttree-test.scm

old:
	sed "s/(require 'wt-tree)/(load \".\/wttree-old.scm\")/" wttree-test.scm | $(SCHEME)

orig:
	$(SCHEME) wttest.scm
