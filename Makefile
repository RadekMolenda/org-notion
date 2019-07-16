.PHONY : test

LOADPATH = -l ./org-notion.el

test:
	cask exec ert-runner $(LOADPATH)
