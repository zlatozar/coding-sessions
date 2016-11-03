#_______________________________________________________________________________
#
#                       Makefile for CLRS
#_______________________________________________________________________________
#

export PYTHONPATH=$(shell echo $(PWD))

PYTHON=python

all: clean

clean: clean-pyc

clean-pyc:
	@find . -name '*.pyc' -exec rm -f {} +
	@find . -name '*~' -exec rm -f {} +
	@find . -name '__pycache__' -exec rm -rf {} +

.PHONY: clean-pyc
