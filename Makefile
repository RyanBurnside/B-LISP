# export PATH := $(PATH):$(HOME)/Desktop/BlitzMax/bin

BMK = bmk

all: B-LISP

B-LISP: B-LISP.bmx
	$(BMK) makeapp -w B-LISP.bmx
