# export PATH := $(PATH):$(HOME)/Desktop/BlitzMax/bin

BMK = bmk

all: b-repl

b-repl: B-LISP.bmx parse.bmx b-repl.bmx
	$(BMK) makeapp -w b-repl.bmx
