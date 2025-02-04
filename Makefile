# export PATH := $(PATH):$(HOME)/Desktop/BlitzMax/bin

BMK = bmk

all: b-repl

clean:
	rm -rf .bmx b-repl

b-repl: b-lisp.bmx parse.bmx b-repl.bmx
	$(BMK) makeapp -w -r -x b-repl.bmx
