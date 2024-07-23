# export PATH := $(PATH):$(HOME)/Desktop/BlitzMax/bin

BMK = bmk

all: NaN

NaN: NaN.bmx
	$(BMK) makeapp -w NaN.bmx
