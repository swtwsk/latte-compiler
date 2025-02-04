UNAME := $(shell uname -n)

all:
ifeq ($(UNAME), students)
	export PATH=/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:/home/students/inf/PUBLIC/MRJP/ghc-8.2.2:$$PATH; /home/students/inf/PUBLIC/MRJP/Stack/stack build
	export PATH=/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:/home/students/inf/PUBLIC/MRJP/ghc-8.2.2:$$PATH; /home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path=.
else
	stack build
	stack install --local-bin-path=.
endif

runtime:
	gcc -m32 -c lib/runtime.c -o lib/runtime.o
