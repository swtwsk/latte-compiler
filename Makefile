UNAME := $(shell uname)

all:
ifeq ($(UNAME), students)
	$(shell export PATH=/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:/home/students/inf/PUBLIC/MRJP/ghc-8.2.2:$PATH)
	/home/students/inf/PUBLIC/MRJP/Stack/stack build
	/home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path=.
else
	stack build
	stack install --local-bin-path=.
endif
