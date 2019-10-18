# Cur Makefile

install:
	raco pkg install -D --auto -t dir cur-lib/ cur-test/

test:
	raco test -j 4 -p cur-test

remove:
	raco pkg remove --auto cur-test cur-lib
