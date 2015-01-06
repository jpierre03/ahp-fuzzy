all:	run
	date

ahp.bin: ahp.cpp
	c++ ahp.cpp -o ahp.bin

run:	ahp.bin
	./ahp.bin

