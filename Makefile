all:	ahp.bin
	date

ahp.bin: ahp.cpp
	c++ ahp.cpp -o ahp.bin

