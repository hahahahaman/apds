OBJS = main.o data.o algorithms.o
CC = g++
DEBUG = -g
CFLAGS = -std=c++11 -Wall -c $(DEBUG)
LFLAGS = -Wall $(DEBUG)

main : $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -o testing.bin

main.o : main.cpp data.h algorithms.h
	$(CC) $(CFLAGS) main.cpp

data.o : data.h data.cpp
	$(CC) $(CFLAGS) data.cpp

algorithms.o : algorithms.h algorithms.cpp
	$(CC) $(CFLAGS) algorithms.cpp

clean:
	\rm *.o testing.bin
