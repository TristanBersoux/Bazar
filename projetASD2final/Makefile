export CC=g++
export CFLAGS=-W -Wall -std=c++17 -pedantic

main: main.o parser.o bloc.o strucBloc.o
	$(CC) -o main main.o parser.o bloc.o strucBloc.o $(CFLAGS)

main.o: main.cpp parser.hpp
	$(CC) -o main.o -I. -c main.cpp $(CFLAGS)

parser.o: parser.cpp parser.hpp
	$(CC) -o parser.o -I. -c parser.cpp $(CFLAGS)

bloc.o: bloc.cpp bloc.hpp
	$(CC) -o bloc.o -I. -c bloc.cpp $(CFLAGS)

strucBloc.o: strucBloc.cpp strucBloc.hpp
	$(CC) -o strucBloc.o -I. -c strucBloc.cpp $(CFLAGS)


clean:
	rm -rf *.o
cleanall:
	rm -rf *.o *~ main
