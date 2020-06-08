
all:
	$(MAKE) parse
	$(MAKE) build

build: CFLAGS=-O2 -g -Wall -Werror
build: LDFLAGS=-lSDL2 -lGL
build: $(shell ls -1 build/*.c | sed 's/c$$/o/g')
	$(CC) $(CFLAGS) -flto -o main build/*.o $(LDFLAGS)

build/%.o: build/%.c
	$(CC) $(CFLAGS) -c $< -o $@

parse:
	mkdir -p build
	rm -rf build/*
	luajit ct.lua *.ct
