all:
	luajit parse.lua gui vec2 vec3 main
	gcc -O1 -Wall -Werror -g -std=c11 -o test main.c -lSDL2 -lGL
	./test
