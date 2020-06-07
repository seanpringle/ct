all:
	mkdir -p build
	rm -rf build/*
	luajit parse2.lua
	cd build; ./build.sh
	build/main
