all:
	./opam_build false

install:
	./opam_build true

test:
	./opam_build test

%-build:
	OS=$* ./build

%-install:
	OS=$* ./build true

%-test:
	OS=$* ./build test

clean:
	rm -rf _build
