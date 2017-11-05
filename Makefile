# all: cabal
all:
	cd src && $(MAKE)

cabal:
	cabal configure && cabal build
	ln -sf dist/build/jvm-compiler/jvm-compiler insc_jvm
	ln -sf dist/build/llvm-compiler/llvm-compiler insc_llvm


cabal-clean:
	cabal clean

# clean: cabal-clean
clean:
	cd src && $(MAKE) clean

dist-clean: clean
	rm insc_jvm insc_llvm
