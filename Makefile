all: cabal

cabal:
	cabal configure && cabal build
	ln -sf dist/build/jvm-compiler/jvm-compiler insc_jvm
	ln -sf dist/build/llvm-compiler/llvm-compiler insc_llvm


clean:
	cabal clean