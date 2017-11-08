all: insc_jvm insc_llvm

insc_jvm: MainJVM.hs
	ghc MainJVM.hs -o ../insc_jvm

insc_llvm: MainLLVM.hs
	ghc MainLLVM.hs -o ../insc_llvm

clean:
	cd Parser && '$(MAKE)' clean
	rm -rf *.o *.hi