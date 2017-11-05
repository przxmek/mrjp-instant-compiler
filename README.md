# instant-compiler
## Przemyslaw Kuczynski (pk334685@students.mimuw.edu.pl)

### Budowanie
```bash
make
```

### Użycie
```bash
./insc_jvm test/examples/test01.ins
cd test/examples
java test01
```

```bash
./insc_llvm test/examples/test01.ins
lli test/examples/test01.bc
```

### Zewnętrzne narzędzia i biblioteki
* Jasmin
* bnfc

### Struktura projektu
* `lib` 
  * `jasmin.jar` - Jasmin (wersja 2.2 z PUBLIC_MRJP)
* `test` - Katalog testowy (dostarczony razem z treścią zadania)
* `src` - Katalog z kodem źródłowym
  * `Parser` - kod wygenerowany przez `bnfc`
    * `Instant.cf` - gramatyka w formacie BNFC
  * `CompilerJVM.hs`, `CompilerLLVM.hs` - Zasadniczy kod obu kompilatorów
  * `MainJVM.hs`, `MainLLVM.hs` - Punkt wejścia obu kompilatorów 
  * `MainCommon.hs` - Dzielone funkcjonalności
