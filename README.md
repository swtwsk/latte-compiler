# Andrzej Swatowski - Latte

Andrzej Swatowski, as386085

Pierwsza wersja kompilatora języka Latte, napisanego w Haskellu

W pierwszej wersji kompilatora nie umieściłem żadnych rozszerzeń. Kompilator 
wylicza i optymalizuje stałe na poziomie drzewa składni abstrakcyjnej, tłumaczy 
AST na kod czwórkowy oraz generuje kod asemblera w wersji 32-bitowej.

## Projekt
### Uruchomienie
Projekt jest projektem Stackowym. Wywołanie polecenia `make` powinno zbudować 
poprawny plik wykonywalny `latc_x86`. Tym razem Makefile przygotowano tak, by 
sprawdzał czy znajduje się na `studentsie` i uruchamiał odpowiednią instalację 
Stacka z folderu `MRJP`.

Kompilator działa w wersji 32-bitowej, sprawdzano jego działanie na `students` 
oraz na komputerze z systemem Ubuntu 16.04 i gcc w wersji 5.4.0.

W przypadku błędów działania kompilatora poza `students` związanych 
z linkowaniem można spróbować uruchomić komendę `make runtime`, by na nowo 
skompilować plik `runtime.c` z folderu `lib`.

### Struktura
W folderze `app` znajduje się program główny `Main.hs`. W folderze `src` 
znajdują się pozostałe pliki projektu, podzielone na cztery foldery:
- `AST` zawiera pliki wygenerowane przez BNFC,
- `Frontend` zawiera kod odpowiedzialny za analizę semantyczną programu,
- `Backend` zawiera kod wykonujący optymalizacje, generujący kod czwórkowy 
(plik `QuadrupleGenerator.hs`) oraz generujący kod asemblera (`AsmGenerator.hs` 
w podfolderze `X86`),
- `Utils` zawiera drobne funkcje pomocnicze.

Plik `compileAsm.sh` jest skryptem wykonywalnym, który - wywołany przez 
`latc_x86` - kompiluje plik `.s` do pliku wykonywalnego.

Do rozwiązania należą też dwa foldery `lib` oraz `lib_students`, zawierające 
pliki z funkcjami pomocniczymi. W obydwu znajduje się ten sam plik `runtime.c`,
jednakże plik z `lib` skompilowany był na komputerze z Ubuntu 16.04, podczas gdy
plik `runtime.o` z `lib_students` kompilowany był bezpośrednio na `students`.
Dodatkowo, w folderze `lib_students` znajdują się pliki `libc.a`, `crt1.o`, 
`crti.o` oraz `crtn.o`, ponieważ `students` nie zawiera plików nagłówkowych dla
trybu 32-bitowego.

### Wykorzystane biblioteki
Rozwiązanie wykorzystuje biblioteki:

- mtl
- array
- containers
- ansi-terminal
- dlist
- lens
- integer-logarithms
- system-filepath
- filepath
- process.

`dlist` jest biblioteką udostępniającą Haskellową implementację
znanych z Prologa list różnicowych. Dzięki temu monada Writer może
działać znacznie wydajniej.
`ansi-terminal` ułatwia wyświetlanie kolorowych napisów w Linuxowym terminalu.
`lens` udostępnia łatwiejsze operacje na strukturach danych.
`integer-logarithms` jest gotową implementacją funkcji logarytmicznych dla 
liczb całkowitych - wykorzystuję ją podczas decydowania, czy powinienem użyć
operacji przesuwających bitów zamiast droższych `imul` oraz `idiv`.

Pozostałe biblioteki są częścią standardowej implementacji Haskella,
między innymi udostępniając słownik `Data.Map`, monad transformery, a
także umożliwiając wywoływanie procesów systemowych.
