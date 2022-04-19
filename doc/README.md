# FLP 2021/2022 – funkcionální projekt: Haskell

Varianta: DKA-2-MKA

## Testy
Součástí projektu jsou automatické testy, které obstarává `Makefile`. Veškeré testy jsou uloženy ve složce `tests`. Jejich spuštění je následující:

- `make test-invalid` spustí testy pro ověření korektního výpisu jednotlivých chyb
- `make test-print` automatické vyhodnocení testů pro výpis DKA (parametr programu `-i`)
- `make test-minimize` automatické vyhodnocení testů pro výpis MKA (parametr programu `-t`)
- `make tests` spustí oba automatické testy

Automatické testy se vyhodnocují pomocí příkazu **diff**, kterému je možné předat libovolný argument (viz. [diff-man](https://man7.org/linux/man-pages/man1/diff.1.html)), a to přidáním nepovinného parametru `arg=X`, kde `X` je argument příkazu diff, tedy například `make test-minimize arg=r`. Defaultně (bez `arg=X`) je příkaz spouštěn s argumentem `-q`.

Test `make test-invalid` není vyhodnocen automaticky, protože vyžaduje ruční kontrolu.

Protože testů není nikdy dost, byly přizpůsobeny a využity již existující testy nalezené na githubu dostupného [zde](https://github.com/vokracko/FLP-DKA-2-MKA-test).

## Omezení
Pro přehlednější výpis je při výstupu programu přidán nový řádek na konec.

Seznam koncových stavů a pravidel může být dle definice prázdný, program tak považuje prázdný řádek za prázdný seznam. Například automat s prázdným seznamem koncových stavů je přijímán i vypisován v následujícím formátu:
```
1,2
ab
1

1,a,1
1,b,2
```
