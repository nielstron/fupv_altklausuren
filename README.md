# FuPV (EidI2) Altklausuren

Beispielbearbeitungen von Aufgaben aus dem Fach Funktionale Programmierung und Verifikation (Einführung in die Informatik 2) der TU München

Dateien ohne `_sol` enthalten nur Struktur und Tests (unvollständig!), mit `_sol` enthalten Beispiellösungen.

Über PRs um weitere Klausuren oder Tests hinzuzufügen freue ich mich!

### Verfügbare Tests

| Prüfung                                   | Teststatus                                                                        | Details                                    |
|-------------------------------------------|-----------------------------------------------------------------------------------|--------------------------------------------|
| Wiederholungsklausur Wintersemester 17/18 | ![Tests vollständig](https://img.shields.io/badge/tests-complete-success.svg)     |                                            |
| Klausur Wintersemester 17/18              | ![Tests vollständig](https://img.shields.io/badge/tests-complete-success.svg)     |                                            |
| Wiederholungsklausur Wintersemester 16/17 | ![Tests vollständig](https://img.shields.io/badge/tests-complete-success.svg)     |                                            |
| Klausur Wintersemester 16/17              | ![Tests vollständig](https://img.shields.io/badge/tests-complete-success.svg)     |                                            |
| Wiederholungsklausur Wintersemester 15/16 | ![Tests "in Arbeit"...](https://img.shields.io/badge/tests-in%20progress-yellow.svg) |  [ ] compress / merge  [x] Server [x] Memo |
| Klausur Wintersemester 15/16              | ![Tests vollständig](https://img.shields.io/badge/tests-complete-success.svg)     |                                            |

### Tests laufen lassen
```bash 
ocamlc -g -thread unix.cma threads.cma klausur1617wdh.ml && ./a.out
```

### Hinweis auf Unvollkommenheit

Selbstverständlich kann weder bei den Lösungsvorschlägen, noch bei den Tests ein Anspruch auf [Korrektheit oder Vollständigkeit](https://de.wikipedia.org/wiki/G%C3%B6delscher_Unvollst%C3%A4ndigkeitssatz) erhoben werden 
