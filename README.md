# FuPV (EidI2) Altklausuren

Beispielbearbeitungen von Aufgaben aus dem Fach Funktionale Programmierung und Verifikation (Einführung in die Informatik 2) der TU München

Dateien ohne `_sol` enthalten nur Struktur und Tests (unvollständig!), mit `_sol` enthalten Beispiellösungen.

Über PRs um weitere Klausuren oder Tests hinzuzufügen freue ich mich!

### Verfügbare Tests

- Klausur Wintersemester 17/18 (inzwischen vollständig)
- Wiederholungsklausur Wintersemester 16/17 (inzwischen vollständig)
- Klausur Wintersemester 16/17 (inzwischen vollständig)
- Wiederholungsklausur Wintersemester 15/16
    - [ ] compress / merge
    - [x] Server
    - [x] Memo
- Klausur Wintersemester 15/16 (inzwischen vollständig)

### Tests laufen lassen
```bash 
ocamlc -g -thread unix.cma threads.cma klausur1617wdh.ml && ./a.out
```

### Hinweis auf Unvollkommenheit

Selbstverständlich kann weder bei den Lösungsvorschlägen, noch bei den Tests ein Anspruch auf [Korrektheit oder Vollständigkeit](https://de.wikipedia.org/wiki/G%C3%B6delscher_Unvollst%C3%A4ndigkeitssatz) erhoben werden 
