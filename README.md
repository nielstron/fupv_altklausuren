# FuPV Altklausuren

Beispielbearbeitungen von Aufgaben aus dem Fach Funktionale Programmierung und Verifikation (Einführung in die Informatik 2) der TU München

Dateien ohne `_sol` enthalten nur Struktur und Tests (unvollständig!), mit `_sol` enthalten Beispiellösungen.

Über PRs um weitere Klausuren oder Tests hinzuzufügen freue ich mich!

### Verfügbare Tests

- Wiederholungsklausur Wintersemester 16/17 (inzwischen vollständig)
- Wiederholungsklausur Wintersemester 15/16
    - Lift List/SearchTree

### Tests laufen lassen
```bash 
ocamlc -g -thread unix.cma threads.cma klausur1617wdh.ml && ./a.out
```
