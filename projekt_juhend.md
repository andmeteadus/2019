﻿---
layout: page
---

Aine läbimiseks tuleb läbi viia kas praktiline andmeanalüüs ning kirjutada saadud tulemustest populaarteaduslikus vormis artikkel või teha Shiny rakendus. Projekti tulem on illustreeritud asjakohaste joonistega ning on arusaadav ka mittestatistikule. Projekti võib teha kas üksinda või paaristööna. Tulemusi on vaja esitleda suulisel kaitsmisel.

Inspiratsiooniks saab vaadata [2015.](https://andmeteadus.github.io/2015/projektid/), [2017.](https://andmeteadus.github.io/2017/projektid/) ja  [2018.](https://andmeteadus.github.io/2018/projektid/) aasta projekte. 

### Tähtajad 

* Teema valimine - 25.03.2019
* Projekti esitamise tähtaeg - 12.05.2019 kell 23.59.
* Projekti tulemuste müümine (st esitlus) - 20.05.2019 kell 10:15 ruumis 004

Mõned meie välja pakutud teemad on [siin](https://docs.google.com/spreadsheets/d/1pWFMwq3mPY0OXMGVlFcxJIqnfTv41c8Xvr4rNEOGfpg/edit#gid=0). Samas failis saab ka oma teemad registreerida.

### Mida on vaja esitada?

**a1. Artikkel**

Valminud populaarteaduslik artikkel tehakse avalikuks [aine veebilehel](../projektid/).

Vaja on esitada märgenduskeeles Markdown kirjutatud artikkel.
Kuna aine veebilehe postitused on kirjutatud Markdownis, siis on lihtsam projekte veebilehele lisada, kui need on tehtud Markdownis.
Näiteks selle sama lehe lähtekoodi näed [siit](https://raw.githubusercontent.com/andmeteadus/2019/gh-pages/projekt_juhend.md).

Artikli puhtandi võid kirjutada näiteks RStudios, aga see fail ei tohi sisaldada R-i koodi.
Joonise lisamiseks salvesta see eraldi pildifailina ja lisa see pilt Markdownis kirjutatud artiklisse näiteks nii:

```
![](joonis1.png)
```

aga mitte nii:

```
ggplot(data, aes(x, y)) + geom_point()
```

Abiks on järgmised [Markdowni näpunäited](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#images).

**a2. Rakendus**

Ka valminud Shiny rakendused teeme veebis kättesaadavaks. Esitada tuleb nii R-i kood(id) kui ka kasutatud andmetabelid.

**b. Kood**

Vaja on esitada andmeanalüüsi R-i kood.

Selle eesmärgiks on veenduda, et analüüs on reprodutseeritav. 

### Hindamine

* Kas populaarteaduslik artikkel / rakendus on põnev ja selge?
* Kas visualisatsioonid on atraktiivsed ja annavad vastuse uuritud küsimusele?
* Kas andmeanalüüs on reprodutseeritav?
