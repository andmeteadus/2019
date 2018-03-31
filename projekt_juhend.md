---
layout: page
---

Aine läbimiseks tuleb läbi viia kas praktiline andmeanalüüs ning kirjutada saadud tulemustest populaarteaduslikus vormis artikkel või teha Shiny rakendus. Projekti tulem on illustreeritud asjakohaste joonistega ning on arusaadav ka mittestatistikule. Projekti võib teha kas üksinda või paaristööna. Tulemusi on vaja esitleda suulisel kaitsmisel.

Inspiratsiooniks saab vaadata [2015.](https://andmeteadus.github.io/2015/projektid/) ja [2017.](https://andmeteadus.github.io/2017/projektid/) aasta projekte. 

### Tähtajad (võivad veel muutuda)

* Teema valimine - 04.04.2018
* Projekti esitamise tähtaeg - 02.05.2018 kell 23.59.
* Projekti tulemuste müümine (st esitlus) - 09.05.2018

Mõned meie välja pakutud teemad on [siin](https://docs.google.com/spreadsheets/d/1OF39eei2jLbRxb31osRMTG1Kla8sOmcq9YYJBfcjPMA/edit#gid=0). Samas failis saab ka oma teemad registreerida.

### Mida on vaja esitada?

**a1. Artikkel**

Valminud populaarteaduslik artikkel tehakse avalikuks [aine veebilehel](../projektid/).

Vaja on esitada märgenduskeeles Markdown kirjutatud artikkel.
Kuna aine veebilehe postitused on kirjutatud Markdownis, siis on lihtsam projekte veebilehele lisada, kui need on tehtud Markdownis.
Näiteks selle sama lehe lähtekoodi näed [siit](https://raw.githubusercontent.com/andmeteadus/andmeteadus.github.io/2016/master/projekt_juhend.md).

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
