---
layout: page
title: Projektid
---

Projekti [juhend](http://andmeteadus.github.io/2017/projekt_juhend/).
<br><br>

<h3>Tudengite poolt tehtud projektid</h3>
 

[Wikipedia muudatussõjad](http://htmlpreview.github.io/?https://github.com/andmeteadus/2017/blob/gh-pages/Wikipedia_muudatussojad.html)
<br><br>
Muudatussõda (edit-war) on nähtus, mille puhul artikli muutjad ei ole ühisel arusaamal artikli sisulistes küsimustes ning muudavad artiklit kordamööda, omale sobival moel. Näiteks on Nikola Tesla artiklit muudetud korduvalt lühikese aja jooksul, sest on erinevaid allikaid tema rahvuse kohta - näiteks on horvaadid ja austerlased tihti muutnud artiklit just nende rahvust soosivalt. Ühelt poolt oleks huvitav näha, millised artiklid need on ning teha nende põhjal lihtsamat statistikat, ent teisalt võib asjal olla ka meelelahutuslik külg - näiteks kui mõni kuulsus saab ebapopulaarse teoga hakkama, siis üsna tihti muudetakse sellest tingitult tema Wikipedia artiklit päris lõbusal moel. Käesoleva artikli eesmärk on juhendamata õppe meetodeid ning veebist kraabitud andmeid kasutades tuvastada selliseid artikleid, mille puhul võib tegemist olla muudatussõjaga. Uurimisprobleemi muudab keeruliseks märgendatu andmete puudumine, st ei ole teada millised reaalsuses on need artiklid, kus on muudatussõda toimunud, sh kuidas eristada muudatussõda normaalsetest ent muudatustest. Lisaks sellele muudab ülesande keeruliseks andmete rohkus.
<br>
[ (loe edasi) ](http://htmlpreview.github.io/?https://github.com/andmeteadus/2017/blob/gh-pages/Wikipedia_muudatussojad.html)
<br>
[IT-sektori tunnihinnad](http://htmlpreview.github.io/?https://github.com/andmeteadus/2017/blob/gh-pages/IT_tunnihinnad.html)

<br><br>

{% for post in site.posts %}
## [ {{ post.title }} ](..{{ post.url }})
  {{ post.content | strip_html | truncatewords:30}}
  [ (loe edasi) ](..{{ post.url }})
  <br><br>
  
{% endfor %}
