---
layout: page
title: Projektid
---

Tudengite poolt tehtud projektid. Projekti juhendi leiad [siit](http://andmeteadus.github.io/2017/projekt_juhend/).
<br><br>


[Wikipedia muudatussõjad]{% include_relative /_projektid/Wikipedia_muudatussojad.html %}

<br><br>
[Wikipedia muudatussõjad ee](../Wikipedia_muudatussojad.html)

<br><br>


{% for post in site.posts %}
## [ {{ post.title }} ](..{{ post.url }})
  {{ post.content | strip_html | truncatewords:30}}
  [ (loe edasi) ](..{{ post.url }})
  <br><br>
  
{% endfor %}
