--
title: Marker Maker
keywords: chris, marker, movie, maker
tags: python, video
description: Splice images together in imitation of the dynamic and imperfect style of Chris Marker's <i>La Jetée</i>.
--

Chris Marker is one of my favorite documentary film makers but he is perhaps best known for his post-apocalyptic photo-montage-as-film, La jetée. I watched this recently and was astounded by the fluidity of the film and absolutely floored when those 100 or so frames of ‘actual’ film ran by. It is unfair to call the film static, it isn’t. The shots are never still due to either imperfections in the projectors gating mechanism, the slight flickering of the bulb, and the random grain and different exposure levels of each frame of film.

I decided to imitate this effect:
<object height="340" width="560" data="http://www.youtube.com/v/NVZD5BGIzPk&amp;hl=en_US&amp;fs=1&amp;rel=0" type="application/x-shockwave-flash"><param name="allowFullScreen" value="true"><param name="allowscriptaccess" value="always"><param name="src" value="http://www.youtube.com/v/NVZD5BGIzPk&amp;hl=en_US&amp;fs=1&amp;rel=0"><param name="allowfullscreen" value="true"></object>


[Here](/files/markermaker.py) is the python script and an [example](/files/film.xml) scene file.
Depends on mencoder. This is a proof of concept, it is slow (it’s video processing in python).


