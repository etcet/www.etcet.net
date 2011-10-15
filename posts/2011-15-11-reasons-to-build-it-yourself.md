--
title: Reasons to Build it Yourself
tags: thoughts
keywords: not invented here syndrome, programming, thoughts
--

When you tell you are writing a new library, chances are that the obnoxious me is right by telling you "It's been done before, just use that". This is a fact of software development, inherrent to the large amount of [code monkeys], cranking out software since 1950's [machine language programs]. Should it stop you from writing it? No! Let me explain the odd connection between two reasons for ignoring the downfalls of "Not Invented Here" (NIH) syndrome and becoming  a [programming master]. 

## You Want to Understand

"Not Invented Here" syndrome describes ignoring existent products, research, knowledge or, in our case, software from third parties and creating it your damn self (and than doing it multiple times, therefore the syndrome). Many negative aspects are justifiable ascribed to the syndrome, most important one: _"Why you would want te reinvent the wheel?"_. Where I reply _"Because I want to understand the wheel and build a better one!"_.

A few years back when I started with Django I decided to build my own blogging software while I already had the choice from a dozen libraries doing the exact same thing. I believed this was needed because I wanted to learn the framework. I could try reading myself to sleep with Django's documentation, but waking up, anxious to code, would have me scratching a bold spot within the hour. But looking at others code, while at the meanwhile writing my own enabled me to understand the framework.

This understanding demonstrates the first rule for ignoring the NIH monster. __Ignore it when you are learning.__ May it be a new technique, framework or language, you don't understand it untill you use it. You don't understand it untill you complete the process of idea to working implementation.


## You Can do it Better

When I was a year into Django we needed a account managing application for [Bread & Pepper]. The ``GOTO`` application was [django-registration]. At this point I was comfortable with [Python] and [Django] and decided to create my own account application called [django-userena]. I was convinced I could do it better. The judge is still out on that, but I think I succeeded for at least some use cases. This is the second reason for building it yourself: **You can do it better**.

Beware, this reason should be used cautiously because it can hamper the quality of our professions products when your "do it yourself" attitude stems from a minor modification or improvement towards an _open-sourced_ library. It would therefore be wise to contact the author and propose your changes, maybe it's something they wanted to do themself or already tried and then share their reasons for not implementing it. If you can't work it out together, do it yourself.

## Learn and Then Do It Better

It's hard to make a dent[^1] in the current software world -- been trying for a few years, but only expect to make my first scratch in the next five -- but let me disclose my [master plan]. The "[Pragmatic Thinking and Learning]" book introduced me to the concept of Shuhari (守破離). Following citation from [Endō Seishirō] describes it best:

> It is known that, when we learn or train in something, we pass through the stages of shu, ha, and ri. These stages are explained as follows. In shu, we repeat the forms and discipline ourselves so that our bodies absorb the forms that our forebearers created. We remain faithful to the forms with no deviation. 

> Next, in the stage of ha, once we have disciplined ourselves to acquire the forms and movements, we make innovations. In this process the forms may be broken and discarded. 

> Finally, in ri, we completely depart from the forms, open the door to creative technique, and arrive in a place where we act in accordance with what our heart/mind desires, unhindered while not overstepping laws. 

I'm willing to argue that I was in the stage of _shu_ when I was writing another Django blog engine. Been done before... been done before better. But a lot of programmers don't have access to a mentor which challenges them and shows them best practices. Luckily, in the open-source world there is a steady supply of code available from first-class programmers. Read it, duplicate it and understand it.

_Ha_ was me working on [django-userena], knowing the framework and creating innovations, on a miniature scale. Although there may be software that does what you want, being a _ha_ programmer, you are able to improve it. I couldn't have done it without first going through the _shu_ stage, where I got to know the language and framework by blindly copying my superiors.

## Creating a Dent

We came full circle on a backboard philosophy on how to become an expert (Ri) programmer. At first, don't mind that you are reinventing the wheel by copying from a superior programmer. Rinse and repeat untill you reach the stage of _ha_ where you are familiar with the language, framework and are able to create software that stands on it's own. This time you are the one who can supply others with code to learn from. Copying and improving others is part of the slow road to expertise. Finally your knowledge will allow you to break free and reach the stage of a _ri_ programmer[^2]. At this stage, feel free to create your dent in the universe with new innovative software[^3].

[^1]: Tribute to the late Steve Jobs's "We’re here to put a dent in the universe. Otherwise why else even be here?" quote.
[^2]: There are not many who can claim this title, but one of them is the late [Dennis Ritchie], father of C programming language.
[^3]: I _do_ believe that even a novice can create innovative products, but this usually stems from a expertise in a different field which is applicable to the field where they are seen as a novice.

[code monkeys]: http://en.wikipedia.org/wiki/Infinite_monkey_theorem "Wikipedia page about the infinite monkey theorem"
[machine language programs]: http://en.wikipedia.org/wiki/Machine_code "Wikipedia Page about Machine code"
[programming master]: http://zedshaw.com/essays/master_and_expert.html "Zed Shaw's article on becoming a programming master"

[Bread & Pepper]: http://breadandpepper.com "Bread & Pepper Homepage"
[django-registration]: https://bitbucket.org/ubernostrum/django-registration/ "Bitbucket Repository of django-registration"
[Python]: http://www.python.org/ "Python Programming Language homepage"
[Django]: https://www.djangoproject.com/ "Django project homepage"
[django-userena]: https://github.com/bread-and-pepper/django-userena "Github repository of django-userena"
[Pragmatic Thinking and Learning]: http://www.amazon.com/dp/1934356050/?tag=wunki-20 "Pragmatic Thinking and Learning on Amazon"
[master plan]: http://www.youtube.com/watch?v=PPfuDCbhu3c "Video clip of YZ with Thinking of a master plan"
[Endō Seishirō]: http://en.wikipedia.org/wiki/Seishiro_Endo "Wikipedia page about Endō Seishirō"

[Dennis Ritchie]: http://en.wikipedia.org/wiki/Dennis_Ritchie "Wikipedia on Dennis Ritchie, may he rest in peace"
[building Snugio]: /posts/2011-09-23-learning-haskell-by-building-snugio.html "Learning Haskell by Building Snugio"
[webmachine]: https://github.com/basho/webmachine "Github page of webmachine from Basho"