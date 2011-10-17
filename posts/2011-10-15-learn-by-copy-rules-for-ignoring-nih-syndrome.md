--
title: Learn by Copy, Rules for Ignoring NIH Syndrome
tags: thoughts
keywords: not invented here syndrome, programming, thoughts
--

When telling me you are writing a new library, chances are that the obnoxious me is right by replying "It's been done before, just use that". This is a fact of software development, inherrent to the large amount of [code monkeys], cranking out software since 1950's [machine language programs]. Should it stop you from writing it? No! 

The above describes a "syndrome" called "Not Invented Here" (NIH) which is generally considered a bad thing. In this post I will show the merits of the syndrome when used for learning concepts, frameworks or programming languages.

## Understanding

"Not Invented Here" syndrome describes ignoring existent products, research, knowledge or, in our case, software from third parties and creating it your damn self (and than doing it multiple times, therefore the syndrome). The most heard argument against NIH is _"Why you would want te reinvent the wheel?"_. My reply to this is _"Because I want to understand the wheel!"_.

A few years ago, starting with Django, I decided to build my own blogging software. There were already a dozen libraries doing the exact same thing. But still, I believed this was needed because I wanted to learn the framework and while I could try reading myself to sleep with Django's documentation, finishing a website with it would have me scratching a bold spot within the hour. Contrast this at looking at others people code, while at the meanwhile writing my own. Not only does this enable you to understand the framework at a more fundamental level, it also helps you memorize it's tools more effectively.

This understanding precedes the first rule for ignoring the NIH syndrome. __Ignore it when you are learning.__ May it be a new technique, framework or language, you don't understand it untill you use it. You don't understand it untill you complete the process of idea to working implementation. And to dive in using it, create a new project where you have the guidance of an already finished one.

## Improving

When I was a year into Django we needed a account managing application for [Bread & Pepper]. The ``GOTO`` application was [django-registration]. At this point I was comfortable with [Python] and [Django] and decided to create my own account application called [django-userena]. This has to do with my second rule for ignoring NIH: **You can do it better**.

This reason should be used cautiously because it can hamper the quality of our professions products when your "do it yourself" attitude stems from a minor modification or improvement towards an _open-sourced_ library. It would therefore be wise to contact the author and propose your changes, maybe it's something they wanted to do themself or already tried and then share their reasons for not implementing it. If you can't work it out together, do it yourself.

If there is no possibility for cooperation on the project that you are improving, go ahead and build a better one. This gives you a benefit of learning from the mistakes of the giants before you and again, improving your own skillset.

## Use it for Learning

It's hard to make a dent[^1] in the current software world -- been trying for a few years, but only expect to make my first scratch in the next five -- but let me disclose my [master plan]. The "[Pragmatic Thinking and Learning]" book introduced me to the concept of Shuhari (守破離). Following citation from [Endō Seishirō] describes it best:

> It is known that, when we learn or train in something, we pass through the stages of shu, ha, and ri. These stages are explained as follows. In shu, we repeat the forms and discipline ourselves so that our bodies absorb the forms that our forebearers created. We remain faithful to the forms with no deviation. 

> Next, in the stage of ha, once we have disciplined ourselves to acquire the forms and movements, we make innovations. In this process the forms may be broken and discarded. 

> Finally, in ri, we completely depart from the forms, open the door to creative technique, and arrive in a place where we act in accordance with what our heart/mind desires, unhindered while not overstepping laws. 

I'm willing to argue that the stage of _shu_ was me writing another Django blog engine. Been done before... been done before better. Open-sourced software of great use here, because a lot of programmers don't have access to a mentor which challenges them and shows them best practices. Luckily, in the open-source world there is a steady supply of code available from first-class programmers. Read it, duplicate it and understand it.

_Ha_ was working on [django-userena], knowing the framework and creating innovations, on a miniature scale. Although there may be software that does what you want, a _ha_ programmer is able to improve it.

## A Backboard Philosophy

We came full circle on a backboard philosophy on how to become an expert (Ri) programmer. At first, ignore you are reinventing the wheel, rinse and repeat the practice untill you reach the stage of _ha_ where you are familiar with the language, framework and are able to create software that stands on it's own. This time you are able to supply others with code to learn from. Slowly your knowledge will allow you to break free and reach the stage of a _ri_ programmer[^2]. Being a _ri_ programmer makes it more easy to create your dent in the universe with new innovative software[^3].

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