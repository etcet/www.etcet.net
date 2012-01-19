--
title: Foretelling Future Development
keywords: clojure, objective-c, stanford, sicp, A.I, development, trajectory
tags: clojure, objective-c, artificial-intelligence, sicp
description: Done building non trivial projects, in this post I describe the steps I'm going to take to be able to build beautiful software that does difficult things. I can already tell you that it involves Objective-C, Clojure, Artificial Intelligence and S.I.C.P.
--

I started running a few weeks ago and I hated it. I would start cursing during running -- stupid thing to do, since I need the oxygen -- while hoping that with time it would be better. Within four weeks, I'm cursing less and maybe, eventually, I will like it. If not the running, at least the shower, pajama's and book afterwards.

The running sounded like a good analogy for what I want to tell in this post. It barrels down to stepping out of the comfort zone, invest in knowledge and reap the benefits. Years ago, I went from PHP to Python, now to Objective-C and Clojure. The difference with running is that I don't expect to curse about that which follows. I expect the occasional outburst of "Dammit, I could be building a random-pirate-jokalizer in Python!"[^1].

But I don't want Python to be the last pen which writes my bits and bytes. I need a different tool for a different use. A need to evolve and tackle problems, not build sole entertainment or Facebook seconds. This got me thinking about what I should be doing with my time and I can already tell you that it involves the strange combination of Clojure, Objective-C, Artificial Intelligence and S.I.C.P.

[^1]: Don't get me wrong, I love Python and plan to keep using it. I do think other languages are better suited for the things I describe in this post. And also, Pirate jokes are awesome. Don't believe me? Send me a PM on IRC and I will proof you wrong.

## Objective-C and Clojure

The best software is software that let's you do difficult things through a
simple interface[^2]. An example of this is [Dropbox] and
[Lion's Filevault]. For creating beautiful interfaces, I believe Apple's
[Cocoa] framework is miles ahead of the competition (I'm looking at you HTML5
and Android SDK). Uniformity, smooth effects (those with function) and the
overall quality of the Cocoa library make it a competition which is judged by
the eyes. Programming with the Cocoa framework requires the knowledge of the
[Objective-C] language. That's also the sole reason I learned the Objective-C
language with the excellent "[Programming iOS 4]" book.

I also think that software that's connected to the internet will become even
more proliferate than it is now. More sharing of data between users,
synchronization of devices or outsourcing computations to servers. This
requires communication with servers, communication guided by the [REST]
architecture. This is where Clojure comes into play.

Clojure is the thinking part of the software stack. The thinking could happen
in any language, but I see the benefits of a functional language as
Clojure. To drop a few on you: high concurrency, pure functions and meta
programming.[^3] There isn't any line in the Clojure codebase that is
responsible for a visual representation of the data, we have Cocoa running on
a separate device for that. By keeping Clojure responsible for the data and
the computation it performs on it, we can operate on bigger scale by spreading
the load on the developers home turf, namely servers. Thus the user doesn't
need a fast device to operate on difficult problems we as developers can solve
for them. A good stack for solving interesting problems, but which problems?

[^2]: "Simple interfaces, spicy code", that's our motto at [Bread & Pepper]!

[^3]: I would love to make this post even longer and try to convince you about
the advantages of functional programming, but much more seasoned programmers
than me can do a better job at that. If your interests are peaked, view
[this presentation] from Rich Hickey, the creator of Clojure.

## Knowledge is the catalyst of ideas

I couldn't help but to feel inspired by the fifteen minute talk that
[Steve Yegge] gave at OSCON '11 called
"[What Would you do With Your own Google]". In it he rallies his fellow
developers to stop creating the next Facebook or website's showing dancing
kittens. To show he's serious, he resigns himself from the [Google+] project
and start's learning about mathematics, data mining and statistics so he can
tackle the harder challenges that are still there. This inspired me because
I'm guilty of the accused. Besides my final thesis -- exploring terrorism on
the web through automated sentiment analysis -- I have only solved non-trivial
problems with my programming knowledge. I'm not flogging myself about it,
because dancing kittens still pay the bill, but I do want to see growth in the
difficulty and social responsibility that my knowledge brings. To get
inspiration about what to build and how to do it, I selected two courses which
I think will help me.

October 10th is the start of Stanford's course called
"[Introduction to Artificial Intelligence]". A free course given by
[Sebastion Thrun] and [Peter Norvig], writers of the
"[Artificial Intelligence, A Modern Approach]" book. An experiment which gives
you the possibility to follow along with Stanford students, do the homework
and assignments and be graded. My, and your, opportunity to learn more about
statistics and get awed by the possibilities of artificial intelligence.

Finished with the A.I. course I want to chisel away at the next challenge:
learn about the fundaments of computing. For this I have chosen the famous
"[Structure and Interpretation of Computer Programs]" book. It's been on my
bucket list for a while, but only recently have I been able to focus enough to
follow through with this kind of discipline requiring tasks. I don't know what
I will learn from this book, but I'm certain that it will make me a better
programmer.

## Conclusion

There you have it, the tools used in my life as a developer for the coming
months or years. Creating simple and beautiful -- "weniger aber besser"[^4] --
software which solves hard problems. I will ease down on development of things
within my comfort zone, things throwing only the occasional challenging
task. It's time to move on to bigger challenges which require more. I'm
confident that in the coming years I will be able to show you something
beautiful. I'm scarred enough to know that this ideal is there to be
compromised, but wise enough to keep trying to reach it. If you want to know
how I'm doing, please follow me on Twitter ([@wunki]) or this blog.

[^4]: Famous quote from [Dieter Rams] which get's called at least once a day by my good friend and colleague [Wouter de Bres].

[Bread & Pepper]: http://www.breadandpepper.com "Homepage of Bread & Pepper"
[Dropbox]: http://www.dropbox.com "Dropbox's Homepage"
[Lion's Filevault]: http://reviews.cnet.com/8301-13727_7-20081045-263/about-filevault-2-in-os-x-10.7-lion/ "CNET explains Filevault 2"
[Cocoa]: https://secure.wikimedia.org/wikipedia/en/wiki/Cocoa_%28API%29 "Wikipedia article of Cocoa"
[Objective-C]: http://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/ObjectiveC/Introduction/introObjectiveC.html "Introduction to Objective-C by Apple"
[Programming iOS 4]: http://www.amazon.com/dp/1449388434/?tag=wunki-20 "Programming iOS 4 on Amazon."
[Steve Yegge]: http://steve-yegge.blogspot.com/ "Steve Yegge's Blog"
[REST]: https://secure.wikimedia.org/wikipedia/en/wiki/Representational_State_Transfer "Wikipedia page explaining REST"
[this presentation]:
http://wiki.jvmlangsummit.com/images/a/ab/HickeyJVMSummit2009.pdf "PDF Slides of Rich Hickey's talk about Clojure"
[What would you do with your own Google]: http://www.youtube.com/watch?v=vKmQW_Nkfk8 "Video of the presentation at Youtube"
[Google+]: https://plus.google.com/ "Google+ homepage"
[Introduction to Artificial Intelligence]: http://www.ai-class.com/ "Homepage of the free course"
[Sebastion Thrun]: https://secure.wikimedia.org/wikipedia/en/wiki/Sebastian_Thrun "Wikipedia article about Sebastion Thrun"
[Peter Norvig]: https://secure.wikimedia.org/wikipedia/en/wiki/Peter_Norvig "Wikipedia article about Peter Norvig"
[Artificial Intelligence, A Modern Approach]: http://www.amazon.com/dp/0136042597/?tag=wunki-20 "Amazon page of the third edition"
[Structure and Interpretation of Computer Programs]: http://www.amazon.com/dp/0070004846/?tag=wunki-20 "Amazon page of the second edition"
[follow me]: http://twitter.com/#!/wunki_ "My Twitter account" 
[@wunki]: http://twitter.com/#!/wunki "My Twitter account"
[Dieter Rams]: https://secure.wikimedia.org/wikipedia/en/wiki/Dieter_Rams "Wikipedia article about Dieter Rams"
[Wouter de Bres]: http://wdeb.nl "Homepage of Wouter de Bres"
[Twitter]: http://twitter.com/#!/wunki_ "My Twitter account"
