--
title: Learning Haskell by Building Snug.io
description: How I'm going to learn Haskell by copying webmachine to Haskell and calling it snug.io.
tags: haskell, snugio
keywords: haskell, snug.io, webmachine, rest, api
--

Summer of 2009, I was sitting on the beach of Bol in Croatia, with [Real World Haskell] book and a Moleskine. I was trying to learn this mysterious language called Haskell, engulfed in the material and trying finish the exercises at the end of every chapter. The language was challenging but still remember the satisfaction when a new part of it *clicked*. Back at home I discontinued learning Haskell because projects at [Bread & Pepper] required me to be productive and this meant going back to Python.

[Real World Haskell]: http://www.amazon.com/dp/0596514980/?tag=wunki-20 

The memories of this summer came back when I finished a REST API for [Episto][^1] in Clojure. My first project in a functional programming language and it felt like coming home. I think this is because I feel like I'm trying to copy a small part of the universe while programming. My attraction to Mathematics -- BBC does a great job of showing it's beauty in "[The Code]" documentary series -- makes the functional thought process more natural to me than thinking in objects.

[The Code]: http://www.bbc.co.uk/tv/features/code/

This rekindled my interest in Haskell and I decided to order [Learn You a Haskell], as it's mentioned as an gentler introduction to the language compared to [Real World Haskell]. From past experience I know that knowing a programming language can only be achieved by finishing a project which puts the theory in books into code on screen. This would be an ambitious project and it all started when I bought the domain [Snug.io][^2].

[^1]: [Episto] was made during a Hackaton at [Bread & Pepper]. We wanted to build an app where you could send an open letter to any Twitter user. You can read more about it [Episto]. We open-sourced the API, which was written in Clojure at [Github].

[Episto]: http://www.epis.to
[Github]: https://github.com/wunki/episto-api
[Bread & Pepper]: http://breadandpepper.com

[^2]: If I don't finish this project, I just wasted another € 69,-. The Indian Ocean knows how to turn a profit! 

[Learn You a Haskell]: http://www.amazon.com/dp/1593272839/?tag=wunki-20

## What's Snug.io?

In short, [Snug.io] is a port of [webmachine] in Haskell. I was convinced of the brilliance of webmachine when I saw the [Webmachine - Focus on Resources] talk. It matched my idea of what current web development is about. Namely creating an API first and connect to that API with a front-end later. This front-end could be written with the help of frameworks as [Backbone.js] and [Batman.js]. This allows you to keep the input and output of data in one container (the API) and create different visualizations for the data, be it web, iOS or Android with the help of it's native tools.

[Snug.io]: http://www.snug.io
[webmachine]: http://webmachine.basho.com/
[Webmachine - Focus on Resources]: http://vimeo.com/20784244
[Backbone.js]: http://documentcloud.github.com/backbone/
[Batman.js]: http://batmanjs.org/

## But why in Haskell?

First, writing [Episto] made me experience the benefits of functional programming. Although I had [read about them], finishing a project made there meaning tangible. Although I doubt I will need it, I also like that Haskell can be made to be [very fast]. It confirms my manliness in a geeky way, some guys dream of fast cars, I dream of compiled code.

I also like the challenge, I already failed once in learning Haskell, I don't want to fail twice. It's going to be hard to finish this project in Haskell, but I'm already to comfortable in the world of Python. I also know it can be done by looking at other successful Haskell projects like [Warp].

Finally, I have chatted with some people from the Haskell community and they have been most helpful. I'm sure that when I'm stuck, some good samaritan will get me up and running again.

[very fast]: http://shootout.alioth.debian.org/u64q/benchmark.php?test=all&lang=all "Language Shootout"
[read about them]: http://book.realworldhaskell.org/read/why-functional-programming-why-haskell.html
[Warp]: http://www.yesodweb.com/blog/2011/02/warp-speed-ahead

## The How

First thing on my list is finishing [Learn You a Haskell]. This supply the basic knowledge of Haskell needed to understand the code in [Snap], [Warp] and [Yesod], projects that should give me a steady supply of nuggets to use in [Snug.io]. I'll start by writing the routing layer for the [Warp] web-server. The routing layer should look at the incoming URI and parse it. According to the parsed information, the server should dispatch the request to the correct resource and function. You can find a better explanation by reading the [URI Dispatch configuration] of webmachine.

[Snap]: http://snapframework.com/
[Yesod]: http://www.yesodweb.com/
[URI Dispatch configuration]: https://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration

After routing it's time to write the [decision core], simply explained as a request coming in and a response being generated by looking at that request's headers and running it through the [HTTP Flowchart].

[decision core]: https://bitbucket.org/justin/webmachine/wiki/WebmachineMechanics
[HTTP Flowchart]: https://bitbucket.org/justin/webmachine/wiki/BigHTTPGraph

I don't know yet what the next hurdle to tackle is, but after writing the routing and decision core I expect to have gained enough insight to know by then.

## Conclusion

I don't think writing [Snug.io] will be easy, and it will take some time. But the process should be fun and leave us with a tool for creating a REST API which is fast, secure and maintainable. 

> "And available in stores near you!"

I will write about the process, be it good or bad, so do come back to read about it if your are interested in HTTP, REST or Haskell[^3]. Also, for those experienced in Haskell, please follow along on Github and correct me when I blunder.

[^3]: How couldn't you be interested, this sentence alone contains three buzzwords.

