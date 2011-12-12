--
title: Understanding Haskell's Web Application Interface
keywords: wai, haskell, snugio
tags: haskell, snugio
description: Understanding the fundamentals of Haskell's Web Application Interface allows us to create our own tools for the web.
--

In my previous post called "[Learning Haskell by Building Snugio]" I mentioned
that I was going to port Erlang's [webmachine] -- a tool to build REST API's
-- to Haskell. To start I first needed to understand Haskell's Web Application
Interface ([WAI]) and what better way to check my knowledge than by sharing it
with you.

Just like Python's [WSGI], Ruby's [Rack], Clojure's [Ring], Haskell has
[WAI]. That were too much acronyms in one sentence, but they all stand for the
same, a specification for a web server interface. Such specifications were
needed because each web framework had it's own way to hook up to a web
server. Some frameworks used CGI, other FCGI or Apache with "mod_python",
"mod_ruby" etc. This made it hard to swap parts and create a generic, fast
method of serving up your application. Recently Haskell also created such a
specification which can be attached to a fast webserver called [Warp].

## Web Application Interface (WAI)

WAI's base principle resounds around two components, an application and a
webserver. An application is a piece of software which accepts a request and
returns a response. The webserver ([Warp]) component accepts this response and
server it to the end user. We are interested in the application side because
we already have the [Warp] webserver at our disposal. The following code is a
minimal WAI application that can be server by Warp. Follow along as I explain
each line.

~~~ {.haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance

app :: Application => Request -> Response
app req = return $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
~~~

First five lines can be disregarded for now, they are simple imports to build
the application with. In line 7 we simple state that the function ``app``
takes a ``req`` and returns an ``Application``. 

This is defined as follows:

### Application
but Iteratee is a type constructor which takes three parameters

~~~ {.haskell .numberLines}
type Application = Request -> Iteratee B.ByteString IO Response
~~~

### ResponseLBS
~~~ {.haskell .numberLines}
responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
responseLBS s h = ResponseBuilder s h . fromLazyByteString
~~~ 

### Run
~~~ {.haskell }
run :: Port -> Application -> IO ()
~~~

## Middleware


## Routing


## Onwards

[Learning Haskell by Building Snugio]: /posts/2011-09-23-learning-haskell-by-building-snugio.html "Read this post"
[webmachine]: http://wiki.basho.com/Webmachine.html "Webmachine from Basho"
[WSGI]: http://www.python.org/dev/peps/pep-0333/ "PEP 333 explaining WSGI"
[Rack]: rack.rubyforge.org "Rack's homepage"
[Ring]: https://github.com/mmcgrana/ring "Ring on Github"
[WAI]: http://www.haskell.org/haskellwiki/WebApplicationInterface "Haskell's WIKI on WAI"
[Warp]: http://www.yesodweb.com/blog/2011/01/announcing-warp "Introduction for the Warp webserver"
