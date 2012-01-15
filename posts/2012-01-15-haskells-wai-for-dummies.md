--
title: Haskell's WAI for Dummies
keywords: wai, haskell, snugio
tags: haskell, snugio
description: Understanding the fundamentals of Haskell's Web Application Interface allows us to create our own tools for the web.
--

In my previous post "[Learning Haskell by Building Snugio]" I wrote that I was going to port Erlang's [webmachine] -- a tool to build REST API's -- to Haskell. In my port I'm using Haskell's Web Application Interface ([WAI]) and what you are reading is an easy introduction to it.

[Learning Haskell by Building Snugio]: /posts/2011-09-23-learning-haskell-by-building-snugio.html "Read this post"
[webmachine]: http://wiki.basho.com/Webmachine.html "Webmachine from Basho"
[WAI]: http://www.haskell.org/haskellwiki/WebApplicationInterface "Haskell's WIKI on WAI"
[Warp]: http://www.yesodweb.com/blog/2011/01/announcing-warp "Introduction for the Warp webserver"

Just like Python's [WSGI], Ruby's [Rack], Clojure's [Ring], Haskell has [WAI]. That were too much acronyms in one sentence, but they all stand for the same, _a specification for a webserver interface_. 

People started making these specifications because each web framework came with it's own web server. Some frameworks used CGI, other FCGI or Apache with "mod_python", "mod_ruby" etc. This made it hard to exchange code and create a generic, fast method of serving applications. Haskell is one of the last to have it's own specifications called [WAI] which can be served by a fast webserver called [Warp].

[WSGI]: http://www.python.org/dev/peps/pep-0333/ "PEP 333 explaining WSGI"
[Rack]: rack.rubyforge.org "Rack's homepage"
[Ring]: https://github.com/mmcgrana/ring "Ring on Github"

## Web Application Interface (WAI)

WAI was build to have a generic interface for the [Yesod] web framework. It's base principle resounds around two components, an application and a webserver. An application is a piece of software which accepts a request and returns a response. The webserver ([Warp]) component accepts this response and server it to the end user. The following code is a minimal WAI application that can be server by [Warp].

[Yesod]: http://www.yesodweb.com/ "Yesod framework homepage"

~~~ {.haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance

app :: Application
app req = return $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
~~~

On line seven we can see the function ``app`` which has the following type signature:

~~~ {.haskell}
type Application = Request -> ResourceT IO Response
~~~

This type signature takes a ``Request`` and returns a ``Response``. A ``Request`` and ``Response`` are defined as follows:

~~~ {.haskell .numberLines}
data Request = Request
  { requestMethod :: H.Method
  , httpVersion :: H.HttpVersion
  , rawPathInfo :: B.ByteString
  , rawQueryString :: B.ByteString
  , serverName :: B.ByteString
  , serverPort :: Int
  , requestHeaders :: H.RequestHeaders
  , isSecure :: Bool
  , remoteHost :: SockAddr
  , pathInfo :: [Text]
  , queryString :: H.Query
  }
  deriving (Show, Typeable)

data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseSource H.Status H.ResponseHeaders (C.Source IO Builder)
  deriving Typeable
~~~

``Request`` is a data type containing all the information send by the client. This information is used so the server knows how to respond to the ``Request``. For example, ``rawPathInfo`` could be used to determine which function should be executed for this request and the ``requestHeaders`` could (and should) be used to determine the correct response type, like HTML, JSON or XML. The request is all you get and need for your application to return a correct ``Response``[^1].

[^1]: You can learn more about the ``Request`` and ``Response`` types by looking at the [source code of WAI] on Github.

[source code of WAI]: https://github.com/yesodweb/wai/blob/master/wai/Network/Wai.hs "Source code on Github"

``Response`` is also a data type which is build by one of three functions. A function ``ResponseFile`` which accepts a [HTTP Status], headers and the path to a file. Or a function ``ResponseBuilder`` which also accepts a [HTTP Status], headers and a ``Builder``. The ``Builder`` is a data type from the [Blaze] bytestring library and is a fast and efficient way of building strings, in our case strings which make up the response. The final way to create a response is by calling ``ResponseSource``. This is almost the same as ``ResponseBuilder`` excepts that it makes use of conduits[^2]. The piece of code that accepts the request and returns the response is of the ``Application`` type.

[^2]: An explanation of conduits is in a whole new ballpark. If you are interested, Michael Snoyment from the [Yesod] framework explains them in his blog post "[Conduits]"

[HTTP Status]: http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html "HTTP Status codes on W3.org"
[Blaze]: https://github.com/meiersi/blaze-builder "Blaze Builder on Github"
[Conduits]: http://www.yesodweb.com/blog/2011/12/conduits "Explanation of conduits on Michael Snoyberg's blog"

## Application

When building on [WAI], the application is where your code goes. A mini web framework would at least have a routing engine which looks at the request path and sends it to a handler. An example of a routing engine for [WAI] is "[wai-routes]".

The handler takes care of creating the ``Response``. Mind you that a handler could also be one that returns "404 Page Not Found" if nothing matches for that request path. For example, a mini called framework called [Compojure] written for [Clojure's] web application library [Ring] only makes it easy for you to create routes and handlers, that's it. When you want to build your own mini framework, this is where you start. I'm not going to start there with Snugio because like Basho's [Webmachine introduction] says, it's not a framework, but a toolkit.

> Webmachine is not like the web frameworks you're used to. You can call Webmachine a REST toolkit if you like, and we won't argue with you.

[wai-routes]: https://github.com/ajnsit/wai-routes "wai-routes on Github"
[Compojure]: https://github.com/weavejester/compojure "Compojure on Github"
[Clojure's]: http://clojure.org/ "Clojure's homepage"
[Ring]: https://github.com/mmcgrana/ring "Ring on Github"
[Webmachine introduction]: http://wiki.basho.com/Webmachine.html "Homepage of Webmachine on Basho"

## WAI and Snugio

Web application specifications like [WAI] make it easy to build your own framework without worrying about building a stable web server. I'm using it to build [Snugio], but like I wrote before, I won't start by building a routing engine. For Snugio the routing is only needed to connect a request path to a resource. More importance is put on choosing the correct handler for this resource by going down the [Flow Diagram] created by Alan Dean and Justin Sheehy from Basho. I'm lucky that I can use all the hard work done by smart people as Alan Dean, Justin Sheehy and Michael Snoyman to create an fast and and easy to use REST toolkit. Hopefully someday people will be able to use some of my work to build a fast REST API with [Snugio].

[Snugio]: http://snug.io "Homepage of Snugio"
[Flow Diagram]: http://wiki.basho.com/Webmachine-Diagram.html "Webmachine's Flow Diagram"