--
title: Understanding Haskell's Web Application Interface
keywords: wai, haskell, snugio
tags: haskell, snugio
description: Understanding the fundamentals of Haskell's Web Application Interface allows us to create our own tools for the web.
--

~~~ {.haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance

app :: Application
app _ = return $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
~~~

Zonder:

~~~
Hallo allemaal
~~~

~~~ {.haskell}
Hallo elaaoeu

aoeu aoeua oeu
~~~
