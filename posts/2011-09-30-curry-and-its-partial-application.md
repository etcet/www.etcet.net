--
title: Curry and it's Partial Application
tags: haskell, currying
keywords: haskell, currying, partial function application, lambda, anonymous functions, learn you a haskell for great good
--

I'm reading through the excellent [Learn You a Haskell for Great Good] book and arrived at the explanation of currying. After reading it carefully, I wasn't sure that I got it. Turned out, I didn't. At least not untill I finished writing the following post, trying to explain it to you. I'll start with a part of the theory behind currying, followed by using this theory in _partial function_ applications.

[Learn You a Haskell for Great Good]: http://www.amazon.com/dp/1593272839/?tag=wunki-20

## Functions with Multiple Arguments
<section class="information">

### Parameters or Arguments?

Tomato, tomato? Not really, there is a difference between parameters and arguments. Follow this simple rule: **Parameters** are defined in the function's definition and **arguments** are the values which are passed on function calls. </section>
Curried functions are functions that only take one parameter. However, this doesn't stop languages with it from having functions with multiple parameters. Every time a function with multiple parameters is called, that function is transformed to chained, single parameter functions. Let me show you with a simple function that returns the sum of two numbers.

~~~ {.haskell}
sumTwo :: Int -> Int -> Int
sumTwo x y = x + y
~~~

The first line is the functions _type definition_ which tells us that the function takes two arguments of the type ``Int`` and returns an ``Int``. As mentioned, functions with multiple parameters are curried and the following type definition is therefore more accurate:

~~~ {.haskell}
sumTwo :: Int -> (Int -> Int)
~~~

The above function definition tells us that ``sumTwo`` takes a ``Int`` and returns a function which takes an ``Int`` and returns an ``Int``. Before continuing, I need to tell you a small bit about lambda's.

### Lambda's

Lambda's are anonymous functions -- You won't find these on [Facebook] -- and are thus unnamed, read, not bound to a variable. In Haskell you can define a lambda with the ``\`` character. This character was chosen because it looks like the Greek lowercase letter for lambda: λ. This means that the ``sumTwo`` function could we written as follows:

~~~ {.haskell}
-- sumTwo as lambda
\x y -> x + y

-- You could also test this function in GHCI by supplying it
-- with values. This would return 12.
(\x y -> x + y) 5 7
~~~

[Facebook]: http://adrianshort.co.uk/2011/09/25/its-the-end-of-the-web-as-we-know-it/

Important to note is that you supply arguments to a anonymous function by seperating them by spaces after the closing parenthesis. In the above function, the ``x`` get's a value of 5 and ``y`` of 7. Also do not confuse the above with a _function type_ because it contains an arrow. This is the same function as ``sumTwo``, but now in anonymous form. This knowledge about anonymous functions is enough to follow along on the transformation of multiple parameter functions.

## The transformation

So, we know that multiple parameters are transformed and the way anonymous functions are represented. This is enough to show such a transformation.

~~~ {.haskell}
sumTwo :: Int -> Int -> Int
sumTwo x y = x + y

-- This get's curried to the following function.
-- Where for the y parameter, a λ is created.
sumTwo x = \y -> x + y

-- So, if we supply arguments to the function.
-- Having an x of 2.
sumTwo 2 = \y -> 2 + y

-- And an y of 3.
sumTwo 2 = (\y -> 2 + y) 3
~~~

I hope you could follow along with the transformation above, just remember that the following functions are all identical.

~~~ {.haskell}
sumTwo x y = x + y

-- Add one anonymous function
sumTwo = \x y -> x + y

-- Two anonymous functions.
sumTwo = \x -> \y -> x + y
~~~

Let's do the same with a function that multiplies three numbers, thus taking three parameters.

~~~ {.haskell}
multThree :: Int -> Int -> Int ->
multThree x y z = x * y * z
~~~

We could also write the function type of multThree as follows:

~~~ {.haskell}
multThree :: Int -> (Int -> (Int -> Int))
~~~

You can read this as a function that takes an ``Int`` and returns a function which itself takes an ``Int`` which again returns a function which takes an ``Int`` and finally returns just an ``Int``. Quite a mouthful, but follow along in the code to clarify.

~~~ {.haskell}
multThree x y z = x * y * z

-- In it's curried form is.
multThree x = \y -> \z -> x * y * z

-- The first factory is the one that takes the first
-- argument of x. Let's give it x of 5
multThree 5 = \y -> \z -> 5 * y * z

-- An y of 7
multThree 5 = (\y -> \z -> 5 * y * z) 7
multThree 5 7 = \z -> 5 * 7 * z

-- And finally a z of 11
multThree 5 = (\y -> (\z -> 5 * y * z) 11) 7
multThree 5 7 = (\z -> 5 * 7 * z) 11
multThree 5 7 11 = 5 * 7 * 11
~~~

The above actually states that the ``multThree 5`` expression is is a function which takes one parameter, and returns a function which takes one parameter. By removing one λ, you could write it as such:

~~~ {.haskell}
multThree 5 y = \z -> 5 * y * z
~~~

And the expression ``multThree 5 7 y`` is a function which takes one ``Int`` and returns that in multiplied 35 (5 x 7) times.

~~~ {.haskell}
multThree 5 7 y = 5 * 7 * y
~~~

I hope that at this point you understand what currying is because we are now going to put it to use.

## Partial Application of Functions

In Haskell it's perfectly valid to supply a function with too few parameters. As we have seen, this just returns a function which takes as many parameters as we have left out. This is called, _partially applied_ functions. Let's show an example where we create a function which returns the sum of two parameters and then create a new function that always adds seven to it.

~~~ {.haskell}
sumTwo x y = x + y
addSeven = sumTwo 7
~~~

Notice that the ``addSeven`` function doesn't seem to require any parameters. This is a false assumption however when you consider what the partial application of ``sumTwo`` returns.

~~~ {.haskell}
addSeven = sumTwo 7
addSeven = \y -> 7 + y
~~~

Because only one parameter is supplied to the ``sumTwo`` function, the return is a function which requires another parameter. The above shows the ``addSeven`` expression is a function that takes one parameter and adds seven to it. Thus, the power of partial application.

To take it even further, this allows you to leave out parameters when your functions supplies the parameters to an inner function, for example:

~~~ {.haskell}
sum :: (Num a) => [a] -> a
sum = foldl (+) 0
~~~

Again, the above seems to indicate that ``sum`` function doesn't take any parameters. But a closer look reveals that the ``foldl`` function is partially applied because it requires three parameters to return a value and it only receives two. This reveals the following general rule.

~~~ {.haskell}
foo a = bar 2 a
-- is the same as..
foo = bar 2
~~~

## Conclusion

Well, I hope you have enjoyed our little lesson of curried functions and partial applications. I'm going the continue on learning about Haskell, revisit the site if you want to read more posts like the above. Also, if you like to read about my first project in Haskell, check out [Learning Haskell by Building Snug.io] or follow me on Twitter at [@wunki].

[Learning Haskell by Building Snug.io]: /posts/2011-09-23-learning-haskell-by-building-snugio.html
[@wunki]: http://twitter.com/#!/wunki


