---
title: Currying and Partial Application
description: A explanation of currying and it's use in partial function application.
tags: currying, haskell
keywords: currying, haskell, partial application, prefix, infix, curry
---

A few days ago I wrote a post with the same subject as this one. I was proud of the result and decided to post it on [Reddit] so others could learn from my thorough understanding of the matter. Boy, was I wrong. Turns out that very little in that post actually had anything to to with currying.

[Reddit]: http://www.reddit.com/r/haskell/comments/kxdh7/curry_and_its_partial_application/

Luckily, the Haskell community is one of teachers and I received valuable feedback about the correct meaning of currying. Although I needed some time to recover from my initial failure, it would be foolish to leave it at that. So, with all the feedback printed out and annotated, here is my second try in explaining _currying_ and _partial function_ application.

## What is currying?

Currying is a technique which transforms a function that takes a tuple into a chained function which each take only one parameter. Let's start with two functions, ``foo`` being the uncurried version of ``bar``.

~~~ {.haskell}
foo :: (Int, Int) -> Int
foo (a, b) = a * b

bar :: Int -> Int -> Int
bar a = \b -> a * b
~~~

The reason we call ``foo`` uncurried, is because it takes a single tuple as argument and returns the value. This is also shown because of one ``->`` in the type declaration. One the left side of the arrow we see a single argument, consisting of multiple components.

``bar`` is the curried form of ``foo`` because it takes the first element of the pair in ``foo`` and returns a function which takes the second element of the pair, and finally gives you back the same value that you would get using ``foo``. The function ``bar`` is thus transformed into a function which accepts a single parameter and returns a function which again, accepts a single parameter.

Although it may seem that the following function is an uncurried function, this is not the case because it's only different in syntax from the function ``bar`` above:

~~~ {.haskell}
baz :: Int -> Int -> Int
baz a b = a * b
~~~

First clue is found in the type declaration. You can see it has the same type declaration as ``bar``. But not only is the type declaration the same, follow along to see that they both return the same values:

~~~ {.haskell} 
-- Our new function
baz :: Int -> Int -> Int
baz a b = a * b

-- The same function, but this time the y
-- is replaced by a anonymous function
bar a = \b -> a * b

-- So, if we supply arguments to the function.
-- Having an x of 2.
bar 2 = \y -> 2 + y

-- And an y of 3.
-- Returns a value of 5
bar 2 = (\y -> 2 + y) 3
~~~

Let's try the same with a function that multiplies three numbers, thus allowing us to create three chained functions, all accepting a single parameter.

~~~ {.haskell}
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
~~~

We could also write the _function type_ of multThree as follows:

~~~ {.haskell}
multThree :: Int -> (Int -> (Int -> Int))
~~~

You should read this as a function that takes an ``Int`` and returns a function which itself takes an ``Int``, which again returns a function which takes an ``Int`` and finally returns just an ``Int``. Quite a mouthful, but let's follow this logic in code to clarify that it's gives the same result as the following chained function.

~~~ {.haskell}
-- The chained version of multTree
multThree' x = \y -> \z -> x * y * z

-- Let's replace the x with the value 5
multThree' 5 = \y -> \z -> 5 * y * z
~~~

This shows that ``multThree' 5`` returns a function which accepts a single parameter and returns a function which multiplies this parameters with 5. One level deeper, also supplying the second argument ``y``:

~~~ {.haskell}
multThree' 5 = (\y -> (\z -> 5 * y * z)) 7 
~~~

Returns a function which accepts one argument, and returns that argument multiplied 35 times. Supplying all three arguments finally returns a value:

~~~ {.haskell}
multThree' 5 = (\y -> (\z -> 5 * y * z) 11) 7
~~~

I hope it's clear that although the functions are both syntacticly different, they are the same and return the same results when all arguments are supplied. Although functions in Haskell seem to accept multiple arguments, **all** functions are considered curried and thus **accept a single parameter**. This makes it possible to use partial function applications.

## Partial function application

As we have seen, supplying less arguments to a function that takes multiple -- at leasts seems to -- arguments returns a function which takes as many parameters as we have left out. This is called, _partially applied_ functions. Let's show an example where we create a function which returns the sum of two parameters and then create a new function that always adds seven to it.

~~~ {.haskell}
sumTwo x y = x + y
addSeven = sumTwo 7
~~~

Notice that the ``addSeven`` function doesn't seem to require any parameters. This is a false assumption however when you consider what the partial application of ``sumTwo`` returns.

~~~ {.haskell}
addSeven = sumTwo 7
addSeven = \y -> 7 + y
~~~

Because only one parameter is supplied to the ``sumTwo`` function, another function is returned which requires one more parameter. The above shows the ``addSeven`` expression is a function that takes one parameter and adds seven to it. To take it even further, this allows you to leave out parameters when your functions supplies the parameters to an inner function, for example:

~~~ {.haskell}
sum :: (Num a) => [a] -> a
sum = foldl (+) 0
~~~

Again, the above seems to indicate that ``sum`` function doesn't take any parameters. But a closer look reveals that the ``foldl`` function is partially applied because it requires three parameters to return a value and it only receives two. This also reveals the following general rule.

~~~ {.haskell}
foo a = bar 2 a
-- is the same as..
foo = bar 2
~~~

Partial function application allows you to re-use functions and chain them together. For example, in [Real World Haskell] the example is given of a function called ``isInAny`` which checks if a list can be found in another list. The function starts as such:

[Real World Haskell]: http://www.amazon.com/dp/0596514980/?tag=wunki-20

~~~ {.haskell}
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
~~~

[Real World Haskell]: http://www.amazon.com/dp/0596514980/?tag=wunki-20

<section class="information">

### Prefix or Infix

Prefix or infix are ways to supply your functions with arguments. Prefix functions begin with the function name, followed by it's arguments while infix functions are surrounded by their arguments. An example of an infix function is the ``+``. Haskell allows you to change prefix functions which take two arguments to infix by surrounding them with `` ` ``. </section>
The ``any`` function checks if at least one item in the list ``haystack`` fullfils the condition which is supplied with ``inSequence`` check. Definition of ``inSequence`` is supplied in the ``where`` clausule and uses the _infix_ function ``isInfixOf``. This is a function which accepts two lists and returns ``True`` if the first list is, wholly and intact, contained anywhere in the second list. Thus checking if the list supplied with ``needle`` is part of the list ``haystack``. We could shorten this function by using a lambda function instead of the ``where`` clausule:

~~~ {.haskell}
isInAny needle haystack = any (\s -> needle `isInfixOf` s) haystack
~~~

Hereby the ``isInfixOf`` is supplied with the ``haystack`` through the ``s`` parameter. But.. since we know how partially applied functions work, we could shorten this function even further:

~~~ {.haskell}
isInAny needle haystack = any (isInfixOf needle) haystack
~~~

Because ``(isInfixOf needle)`` is a partially applied function, it returns a function which accepts the remaining arguments. The one remaining argument for this function is thus supplied with ``haystack``.

## Conclusion

So far my second try at currying.. While I continue learning Haskell I will be writing more posts as the above, revisit the site if you want to read more. Also, if you like to read about my first project in Haskell, check out [Learning Haskell by Building Snug.io] or follow me on Twitter at [@wunki].

[Learning Haskell by Building Snug.io]: /posts/2011-09-23-learning-haskell-by-building-snugio.html
[@wunki]: http://twitter.com/#!/wunki