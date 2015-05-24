---
layout: post
title:  "What I dislike about F#"
date:   2015-05-24 17:38:00
comments: false
categories: fsharp functional-programming
---

Let me start by stressing that I do like the F# language, and this post is not a bashing of F# as such. I think F# has a lot to offer, and I recognize the compromises the language designers had to make, in order to make the .NET integration as smooth as possible. 

However, all programming languages have intrinsic flaws, and this post simply highlights some of the aspects of F# that I find somewhat annoying. As you have already guessed, this post will be highly subjective ;)

# OCaml heritage

F# is both semantically and syntactically inspired by OCaml. Whereas this might make the language more familiar to OCaml developers, I believe that this choice has had a negative impact on the overall language design.

The original F# syntax was closely inspired by that of OCaml. Since then, a "lighter" syntax was introduced (and made default), which removed a lot of the syntactic overhead of the old syntax. Even with the lighter syntax, however, the overall syntactic elements closely resemble those of OCaml, which means that F# is filled with lots of "arbitrary" syntactic decisions.

Particularly, I think that F# could have borrowed a few of the syntactic improvements of Haskell, to make the code a bit easier on the eyes. 

One example of this is tupled type constructors, which produces a lot more noise than Haskell's curried version (I recognize that this is also a semantic difference). Consider the following contrived example:

{% highlight fsharp %}
let formatPhoneNumber (PhoneNumber (prefix, number)) = ...
{% endhighlight %}

And a hypothetical curried version could look like this:

{% highlight fsharp %}
let formatPhoneNumber (PhoneNumber prefix number) = ...
{% endhighlight %}

Notice that the curried version is not only cleaner, but also more flexible, and doesn't rule out that we could use a tupled version if we should so desire.

Even so, I recognize that this isn't going to make all that much of a difference by itself - but there are many small syntactic and semantic improvements that could be made, if the language design hadn't followed OCaml as closely. Other examples include the lack of pattern matching directly in the function definition, the verbose lambda syntax (`fun x -> ...`), no option to define custom operator associativity and precedence, semicolons as list separators (and the tuples-without-parens that causes it), verbose guard syntax, and many more. Each of them minor, but they add up to some level of annoyance.

I am not saying that the syntax should necessarily be identical to that of Haskell either, but simply that the overall syntax could be improved in several places, in my (obviously subjective) view.

Finally, the language currently has a lot of optional and redundant syntax, due to this "ML compatibility" (which I think is complete bogus anyway). The optional syntactic constructs don't bother me much, but it feels unclean that they are there for no better reason.

# Too much OOP

Whereas F#'s OCaml heritage is only a minor annoyance, my primary F# pain point is the amount of OOP interop that is forced onto you.

F# comes with a small functional standard library, providing things such as list-manipulation and the like. As long as this is all you need, the world is all peachy. However, as soon as you are working on a task of non-trivial size, you will need to use functionality from _somewhere else_, as the F# standard library simply doesn't offer too much functionality out-of-the-box.

In practice, this mostly means that you will need to use regular C#-focused .NET libraries, as the current number of F# libraries is still somewhat limited (more on this below). On one hand, the ability to use .NET libraries directly is one of F#'s major selling points, but on the other, this forces you to switch to an imperative coding style way more often than I would like to.

Imperative (and OOP) code in F# is more or less like C# with a lighter syntax. However, having to switch to an imperative style whenever you need to do anything non-trivial defeats much of the purpose of F# altogether (exaggeration alert). To make it even worse, OOP code in F# looks _messy_ compared to the functional equivalent - something that is worsened by the fact that type inference doesn't play well with the OOP parts, requiring large amounts of type annotations.

In response, I more or less always end up writing functional wrappers for all the .NET functionality I need, which works relatively well in most cases, but adds huge amounts of overhead in others, and are completely hopeless in the rest. It would be nice if these functional wrappers were already in place, although I do understand the _scale_ of such an undertaking. Still, in a language like Haskell, more or less all APIs are functional by nature - nice.

F#-specific third-party libraries are better of course, but much less abundant. And even here, OOP code tends to sneak in somewhere. In general, the F# community is much more pragmatic than me when it comes to OOP - but a bit too pragmatic in my opinion. Whenever I have to write OOP code in F# it physically hurts, and I will do what I can do avoid it. Mostly, it just means it will hurt.

It should of course be noted that OOP interop is the price we pay for running on the .NET platform. As with most compromises, it is both a blessing and a curse.

# Lack of functional polymorphism

One point that might force one into developing a partly OOP-based API, is the lack of non-OOP polymorphism.

Apart from parametric polymorphism (aka generics) and a few hack-arounds, F# only supports polymorphism through the use of OOP (subtype polymorphism). In practice, this means that we are forced to use OOP whenever we want to abstract over different types in a slightly sophisticated way.

In simple application code, we can often get quite far using generics and built-in convenience functionality, but once we are developing non-trivial abstractions, this lack of non-OOP polymorphism shows itself rather quickly. Being forced to use OOP because of the limitations of the language is a rather frustrating experience.

Still, I certainly recognize that this is not an easy problem to solve for the language designers, due to the limitations imposed by the .NET interop. Nevertheless, having some sort of functional polymorphism, be it ad-hoc polymorphism/type classes or higher-order modules, would be a nice complement to the language.

# Too litle focus on data structures


