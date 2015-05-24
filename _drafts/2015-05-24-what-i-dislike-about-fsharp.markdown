---
layout: post
title:  "What I dislike about F#"
date:   2015-05-24 17:38:00
comments: false
categories: fsharp functional-programming
---

Let me start by stressing that I do like the F# language, and this post is not a bashing of F# as such. I think F# has a lot to offer, and I recognize the compromises the language designers had to make, in order to make the .NET integration as smooth as possible. 

However, all programming languages have flaws, and this post simply highlights some of the aspects of F# that I find somewhat annoying. Note that this post is highly subjective ;)

# OCaml heritage

F# is both semantically and syntactically inspired by OCaml. Whereas this might make the language more familiar to OCaml developers, I believe that this choice has had a negative impact on the overall language design.

The original F# syntax was closely inspired by that of OCaml. Since then, a "lighter" syntax was introduced (and made default), which removed a lot of the syntactic overhead of the old syntax. Even with the lighter syntax, however, the overall syntactic elements closely resemble that of OCaml, which means that F# is filled with lots of "arbitrary" syntactic decisions.

Particularly, I think that F# could have borrowed a few of the syntactical improvements of Haskell, to make the code a bit easier on the eyes. 

One example of this, is tupled type constructors, which produces a lot more noise than Haskell's curried version (I recognize this is also a semantic difference). Consider the following contrived example:

{% highlight fsharp %}
let formatPhoneNumber (PhoneNumber (prefix, number)) = ...
{% endhighlight %}

And a hypothetical curried version could look like this:

{% highlight fsharp %}
let formatPhoneNumber (PhoneNumber prefix number) = ...
{% endhighlight %}

Notice that the curried version is not only cleaner, but also more flexible, and doesn't rule out that we could use a tupled version if we should so desire.

Even so, I recognize that this isn't going to make all that much of a difference by itself - but there are many small syntactic and semantic improvements that could be made, if the language design hadn't followed OCaml as closely. Other examples include lack of pattern matching directly in the function definition, the verbose lambda syntax (`fun x -> ...`), no option to define operator associativity and precedence, semicolons as list separators (and the tuples-without-parens that causes it), verbose guard syntax, and many more. Each of them minor, but they add up to some level of annoyance.

I am not saying that the syntax should necessarily be identical to that of Haskell either, but simply that the overall syntax could be improved in several places, in my (obviously subjective) view.

Finally, the language currently has a lot of optional and redundant syntax, due to this "ML compatibility" (which I think is complete bogus anyway). The optional syntactic constructs don't bother me much, but it feels unclean that they are there for no better reason.

# Too much OOP and .NET

Whereas the OCaml heritage is only a minor painpoint, this is much more severe

# Lack of functional polymorphism

# Too litle focus on data structures


