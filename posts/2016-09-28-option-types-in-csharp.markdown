---
title: Option types in C#
date: 2016-09-28 20:23:00
comments: true
share: true
categories: functional-programming csharp option optional
---

_Disclaimer: Whereas this blog post is an argument for using option types in your C# code in general, I am of course naturally biased towards my own implementation, [Optional](https://github.com/nlkl/Optional)._

No one likes nulls. No one likes nulls, to the extend that it is almost becoming cliché to mention "Tony Hoare's billion dollar mistake". So, instead of focusing on the complaints (although I will probably be guilty of that anyway), I will try to keep this post as concrete and practical as possible.

In spite of all its downsides, null values are actually solving a very real and very common problem: providing a way to represent the absence of a value (the primary purpose af null values nowadays anyway). The downside, of course, being that null values are everywhere, without any indication in the type system - all values might be null, and if you forget to check it, cataclysm.

An option type is another, more explicit, way to model such absence of value, typically employed by functional programming languages. Although not the only alternative to nulls, option types are simple and easy to model in most statically typed programming languages (including C#), even without overly advanced type level features.

## What is an option type anyway?

An option type is simply a _strongly typed null value_, a type (or class if you will) representing a value that might, or might not, be present. 

In C# terms, you might think of an option type as a more general version of a `Nullable<T>`, that also works for reference types. In this spirit, a poor man's option type could simply be:

```cs
Option<string> option = ...;
return option.HasValue ? option.Value : "Not found";
```

Now, this approach takes us 50% of the way, as we have now explicitly modelled the absence of a value. However, an observant reader might notice that we do not get the safety that one might hope for: if we call `option.Value` without first checking `option.HasValue`, things are still bound to explode sooner or later. 

More formally, we typically say that an option type can be in one of two states: _Some_, representing the presence of a value, and _None_, representing the absence of a value.

By removing the `Value` property, and replacing it with a safer operation, which forces us to always consider the None-scenario, we arrive at a simple and safe option type:

```cs
Option<string> option = ...;
return option.Match(
    value => value,       // In case of Some
    ()    => "Not Found"  // In case of None
);
```

The `Match` operation takes two functions, one for handling the Some-state, where a value is present, and one for handling the None-state - and it is not possible to neglect either of the two potential states. (The name, `Match`, refers to a concept known as pattern matching, but this is of minor practical significance, and is therefore mostly chosen for historical reasons.)

At its core, this is how my own option type implemention, [Optional](https://github.com/nlkl/Optional), is modelled if you strip away all the bells and whistles. The bells and whistles are nice, though, and makes working with option types much less cumbersome than one might initially suspect.

## Avoiding that dreadful NullReferenceException

NullReferenceExceptions are a pain. The only thing more annoying than forgetting to check for null, is to actually check for null. 

Contradictive as it might sound, it is actually not far from truth. Because every single value might be null, systematically checking for nulls is a formidable task. In fact, the overhead is so great, that I have never seen a code base which didn't rely on assumptions on which values might be null and which might not. Needless to say, such assumptions can be wrong, and often are.

Notes: Marking values that are optional, no compiler guaranteed safety (but still better than nothing)

## Be explicit about your domain and your data

## Safe but concise

## Batteries included

## What are the options?

Notes: Optional is functional, imperative option types also exist

