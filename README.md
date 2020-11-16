# nocell

A language for spreadsheets

![Tests](https://github.com/alan-turing-institute/nocell/workflows/Run%20tests/badge.svg)



## Design

- [See the wiki for plan and design](https://github.com/alan-turing-institute/nocell/wiki)
- [The documentation lives here](https://alan-turing-institute.github.io/nocell/index.html)

## How we work together

### Branches

* `main` - is demo-able
* `develop` - the tests pass
* `feature/xy` - Feature branches
* Pull requests - Mandatory, if you change an interface; Optional, if you want a review.

### Style 

* Explcitly provide rather than `all-defined-out`
* Use `contract-out` on provides
* No owners, but let people know what you are working on (eg, Slack or grab an issue)

### Documentation

**Please think about and edit this section**

In the new world of remote-working, it seems likely that documentation will be
more valuable. I (James G) intend to try to write more "planning-type"
documentation -- ie, design principles, architecture, and so on -- rather than
just diving into code. I propose to try to make this documentation precise but
pithy.

There's some trade-off here that I don't really know how to manage. Typically
one needs to try out ideas in code, and those ideas will change. But on the
other hand, writing things in advance is a way to help one think, which is
important since we don't meet up in person with a whiteboard any more. And if
one writes code too early, the design tends to ossify.

I think perhaps the implication is that one needs to be slightly less ambitious
(end-to-end) and more about building stuff that does a small thing, but
works. That fits quite well with this project since the end-to-end prototype has
already been worked out.

