Booklet [![Build Status](https://travis-ci.org/digimead-specific/Booklet.png)](https://travis-ci.org/digimead-specific/Booklet)
=======

*Booklet* is a publishing software for short texts, particularly user documentation. It is designed to be easy to write and read on any platform.
*Booklet* is a fork of [Pamflet][pf] by Nathan Hamblen and others. Before use *Booklet* please read [Pamflet documentation][pd]. Most things that work
in [Pamflet][pf] work in *Booklet* too.

The differences of Booklet:

* hardcoded template moved on [Scalate][sl];
* ability to use markdown inside HTML blocks;
* options are merged from different levels: per documentation, per page, user specific;
* reevaluate environment from the beginning for every publication. *From the one side this is much slower than caching. From the other one - any changes are applied immediately, which is important for end user while design stage. Also realtime parameter isn't a primary one for a such type of text generators. It works like a charm with [compass watch][cw] and interactive development.*

[See Booklet documentation](http://digimead-specific.github.io/Booklet/).

[pf]: https://github.com/n8han/pamflet
[pd]: http://pamflet.databinder.net
[sl]: http://scalate.fusesource.org
[cw]: http://compass-style.org/help/tutorials/command-line/