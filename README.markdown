Booklet [![Build Status](https://travis-ci.org/digimead-specific/Booklet.png)](https://travis-ci.org/digimead-specific/Booklet)
=======

*Booklet* is a publishing software for short texts, particularly user documentation. It is designed to be easy to write and read on any platform.
*Booklet* is a fork of [Pamflet][pf] by Nathan Hamblen and others. Before use *Booklet* please read [Pamflet documentation][pd]. Most things that work
in [Pamflet][pf] work in *Booklet* too.

Features of Booklet:

* hardcoded content is moved to dynamic [Scalate][sl] templates;
* ability to use markdown inside HTML blocks;
* ability to modify options per block of markdown;
* options are merged from different levels: per documentation, per page, user specific;
* reevaluate environment from the beginning for every transformation. *Changes are applied immediately, which is important for end user while design stage. Realtime parameter isn't a primary one for a such type of text generators. It works like a charm with [compass watch][cw] and interactive development.*

[See Booklet documentation](http://digimead-specific.github.io/Booklet/).

Authors
-------

* Alexey Aksenov
* Nathan Hamblen and others (as Pamflet library)

License
-------

Booklet is licensed to you under the terms of the GNU Lesser General Public License,
version 3.0, a copy of which has been included in the LICENSE file.

Copyright
---------

Copyright © 2012-2013 Alexey B. Aksenov/Ezh. All rights reserved.

[pf]: https://github.com/n8han/pamflet
[pd]: http://pamflet.databinder.net
[sl]: http://scalate.fusesource.org
[cw]: http://compass-style.org/help/tutorials/command-line/
