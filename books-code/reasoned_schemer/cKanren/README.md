cKanren
=======

This library implements miniKanren (http://minikanren.org) with an
extensible framework for defining constraints.

How to install
--------------

cKanren can be installed as a collection within Racket (http://racket-lang.org)
as follows:

* `cd cKanren/cKanren`
* `raco link .`
* `raco setup cKanren`

After setup finishes, you will be able to use miniKanren, `#lang
cKanren`, and all constraint libraries that ship with cKanren.

For users
---------

If you are interested in writing miniKanren programs, you can
`(require cKanren/miniKanren)` for standard miniKanren definitions.
You can also require constraint libraries like `neq` as `(require cKanren/neq)`.

For Emacs users
---------------

http://docs.racket-lang.org/guide/other-editors.html

Stable constraint libraries
---------------------------

The following libraries have been tested extensively.

* Tree unification              `cKanren/tree-unify`
* Disequality constraints       `cKanren/neq`
* Absento, symbolo, and numbero `cKanren/absento`

All other constraints libraries are experimental.
