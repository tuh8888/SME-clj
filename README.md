# SME-clj

tuh8888: I have are several goals for this fork

1. To use the code here to help understand the algorithm described in [1]
2. If this code seems to be a sufficient replacement for the Common Lisp used in [1], use this as well
3. Bring this code up to date with clojure 1.10
4. Make this code compatible with my other analogy work

...

SME-clj is an implementation of the Structure-Mapping Engine [1] in Clojure [3].
It was developed as part of my MSc thesis research [2]. To my knowledge it is
one of the only publicly available open source implementations of SME, besides
the original Common Lisp code (which is hard to find, and harder still to read).

If you intend to use this implementation and/or understand or modify its code,
start by reading the SME algorithm description in [1] if you have not done so.
Then, look at the docstring for sme-clj.core, as well as the heat-water analogy
example.


Dependencies
------------

Only Clojure 1.2.0 and Clojure-contrib 1.2.0 are required. If Leiningen is
available, use `lein deps`.


Usage
-----

Like any other Clojure library. See sme-clj.example.simple-heat-water for
basic analogical matching.


References
----------

   1. Falkenhainer, B., Forbus, K. & Gentner, D. (1989). The structure-mapping
      engine: algorithm and examples. *Artificial Intelligence, 41*, 1-62.

   2. van der Meer, S.A. (2010). *Making meaningful movements*. Unpublished
      master's thesis, Radboud University Nijmegen, The Netherlands.
      (Available on request, see author section)

   3. http://www.clojure.org/


Author
------

Stefan van der Meer - stefanvandermeer@gmail.com

See LICENSE file.
