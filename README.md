plumtree
========

**NOTE: this is Plum's fork of the original `plumtree` library that has swapped out `eleveldb` and Basho's `leveldb` forks for `lets`. Please read below in the "Plum's Fork" section as there are important performance considerations to be aware of before you use this.**

Plumtree is an implementation of Plumtree[1], the epidemic broadcast protocol. It is extracted from the
implementation in riak_core[2]. Instead of the riak_core ring and riak's ring gossip protocol, it includes a
standalone membership gossip, built around riak_dt[3]'s ORSWOT[4].

More information on the plumtree protocol and it's history we encourage you to watch Jordan West's RICON
West 2013 talk[5] and Joao Leitao & Jordan West's RICON 2014 talk[6].

A special thanks to Jordan, Joao and the team at Basho for providing much of the code contained in this
library.

1. http://www.gsd.inesc-id.pt/~jleitao/pdf/srds07-leitao.pdf
2. https://github.com/basho/riak_core
3. https://github.com/basho/riak_dt
4. http://haslab.uminho.pt/cbm/files/1210.3368v1.pdf
5. https://www.youtube.com/watch?v=s4cCUTPU8GI
6. https://www.youtube.com/watch?v=bo367a6ZAwM


Build
-----

    $ make

Testing
-------

    $ make test
    $ make xref
    $ make dialyzer

Contributing
----

Contributions from the community are encouraged. This project follows the git-flow workflow. If you want to
contribute:

* Fork this repository 
* Make your changes and run the full test suite
 * Please include any additional tests for any additional code added
* Commit your changes and push them to your fork
* Open a pull request

We will review your changes, make appropriate suggestions and/or provide feedback, and merge your changes
when ready.

Plum's Fork
===========

This is Plum's forked version of plumtree from Helim. We forked this specifically because Plumtree --
despite satisfying all of the replicated configuration sharing features our product requires -- has a hard
dependency on Basho's `eleveldb` and `leveldb` (which cannot be separated).

Why is this a problem? Plum's product that plumtree is a component of is a highly-constrained 32bit ARM
system and Basho's leveldb fork is highly optimized for 64bit systems. The changes required to backport
Basho's fork were too high and unknown for me, especially since it currently will segfault the entire Erlang
VM.

What did we change?
-------------------

We moved away from `eleveldb` and adopted `lets` instead. The `lets` library is a generic ets-like interface
backed by LevelDB. The author wrote his own leveldb bindings that depend on Google's vanilla LevelDB which
compiles and runs well on the ARM. The backend is still LevelDB but the interface to it has changed.

Notably, the snapshot feature in `hashtree.erl` abuses LevelDB's iterators, which is also very
performant. `lets` does not have an interface (currently) for LevelDB's iterators so I moved that logic to
`lets:tab2list`. This loads the data-set into memory as a list of key-value pairs and is far less performant
than the other way of doing it.

!!WARNING!!
-----------

If it was not previously clear, the snapshot feature which is a critical part of the Active Anti Entropy
system must load the entire data-set into memory in order to traverse it and compare. Until `lets` has been
modified to provide an interface to leveldb iterators you should not use this for anything that will store a
lot of data (Plum, for example, doesn't - we have at most 50 keys and the values for those keys are
extremely small).
