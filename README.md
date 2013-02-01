# Functional Brodal Queues

Implementation of a purely functional version of binomial queues and Brodal queues in Scala.

Brodal queues are priority queues with worst-case optimal time complexity - O(1) for `findMin`, `insert` and `meld` (merge) and O(log n) for `deleteMin`. The adaptation to a purely functional structure was made according to Brodal and Okasaki in [Optimal Purely Functional Priority Queues (1996)](ftp://ftp.daimi.au.dk/pub/BRICS/BRICS/Reports/RS/96/37/BRICS-RS-96-37.pdf): by incrementally introducing tweaks to binomial queues, a simpler priority queue implementation. The functional versions implemented here maintain the asymptotic bounds of their imperative counterparts.

A presentation about binomial queues and this data structure can be found [here](http://prezi.com/f78h9tvmgouw/binomial-queues/).

## Copyright

Copyright (c) 2013 Rui Gonçalves. See LICENSE for details.