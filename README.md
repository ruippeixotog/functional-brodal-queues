
Functional Brodal Queues
====

Implementation of binomial queues and a functional version of Brodal queues in Scala.

Brodal queues are priority queues with worst-case optimal time complexity - O(1) for `findMin`, `insert` and `meld` (merge) and O(log n) for `deleteMin`. The adaptation to a functional structure was made according to Brodal and Okasaki in [Optimal Purely Functional Priority Queues (1996)](ftp://ftp.daimi.au.dk/pub/BRICS/BRICS/Reports/RS/96/37/BRICS-RS-96-37.pdf), by incrementally introducing tweaks to binomial queues, a simpler priority queue implementation.

A presentation about binomial queues and this data structure can be found [here](http://prezi.com/f78h9tvmgouw/binomial-queues/).
