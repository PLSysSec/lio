About
=============

The *Labeled IO* (LIO) library is an information flow control (IFC)
library. IFC is a mechanism that enforces security policies by
tracking and controlling the flow of information within a system.
Different from discretionary access control (think UNIX file
permissions), with IFC you can execute an untrusted computation on
your secret data and be sure that it does not leak it or overwrite
it.

LIO is an IFC library that can be used to implement such untrusted
computations. LIO provides combinators similar to those of 'IO' for
performing side-effecting computations (e.g., accessing the
filesystem, modifying mutable references, throwing exceptions, etc.)
To track and control the flow of information, LIO associates a
security policy, usually called a *label*, with every piece of data.
A label may, for example, impose a restriction on who can observe,
propagate, or modify the data labeled as such.  Different from
standard IO operations, the LIO counterparts usually take an
additional parameter for the label which they inspect before
actually performing the (underlying IO) side-effecting computation.
So, before writing to a file LIO asserts that the write will not
violate any security policies associated with the file or the data
to be written.

Most code should import module `LIO` and whichever label format the
application is using (e.g., `LIO.DCLabel`). All untrusted code
should have type `LIO`, which trusted code can safely execute with
`evalLIO`. See [Hackage](http://hackage.haskell.org/package/lio) for a
description of the core library API.

The papers that describes the core of LIO, including motivation and
formal modeling/proofs, are available here:

*  Deian Stefan.
   _Principled and Practical Web Application Security._
   _Ph.D. Thesis, Stanford University. December, 2015._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/stefan:2015:phdthesis.pdf)

*  Deian Stefan, Alejandro Russo, David Mazieres, and John C. Mitchell.
   _Flexible Dynamic Information Flow Control in the Presence of Exceptions._
   _Journal of Functional Programming, Cambridge University Press. Volume27, 2017._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/stefan:2017:flexible.pdf)

* Daniel B. Giffin, Amit Levy, Deian Stefan, David Terei, David Mazières, John Mitchell, and Alejandro Russo.
  _Hails: Protecting Data Privacy in Untrusted Web Applications._
  _In Proceedings of Symposium on Operating Systems Design and Implementation (OSDI), USENIX. October, 2012._
  [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/giffin:2012:hails.pdf)

*  Stefan Heule, Deian Stefan, Edward Z. Yang, John C. Mitchell, and Alejandro Russo.
   _IFC Inside: Retrofitting Languages with Dynamic Information Flow Control._
   _In Proceedings of Conference on Principles of Security and Trust (POST), Springer. April, 2015._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/heule:2015:ifc-inside.pdf)

*  Deian Stefan, Alejandro Russo, Pablo Buiras, Amit Levy, John C. Mitchell, David Mazieres.
   _Addressing Covert Termination and Timing Channels in Concurrent Information Flow Systems._
   _In Proceedings of The 17th ACM SIGPLAN International Conference on Functional Programming (ICFP), ACM, 2012._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/stefan:2012:addressing.pdf)

*  Deian Stefan, Alejandro Russo, John C. Mitchell, and David Mazieres.
   _Flexible Dynamic Information Flow Control in Haskell._
   _In Proceedings of Haskell Symposium , ACM SIGPLAN . September 2011._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/stefan:2011:flexible.pdf)

*  Deian Stefan, Alejandro Russo, David Mazieres, and John C. Mitchell.
   _Disjunction Category Labels._
   _In Proceedings of 16th Nordic Conference on Security IT Systems, NordSec , Springer LNCS. October 2011._
   [PDF](https://cseweb.ucsd.edu/~dstefan/pubs/stefan:2011:dclabels.pdf)


Structure of repository
=============

This repository is divided into three packages:

- `lio`: The main LIO library

- `lio-fs`: Simple IFC filesystem library

- `quickcheck-lio-instances`: QuickCheck2 instances for LIO

- `lio-eval`: Tests and benchmarks. This is a package solely because it
  depends on both `lio` and `quickcheck-lio-instances`. Having the
  tests and benchmarks in `lio` leads to a circular dependency.

- `lio-http-server`: Simple HTTP server implementation. Forked into its own
  project [Frankie](https://github.com/plsyssec/frankie/).

- `lio-simple`: Slightly older simple HTTP server implementation atop
  [Simple](https://hackage.haskell.org/package/simple).

Acknowledgements
=============

We thank  Catalin Hritcu, Benjamin Pierce, and Jeremy Planul for
insightful comments on both the design and implementation of LIO. This
work was funded by the DARPA Clean-Slate Design of Resilient,
Adaptive, Secure Hosts (CRASH) program, BAA-10-70.  Deian Stefan was
funded by The National Defense Science and Engineering Graduate
(NDSEG) Fellowship while working on LIO.
