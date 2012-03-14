# LIO: Haskell Information Flow Control Library #

The /Labeled IO/ (LIO) library provides information flow control for
incorporating untrusted code within Haskell applications.  Most code
should import module "LIO" and whichever label type the application is
using (e.g., "LIO.DCLabel").  The core functionality of the library is
documented in "LIO.TCB".

## Pre-requisites ##

Before building this code, you must use the following cabal commands
to install required libraries:

    cabal update
    cabal install hscolour
    cabal install SHA
    cabal install cereal
    cabal install directory filepath base64-bytestring

    cabal install dclabel

LIO and the dclabel package are developed simultaneously, so if you
are building LIO from source, it is recommended that you first build
dclabel from source:

    $ git clone http://www.scs.stanford.edu/~deian/dclabel.git
    $ cd dclabel
    $ cabal configure
    $ cabal build
    $ cabal install

## Documentation ##

The documentation must be extracted from the source code using
'haddock'.  Run the following commands to build and view the
documentation:

    gmake browse

### Research papers ###

The sequential library (guaranteeing termination-sensitive
non-interference) is described in
[this paper](http://www.scs.stanford.edu/~deian/pubs/stefan:2011:flexible-ext.pdf).

Following the first release of LIO, we've addressed internal timing
and termination covert channels, rendering the 'toLabeled' primitive
deprecated, and added concurrency. Adding concurrency to an IFC
library is usually difficult as timing and termination covert channels
scale and become a significant thred -- see "LIO.Concurrent" and
"LIO.Concurrent.LMVar" for a description of our approach to addressing
this issue. A research paper describing this approach and proofs of
its formal guarantees will be made available in the near future.
