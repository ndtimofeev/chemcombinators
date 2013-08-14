About
=====

Initially Chemcombinatros was to be a library of combinators for building
molecular graphs. Instead, it is a set of solutions to my own business problems.
All solutions are designed so that they can be used from both repl (e.g. ghci)
and the client application code.


What's working now
------------------

Parser:
* empirical form for set common atoms (e.g. C₉H₁₃N)

        Prelude MoleculeParser.Unsafe> he "C9H13N"
        C₉H₁₃N

* halfempirical form for set common atoms and group (e.g. PhCH₂CH(NH₂)Me)
* runtime adding new groups and atoms

General:
* the average molecular weight for all available representations of molecules
* simplify halfempirical representation of molecule to empirical form
* prettyprinting
* calculation the mass distribution for the set common isotops with fixed
  precision


TODO
----

Parser:
* lazy patterns for halfempirical parser
* SMILES and SMARTS parser
* reactions parser
* single-letter peptide sequence parser
* save/load file with rules for parser and calculations

General:
* simplify deployment (no ideas)
* molecular graphs representation
* building set of molecular graphs for any molecular representation
* drawing molecular graph with diagrams
* drawing reactions with diagrams
* complete knowledge base (atomic weights, isotop distributions, etc)
* calculation reaction loads and yield


How to use it
-------------

Now with difficulty. This will require [ghc-7.6.1][1] or later, [cabal-install][2]
and [git][3].

    git clone https://github.com/KblCb/chemcombinators.git
    cd chemcombinators
    cabal install

Then run ghci and do:

    Prelude> :m + MoleculeParser.Unsafe
    Prelude MoleculeParser.Unsafe>

[1]: http://www.haskell.org/ghc/
[2]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[3]: http://git-scm.com/
