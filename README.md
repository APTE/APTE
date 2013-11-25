APTE : Algorithm for Proving Trace Equivalence
Version : 0.3.1beta

For more information, visit the [dedicated website.](http://projects.lsv.ens-cachan.fr/APTE/).


1) Installation
2) Usage 
3) Uninstall

********************************
********************************

1) Installation

To install APTE, first reach the extracted directory APTE_v0.3beta, then enter the following commands:

- cd Source
- make

Once the installation is done, an executable 'apte' is created in the directory APTE_v0.3beta

2) Usage

Synopsis :
      apte [-debug [high|low|none]] file

Options :
      -debug [high|low|none] : APTE is programmed with three level of debugging.
          The High debugging option checks several invariants of the algorithms.
          While this mode provides more guarantee about the result, it makes the
          algorithm slower.
          The Low debugging option only checks basic invariants. (default) 
          The None debugging option does not check any invariant. Chose this option
          for optimal running time.

3) Uninstall

To uninstall APTE, first reach the extracted directory APTE_v0.3beta, then enter the following commands:

- cd Source
- make clean
