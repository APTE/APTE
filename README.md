#APTE : Algorithm for Proving Trace Equivalence
Version : 0.4beta

For more information, visit the [dedicated website](http://projects.lsv.ens-cachan.fr/APTE/).


1. Installation
2. Usage 
3. Uninstall

********************************
********************************

###1. Installation

To install **APTE**, first reach the extracted directory APTE-v0.4beta, then enter the following commands:

1. cd Source
2. make

Once the installation is done, an executable 'apte' is created in the directory main directory APTE-v0.4beta

###2. Usage

#####Synopsis :

	apte [-debug high|low|none] [-unfold] [-no_comm] [-no_erase] [-verbose [<int>]] [-display size|step]
	[-log <int>] file

#####Options :

- -debug [high|low|none]

    APTE is programmed with three level of debugging.
    + The High debugging option checks several invariants of the algorithms. While this mode provides more guarantee about the result, it makes the algorithm slower.
    + The Low debugging option only checks basic invariants. (default) 
    +The None debugging option does not check any invariant. Chose this option for optimal running time.

- -display step : Show statistics on the matrices generated by APTE for each main step of the algorithm.

- -display size : Group the statistics on the matrices generated by the size traces.

- -log &lt;int&gt; : Log all the symbolic processes and the matrices obtained on the leaves for all traces of size smaller than or equal to <int>.

- -no_comm : Does not consider the internal communication in the trace equivalence.  
**WARNING** : This option should not be used in presence of private channel.

- -no_erase : Does not consider a slight optimisation that consists of removing symbolic processes with the same process during the execution of the algorithm.  
**Note** : This option is automatically activated when -unfold is used.

- -unfold : Use the glutton strategy that consists of unfolding all symbolic traces
          and apply the symbolic equivalence decision procedure for each of them.

- -verbose [&lt;int&gt;] : Display some statistics on the matrices generated at the.
          end of the execution. When an integer &lt;int&gt; is given, the statistics are displayed
          every &lt;int&gt; matrices generated.

###3. Uninstall

To uninstall APTE, first reach the extracted directory APTE_v0.4beta, then enter the following commands:

1. cd Source
2. make clean
