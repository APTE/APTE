# BENCHMARKING POR
We present on this page how to build APTE and instructions for reproduction of our benchmarks.

>APTE: *Algorithm for Proving Trace Equivalence*. 
>This tool is a decision procedure for proving trace equivalence between two processes for a bounded number of sessions.

Some links:
[official website](http://projects.lsv.ens-cachan.fr/APTE/),
[official distribution on Github](https://github.com/APTE/APTE),
and the [diff](https://github.com/APTE/APTE/compare/lutcheti:ref...APTE:POR) 
containing only POR contributions.

## How to build APTE and launch benchmarks
### Build APTE
Download APTE either using git and the [official repository](https://github.com/APTE/APTE):
```git
git clone https://github.com/APTE/APTE.git
cd APTE
```
or by downloading a tarball:
```sh
$ wget https://github.com/APTE/APTE/archive/master.zip
$ unzip master.zip
$ cd APTE-master
```
In order to compile APTE, you need an OCaml compiler > 3.0 (see [OCaml Download]) and then type:
```sh
$ make
```
producing the executable '*apte*'. A help message is displayed when typing:
```sh
$ ./apte
```
### Benchmarks
Some python scripts are available to run benchmarks and extract results.
You need Python > 2.0.

####  Launch benchmarks
To launch benchmarks, go to '*bench*' folder and use the script '*launch_benchmark.py*'.
By default, the script launches all versions of APTE on all protocols.
You can also use arguments as follows:
```sh
$ cd bench
$ ./launch_benchmark.py -v [versions] -ft [filter] -f [filename]
```
- where [versions] are a list of versions taken from ["ref", "comp", "red"] (ex. "-v ref red");
- where [filter] is used to select all protocols whose names contain [filter] (ex. "WMF", "AKA", "Graph", "DS", "NS", "Yahalom");
- where [filename] is used to create a log file inside '*log/*' folder.

It is advised to launch in parallel one instance of this script per core of your processor since one instance will only use one core.
For instance:
```sh
$ ./launch_benchmark.py -v ref -ft WMF -f WMF_ref
$ ./launch_benchmark.py -v red -ft WMF -f WMF_red
$ ./launch_benchmark.py -v red -ft Simple -f Simple_toy_example_red
```
If one test takes to much time (exceed your time out) you can kill the APTE process (*'htop'* and press *'k'*).
You can then manually extract results from log files or read the following section.

#### Extract results
Once '*launch_benchmark.py*' performed enough benchmarks, you can extract results and display them in a matrix using the script '*extract_result.py*'.
This script looks for log files in the foler '*log*', extract results and produce a matrix. Note that if you
killed a process launched by '*launch_benchmark.py*' because it took to much time, *'extract_res.py'*
is not able to extract this information.
To use this script, you need the followings libraries (*easy_install [lib]* or *pip install [lib]' to install*): 
- [dateutil];
- [tabulate].

To display results (in seconds), type:
```sh
$ ./extract_res.py
```
[APTE]:https://github.com/APTE/APTE
[APTE_POR]:https://github.com/APTE/APTE/tree/POR
[OCaml Download]: http://caml.inria.fr/download.en.html
[dateutil]: https://pypi.python.org/pypi/python-dateutil/2.4.0
[tabulate]: https://pypi.python.org/pypi/tabulate

## Tested protocols and results
### Protocols
We performed benchmarks on real world protocols. They all are defined in the folder *./bench/protocols/* (via Github: [Protocols Files]). We check various properties. We list here all tested protocols and what property we check using APTE:
- [Yahalom] (folder *Yahalom*): we test strong secrecy of the established key received by Bob (the responder);
- [Needham Shroeder Symmetric Key] (folder *NS_sym*): we test strong secrecy of the established key received by Bob (the responder);
- [Dennin Sacco] (folder *DenningSacco*): we test strong secrecy of the established key received by Bob (the responder);
- [Passive Autentication in ePassport] (folder *Passport_PA*): we test anonymity of the ePassport;
- [Private Authentication] (folder *PrivateAuthentication*): we test anonymity of Bob (the responder);
- [Wide Mouthed Frog] (folder *WMF*): we test strong secrecy of the established key received by Bob (the responder).

### Results
We ran the three versions of APTE (reference, compression and reduction) on 
those protocols with various sizes (number of agents in parallel).
We performed those benchmarks on this machine:
- OS: Linux sume 3.10-2-amd64 #1 SMP Debian 3.10.5-1 (2013-08-07) x86_64 GNU/Linux
- CPU: Intel(R) Xeon(R) CPU X5650  @ 2.67GHz / stepping: 2 / microcode: 0x13 / cpu MHz: 2659.937 / cache size: 12288 KB
- RAM: 47GO

Note that, APTE only uses one core of the processor.

The results can be found in the journal paper **A Reduced Semantics for Deciding Trace Equivalence** in *Logical Methods in Computer Science*.

[Protocols Files]:https://github.com/APTE/APTE/tree/master/bench/protocols
[Yahalom]:http://www.lsv.ens-cachan.fr/Software/spore/yahalom.html
[Dennin Sacco]:http://www.lsv.ens-cachan.fr/Software/spore/denningSacco.html
[Needham Shroeder Symmetric Key]:http://www.lsv.ens-cachan.fr/Software/spore/nssk.html
[Passive Autentication in ePassport]:http://www.lsv.ens-cachan.fr/Publis/PAPERS/PDF/ACD-csf12.pdf
[Private Authentication]:https://users.soe.ucsc.edu/~abadi/Papers/tcs-private-authentication.pdf
[Wide Mouthed Frog]:http://www.lsv.ens-cachan.fr/Software/spore/wideMouthedFrog.html