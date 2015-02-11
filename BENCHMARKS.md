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


Several graphs are shown in the paper.
Here are the raw results (running time in seconds and **NonTerm** when running time > 20 hours 
or Memory consumption > 15 GO):

| Protocols          |       ref           | comp          |  red        |
|:-------------------|:-------------------:|:-------------:|:-----------:|
| Bench_Graph_1      |       0.00E+00      | 0.00E+00      | 0.00E+00    |
| Bench_Graph_2      |       0.00E+00      | 0.00E+00      | 0.00E+00    |
| Bench_Graph_3      |       1.60E-02      | 4.00E-03      | 4.00E-03    |
| Bench_Graph_4      |       6.16E-01      | 2.40E-02      | 1.20E-02    |
| Bench_Graph_5      |       3.46E+01      | 1.80E-01      | 4.00E-02    |
| Bench_Graph_6      |       2.79E+03      | 1.49E+00      | 1.00E-01    |
| Bench_Graph_7      |        NonTerm      | 1.25E+01      | 3.28E-01    |
| Bench_Graph_8      |        NonTerm      | 1.17E+02      | 9.36E-01    |
| Bench_Graph_9      |        NonTerm      | 1.39E+03      | 2.49E+00    |
| Bench_Graph_10      |       NonTerm      | 1.58E+04      | 6.63E+00    |
| Bench_Graph_11      |       NonTerm      |  NonTerm      | 1.72E+01    |
| Bench_Graph_12      |       NonTerm      |  NonTerm      | 4.06E+01    |
| Bench_Graph_13      |       NonTerm      |  NonTerm      | 9.38E+01    |
| Bench_Graph_14      |       NonTerm      |  NonTerm      | 2.52E+02    |
| Bench_Graph_15      |       NonTerm      |  NonTerm      | 7.03E+02    |
| Bench_Graph_16      |       NonTerm      |  NonTerm      | 1.68E+03    |
| Bench_Graph_17      |       NonTerm      |  NonTerm      | 3.50E+03    |
| Bench_Graph_18      |       NonTerm      |  NonTerm      | 7.20E+03    |
| Bench_Graph_19      |       NonTerm      |  NonTerm      | 1.72E+04    |
| Bench_Graph_20      |       NonTerm      |  NonTerm      | 4.02E+04    |
| Bench_Graph_21      |       NonTerm      |  NonTerm      | 8.93E+04    |
| Bench_Graph_22      |       NonTerm      |  NonTerm      |  NonTerm    |
| Bench_Graph_23      |       NonTerm      |  NonTerm      |  NonTerm    |
| Bench_Graph_24              NonTerm      |  NonTerm      |  NonTerm    |
| DS-Shared-3      |         1.72E-01      | 8.20E-03      | 8.00E-03    |
| DS-Shared-4      |         5.78E+01      | 3.00E-01      | 1.64E-01    |
| DS-Shared-5      |         2.69E+04      | 4.20E+00      | 1.16E+00    |
| DS-Shared-6      |          NonTerm      | 5.06E+00      | 1.36E+00    |
| DS-Shared-7      |          NonTerm      | 6.43E+02      | 3.66E+01    |
| DS-Shared-8      |          NonTerm      | 1.36E+04      | 2.94E+02    |
| DS-Shared-9      |          NonTerm      | 1.49E+04      | 3.74E+02    |
| DS-Shared-10      |         NonTerm      |  NonTerm      | 1.33E+04    |
| DS-Shared-11      |         NonTerm      |  NonTerm      | 5.13E+04    |
| NS_SharedK_3      |        2.96E-01      | 4.40E-02      | 4.00E-02    |
| NS_SharedK_4      |        2.13E+02      | 3.41E+00      | 2.04E+00    |
| NS_SharedK_5      |         NonTerm      | 6.40E+01      | 1.23E+01    |
| NS_SharedK_6      |         NonTerm      | 4.19E+03      | 1.29E+02    |
| NS_SharedK_7      |         NonTerm      |  NonTerm      | 1.56E+04    |
| NS_SharedK_8      |         NonTerm      |  NonTerm      |  NonTerm    |
| NS_SharedK_9      |         NonTerm      |  NonTerm      |  NonTerm    |
| Pass-PA_ano_2      |       4.40E-02      | 2.80E-02      | 2.40E-02    |
| Pass-PA_ano_3      |       1.09E+01      | 6.80E-02      | 7.20E-02    |
| Pass-PA_ano_4      |       7.12E+03      | 2.94E+00      | 1.52E+00    |
| Pass-PA_ano_5      |        NonTerm      | 6.98E+00      | 3.02E+00    |
| Pass-PA_ano_6      |        NonTerm      | 1.95E+03      | 1.24E+02    |
| Pass-PA_ano_7      |        NonTerm      | 6.19E+03      | 4.60E+02    |
| Pass-PA_ano_8      |        NonTerm      |  NonTerm      | 1.73E+04    |
| Pass-PA_ano_9      |        NonTerm      |  NonTerm      | 4.12E+04    |
| Pass-PA_ano_10     |        NonTerm      |  NonTerm      |  NonTerm    |
| PrivateAuth-2      |       3.20E-02      | 1.20E-02      | 1.20E-02    |
| PrivateAuth-3      |       1.24E+00      | 3.20E-02      | 2.80E-02    |
| PrivateAuth-4      |       1.61E+03      | 1.91E+00      | 1.76E+00    |
| PrivateAuth-5      |        NonTerm      | 7.33E+00      | 7.27E+00    |
| PrivateAuth-6      |        NonTerm      | 9.52E+02      | 8.10E+02    |
| PrivateAuth-7      |        NonTerm      | 3.23E+03      | 2.88E+03    |
| PrivateAuth-8      |        NonTerm      |  NonTerm      |  NonTerm    |
| PrivateAuth-9      |        NonTerm      |  NonTerm      |  NonTerm    |
| WMF_SS_10      |            NonTerm      | 1.00E+03      | 9.55E+01    |
| WMF_SS_11      |            NonTerm      | 1.32E+04      | 1.10E+03    |
| WMF_SS_12      |            NonTerm      | 1.56E+04      | 1.18E+03    |
| WMF_SS_13      |            NonTerm      |  NonTerm      | 1.27E+04    |
| WMF_SS_14      |            NonTerm      |  NonTerm      |  NonTerm    |
| WMF_SS_3      |            8.00E-03      | 4.00E-03      | 4.00E-03    |
| WMF_SS_4      |            3.08E-01      | 1.20E-02      | 1.20E-02    |
| WMF_SS_5      |            1.63E+01      | 1.44E-01      | 1.08E-01    |
| WMF_SS_6      |            4.95E+02      | 1.80E-01      | 1.32E-01    |
| WMF_SS_7      |             NonTerm      | 2.42E+00      | 8.92E-01    |
| WMF_SS_8      |             NonTerm      | 3.26E+01      | 1.07E+01    |
| WMF_SS_9      |             NonTerm      | 3.78E+01      | 1.12E+01    |
| Yahalom-3      |           1.13E+01      | 4.00E-01      | 3.60E-01    |
| Yahalom-4      |           1.33E+04      | 9.25E+01      | 4.03E+01    |
| Yahalom-5      |            NonTerm      | 2.27E+02      | 1.19E+02    |
| Yahalom-6      |            NonTerm      |  NonTerm      |  NonTerm    |
| Yahalom-7      |            NonTerm      |  NonTerm      |  NonTerm    |
| Yahalom-8      |            NonTerm      |  NonTerm      |  NonTerm    |
| Yahalom-9      |            NonTerm      |  NonTerm      |  NonTerm    |


[Protocols Files]:https://github.com/APTE/APTE/tree/master/bench/protocols
[Yahalom]:http://www.lsv.ens-cachan.fr/Software/spore/yahalom.html
[Dennin Sacco]:http://www.lsv.ens-cachan.fr/Software/spore/denningSacco.html
[Needham Shroeder Symmetric Key]:http://www.lsv.ens-cachan.fr/Software/spore/nssk.html
[Passive Autentication in ePassport]:http://www.lsv.ens-cachan.fr/Publis/PAPERS/PDF/ACD-csf12.pdf
[Private Authentication]:https://users.soe.ucsc.edu/~abadi/Papers/tcs-private-authentication.pdf
[Wide Mouthed Frog]:http://www.lsv.ens-cachan.fr/Software/spore/wideMouthedFrog.html