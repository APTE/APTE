**GOAL**:
 - PA4_nd1.txt should be verified quickly.
 - PA4_d.txt should take no more than 2*old POR (~max 2 seconds)

Naming: `NameProtocol_nbProcInParallel_DetFlag.txt` where DetFlag:
  - d for determinate
  - nd1: for non-det but different roles communicate on different channels
  - nd2: everything on the same channel

# Benchmarks
Benchmarks for Porridge, branch `stats` (with merge from master and simple_outputs) and for Apte, branch POR2-bench (with merge from POR2).

Legend:
 - *Old Compr (with impro.)*: compression with improper blocks elimination (i.e., `-with_por compr improper`)
 - T: more than 5 minutes
 - X: out of scope
 - M: more than 2 GO of RAM

## First benchs for [Porridge=c865, APTE=a935]
| Protocol | Without POR | Old POR | Old Compr. (with impro.) | New POR (Porridge) | Comments |
| --- | ---        | --- | --- | --- | --- |
| PA2_d	|	 0.016       |	0.004 |	0.008| 0.008 (0) | |
| PA2_nd2 |	 0.052       | X	  |	X    | 0.056 (0.004) | |
| PA3_d	|	 0.616       |	0.016 |	0.02 | 0.032 (0.012) | |
| PA3_nd1 |  0.552       |	X     |	 X   | 1.356 (1.108) | |
| PA3_nd2|	 28.58       |	X     |	 X   | 84.05 (51.74)| |
| PA4_d	|	T            |	1.29 |	1.30 | 6.65 (0.82) | objectif! |
| PA4_nd1 |              |	X |	X | M | objectif! |
| PA4_nd2|	             |	X |	X | M? | |
| PA6_d	|	             |	386.59|	364.99 | M | |
| PA6_nd1 |              |	X |	X | | |
| PA6_nd2|	             |	X |	X | | |

### Looking for costly functions
todo
