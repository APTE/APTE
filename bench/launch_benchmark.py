#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
import os
import sys
import glob                     # for regexp
from time import clock, time
import subprocess
from datetime import datetime
import shlex
import argparse

import data
import utils

# 
def sortGraph(listEx):
   listAssoc = [(int(ex.split("Graph_")[1].split("_par")[0]), ex) for ex in listEx]
   listSorted = sorted(listAssoc, cmp=lambda (x1,x2),(y1,y2): cmp(x1, y1))
   return([ex for (nb, ex) in listSorted])

def sortWMF(listEx):
   if "diff" in listEx[0]:
      listAssoc = [(int(ex.split("-key-")[1].split("-diff.txt")[0]), ex) for ex in listEx]
   else:
      listAssoc = [(int(ex.split("-key-")[1].split(".txt")[0]), ex) for ex in listEx]
   listSorted = sorted(listAssoc, cmp=lambda (x1,x2),(y1,y2): cmp(x1, y1))
   return([ex for (nb, ex) in listSorted])

def main():
    # PARSING ARGSSS
    parser = argparse.ArgumentParser(description='Launch some benchmarks on different versions of APTE')
    parser.add_argument('-f', '--file_log',
                        help='you can choose a name of the log file')
    parser.add_argument('-v', '--version', nargs='*',
                        help='you can choose the versions in [ref,comp,red]')
    parser.add_argument('-ft', '--filter_tests',
                        help='you can filter tests by giving a substrings')

    args = parser.parse_args()

    nameFile = "results"
    if args.file_log:
        nameFile = args.file_log
    log_all = open("log/" + nameFile + ".log", "a")
    def print_all(s):
#        print s
        log_all.write(s)
        log_all.flush()
    def pprint_all(s):
        print s
        log_all.write(s)
        log_all.flush()

    def new(s):
       words = s.split("-")
       return(("PrivateAuth" in words) or
              ("shared" in words))

    list_tests = (glob.glob('protocols/*/*.txt'))
    list_tests = filter(lambda s : new(s) or utils.filterData(s,data.TESTS), list_tests)
    bina_pref = '../apte'
    if not(args.version):
        args.version = ["ref","comp","red"]
    if args.version or args.difficulty:
        if args.version:
            print(args.version)
            list_binaries = []
            if "ref" in args.version:
                list_binaries.append(bina_pref)
            if "comp" in args.version:
                bina = bina_pref + " -with_por compr improper"
                list_binaries.append(bina)
            if "red" in args.version:
                bina = bina_pref + " -with_por red improper nouse"
                list_binaries.append(bina)

    if args.filter_tests:
# We consider versions of tests where agents are different in each session only if we add '_diff ' to the filter.
       if "_diff" in args.filter_tests:
          pattern = args.filter_tests.split("_diff")[0]
          list_tests = filter(lambda s: pattern in s and "diff" in s, list_tests)
       else:
          list_tests = filter(lambda s: args.filter_tests in s and not("diff" in s), list_tests)
       if args.filter_tests == "Graph":
          list_tests = sortGraph(list_tests)
       if args.filter_tests == "WMF" or args.filter_tests == "WMF_diff":
          list_tests = sortWMF(list_tests)
       if args.filter_tests == "AKA":
          list_tests = sortAKA(list_tests)

    pprint_all("="*15 + " STARTING A NEW BENCHMARK " + "="*15 +"\n")
    pprint_all("Date: " + str(datetime.now()) + "\n")
    if not(args.version or args.difficulty):
        print("You have used no option, are you sure? Look at the helping message above.")
    pprint_all("You chose those versions: " + str(args.version) + "\n" +
               "On all those examples: " + str(list_tests) + "\n")
    raw_input("Press Enter to launch all benchmarks...")

   # BENCHMARKS
    HEAD = " " + "#"*10 + " "
    HEADA = " " + "-"*3 + " "
    IND = " " * 50

    list_binaries.sort()
    for binary in list_binaries:
        b_name = binary.split('../')[1]
        pprint_all("\n" + HEAD + "Starting a benchmark version: " + b_name + HEAD)
        log_all.write("\n")
        pprint_all(IND + str(datetime.now()) + "\n")
        for file in list_tests:
            t_name = file.split("/")[-1].split(".txt")[0]
            pprint_all(HEADA + "Benchmark of Protocol: " + t_name + HEADA)
            log_all.write("\n")
            pprint_all(IND + str(datetime.now()) + "\n") # timestamp
            log_t_b = open("log/byFiles/" + t_name + "_" + b_name + ".log", "w+")
            log_t_b.write(IND + str(datetime.now()))
            args = shlex.split(binary + " " +  file)
            print(args)
            proc = subprocess.Popen(args,
                                    stdout=subprocess.PIPE)
            for line in iter(proc.stdout.readline,''):
                line_t = line.rstrip()
                if line_t[0:3] == "Res" or "Number of " in line_t:
                    print_all(line_t + "\n")
                    print(line_t)
                else:
                    # print(line_t)
                    pass
                log_t_b.write(line_t + "\n")
                log_t_b.flush()
            log_t_b.write("\n")
            pprint_all("\n")
        log_t_b.write("\n")
        pprint_all("\n")

main()
