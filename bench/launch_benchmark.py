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


## I reuse an old script (for SPEC)


def main():
    # PARSING ARGSSS
    parser = argparse.ArgumentParser(description='Launch some benchmarks on different versions of APTE')
    parser.add_argument('-d', '--difficulty',
                        help='you can choose the type of examples you want to check by difficulty:  [easy,middle,hard]')
    parser.add_argument('-f', '--file_log',
                        help='you can choose a name for the results file')
    parser.add_argument('-v', '--version', nargs='*',
                        help='you can choose the version beteween [ref,comp,old_comp,comp_no_impro,red]')
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

    list_tests_tout = glob.glob('../Simple_Example/Simple_*.txt')
    list_binaries_tout = glob.glob('../apte_*')
    list_tests = list_tests_tout
    if args.version or args.difficulty:
        if args.version:
            print(args.version)
            list_binaries = []
            if "ref" in args.version:
                bina = [i for i in list_binaries_tout if ("_1_" in i)][0]
                list_binaries.append(bina)
            if "comp" in args.version:
                bina = [i for i in list_binaries_tout if ("_1_" in i)][0]
                bina = bina + " -with_por "
                list_binaries.append(bina)
            if "old_comp" in args.version:
                bina = [i for i in list_binaries_tout if ("_2_" in i)][0]
                list_binaries.append(bina)
            if "comp_no_impro" in args.version:
                bina = [i for i in list_binaries_tout if ("_4_" in i)][0]
                bina = bina + " -with_por "
                list_binaries.append(bina)
            if "red" in args.version: # TODO
                bina = [i for i in list_binaries_tout if ("_1_" in i)][0]
                bina = bina + " -with_por red "
                list_binaries.append(bina)
        if args.difficulty:
            print(args.difficulty)
            # on met les easy dans tous les cas
            def test_easy(i):
                return((("wmf" in i) or
                 ("Private" in i and not("_2_" in i or "_3_" in i)) or
                 ("Basic" in i) or
                 ("bench_3par" in i) or
                 ("tests_h_4par" in i)))
            def test_middle(i):
                return((("wmf" in i) or
                 ("Private" in i and not("_3_" in i)) or
                 ("bench_5par" in i) or
                 ("bench_7par" in i) or
                 ("bench_10par" in i) or
                 ("tests_h_4par" in i) or
                 ("Andrew" in i)))
            list_tests = [i for i in list_tests_tout if test_easy(i)]
            if "middle" in args.difficulty:
                tests_m = [i for i in list_tests_tout
                           if test_middle(i) and (not (i in list_tests)) ]
                list_tests += tests_m
            if "hard" in args.difficulty:
                list_tests = list_tests_tout
    else:
        parser.print_help()
        list_tests = list_tests_tout
        list_binaries = list_binaries_tout

    pprint_all("="*15 + " STARTING A NEW BENCHMARK " + "="*15 +"\n")
    pprint_all("Date: " + str(datetime.now()) + "\n")
    if not(args.version or args.difficulty):
        print("You use no options, are you sure? Look at the helping message above.")
    pprint_all("You choose those version: " + str(list_binaries) + "\n" +
               "On all those examples: " + str(list_tests) + "\n")
    pprint_all("Am I right?")
    raw_input("Press Enter to continue...")

   # BENCHMARKS
    HEAD = " " + "#"*10 + " "
    HEADA = " " + "-"*3 + " "
    IND = " " * 50

    list_binaries.sort()
    list_tests.sort()
    for binary in list_binaries:
        b_name = binary.split('../')[1]
        pprint_all("\n" + HEAD + "Starting a benchmark version: " + b_name + HEAD)
        log_all.write("\n")
        pprint_all(IND + str(datetime.now()) + "\n")
        for file in list_tests:
            t_name = file.split("/Simple_Example/")[1].split(".txt")[0]
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
