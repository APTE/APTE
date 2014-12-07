#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
import os
import sys
import glob                     # for regexp
from time import clock, time
import subprocess
from datetime import datetime

## I reuse an old script (for SPEC)

def main():
    HEAD = " " + "#"*10 + " "
    HEADA = " " + "#"*3 + " "
    IND = " " * 50
    list_tests_tout = glob.glob('../Simple_Example/Simple_*.txt')
    list_tests =  [i for i in list_tests_tout if not("10" in i)]
    list_binaries = glob.glob('../apte_*')
    list_binaries.sort()
    list_tests.sort()
    log_all = open("log/results" + ".log", "a")
    def print_all(s):
#        print s
        log_all.write(s)
        log_all.flush()
    print_all("="*15 + " STARTING A NEW BENCHMARK " + "="*15 +"\n")
    print_all("Date: " + str(datetime.now()) + "\n")
    print("Liste Binaires: " + str(list_binaries) + "\n" +
          "Liste exemples: " + str(list_tests) + "\n")
    for binary in list_binaries:
        b_name = binary.split('../')[1]
        print_all("\n" + HEAD + "Starting to benchmark version: " + b_name + HEAD + "\n")
        print_all(IND + str(datetime.now()) + "\n")
        for file in list_tests:
            t_name = file.split("/Simple_Example/")[1].split(".txt")[0]
            print_all(HEADA + "Benchmark of Protocol: " + t_name + HEADA + "\n")
            print_all(IND + str(datetime.now()) + "\n") # timestamp

            log_t_b = open("log/" + t_name + "_" + b_name + ".log", "w+")
            log_t_b.write(IND + str(datetime.now()))
            proc = subprocess.Popen([binary, file],
                                    stdout=subprocess.PIPE)
            for line in iter(proc.stdout.readline,''):
                line_t = line.rstrip()
                if line_t[0:3] == "Res":
                    print_all(line_t + "\n")
                else:
                    print(line_t)
                log_t_b.write(line_t + "\n")
                log_t_b.flush()
        print_all("\n")

main()
