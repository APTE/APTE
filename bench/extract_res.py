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
import pprint
import logging
import math
import marshal

## I reuse an old script (for SPEC)

logging.basicConfig(stream=sys.stdout,
                    level=logging.WARNING)


DICO = {
    'ref' : {
        "name" : "Apte without POR (reference version)",
        "call" : "apte_1_new_all",
        "branch" : "",
        "benchs": {
            "TEST": {
                "file": "TEST.txt",
                "res" : True,
                "date" : "1263",
                "time": "453",
                "nbExplo" : "4674",
                "fileFrom" : "BENCH.log"
            }
        }
    },
    'old_comp' : {
        "name" : "Old compression like defined in POST'14",
        "call" : "apte_2_old_compr",
        "branch" : "",
        "benchs": {}
    },
    'old_red' : {
        "name" : "Old reduction like defined in POST'14",
        "call" : "apte_3_old_red",
        "branch" : "",
        "benchs": {}
    },
    'comp' : {
        "name" : "Compression (+ killing improper)",
        "call" : "apte_1_new_all -with_por compr improper",
        "branch" : "",
        "benchs": {}
    },
    'comp_no_impro' : {
        "name" : "Compression (- killing improper)",
        "call" : "apte_1_new_all -with_por compr",
        "branch" : "",
        "benchs": {}
    },
    'red' : {
        "name" : "Reduction (+ killing improper + NoUse criterion)",
        "call" : "apte_1_new_all -with_por red improper nouse",
        "branch" : "",
        "benchs": {}
    },
    'red_no_impro' : {
        "name" : "Reduction (- killing improper + NoUse criterion)",
        "call" : "apte_1_new_all -with_por red nouse",
        "branch" : "",
        "benchs": {}
    },
    'red_no_nouse' : {
        "name" : "Reduction (+ killing improper - NoUse criterion)",
        "call" : "apte_1_new_all -with_por red improper",
        "branch" : "",
        "benchs": {}
    },
    'red_no_2_nouse_improper' : {
        "name" : "Reduction (- killing improper - NoUse criterion)",
        "call" : "apte_1_new_all -with_por red",
        "branch" : "",
        "benchs": {}
    }
}

def extractBench(text):
    lastBench = text.split("=============== STARTING A NEW BENCHMARK ===============")[1]
    return(lastBench)

def extractVers(text):
    SEP = "##########"
    if not(SEP in text):
        return []
    listVers = text.split(SEP)[1:]
    listVers2 = []
    i = 0
    while(i <len(listVers)):
        vers = listVers[i].split(": ")[1]
        benchVers = listVers[i+1]
        listVers2.append((vers, benchVers))
        i = i + 2
    return(listVers2)

def extractTests(text):
    SEP = "---"
    if not(SEP in text):
        return []
    listTests = text.split(SEP)[1:]
    listTests2 = []
    i = 0
    while(i <len(listTests)):
        vers = listTests[i].split(": ")[1]
        benchTests = listTests[i+1]
        listTests2.append((vers, benchTests))
        i = i+2
    return(listTests2)

def findVers(call, dico):
    res = {}
    resKey = ""
    for versKey in dico:
        vers = dico[versKey]
        if (vers["call"].strip() == call.strip()):
            res = vers
            resKey = versKey
    return(resKey)

    # MAIN
def main():
    nameFile = "summary"
    log_all = open("summary/" + nameFile + ".log", "a")
    def print_all(s):
#        print s
        log_all.write(s)
        log_all.flush()
    def pprint_all(s):
        print s
        log_all.write(s)
        log_all.flush()

    pp = pprint.PrettyPrinter(indent=1)
    nbTests = 0
    nbVers = 0
    nbNewTests = 0
    nbRewrite = 0
    list_tests_tout = glob.glob('../Simple_Example/Simple_*.txt')
    list_binaries_tout = glob.glob('../apte_*')
    list_tests = list_tests_tout
    listLog = glob.glob('log/*.log')
    listLog = glob.glob('log_BCK/red_red_nouse.log')
    listLog = glob.glob('log_BCK/*.log')
    dicoPath = "summary/DumpRes.json"
    isLoad = True
    if isLoad:
        dicoFile = open(dicoPath, 'rb')
        dico = marshal.load(dicoFile)
        dicoFile.close()
    else:
        dico = DICO
    for log in listLog:
        logging.debug("=" * 20 + "   NEW logFile   " + "=" * 20)
        logging.debug("logFile: " + log + "\n")
        logFile = open(log, 'r')
        logText = logFile.read()
        bench = extractBench(logText)
        listVers = extractVers(bench)
        # listVers: list of pairs (version, raw bench)
        for el in listVers:
            nbVers = nbVers +1
            (version, benchVers) = el
            versionKey = findVers(version, dico)
            versionDico =dico[versionKey] 
            versionName = versionDico["name"]
            logging.debug(" ----- NEW version: " + versionName + " ----- ")
            listTests = extractTests(benchVers)
            # listTests: list of pairs (test, raw bench)
            for el in listTests:
                (test, benchTests) = el
                if not("obtained" in benchTests):
                    logging.info("new test: " + test)
                    logging.info("Test is not yet finished...")
                else:
                    nbTests = nbTests + 1
                    testName = test.split(".")[0]
                    testFile = test
                    isTrue = ("true" in benchTests)
                    date = benchTests.splitlines()[1].strip()
                    if "explorations:" in benchTests:
                        nbExplo = int(benchTests.split("explorations:")[1].split(".")[0])
                    else:
                        nbExplo = -1
                    time = float(benchTests.split("obtained in")[1].split(" seconds")[0])
                    logging.info("New test: " + testName + "|: True? " + str(isTrue) + ", nbExplo: " + str(nbExplo) +
                                 ", date: " + date + ", time: " + str(time) + "  |  ")
                    testDico = {
                        "file": testFile,
                        "res" : isTrue,      # bool
                        "date" : date,       # string
                        "time" : time,       # float
                        "nbExplo" : nbExplo, # int
                        "fileFrom": log,     # string
                    }
                    if testName in versionDico["benchs"]:
                        oldTest = versionDico["benchs"][testName]
                        oldDate = oldTest["date"]
                        oldTime = oldTest["time"]
                        oldFile = oldTest["fileFrom"]
                        diff = math.fabs(oldTime - time)
                        if max(time,oldTime) == 0:
                            diffRel = 0
                        else:
                            diffRel = (diff / max(time,oldTime))
                        overWrite = ""
                        if (date > oldDate): # NE MARCHE PAS SI ON COMPARE DS LA MEME JOURNEE !!
                            nbRewrite = nbRewrite + 1
                            overWrite = " --> OVERWRITTEN! "
                            versionDico["benchs"][testName] = testDico
                        toPrint = (("Diff rel: %f%s--- Clash for version %s on test %s.   --- Difference: %f.\n" +
                                    " " * 30 + "OLD/NEW for time: %f/%f, Date: %s / %s" +
                                    ", logFile: %s/%s.") %
                                   (diffRel, overWrite, versionKey, testName, diff, oldTime, time, oldDate, date, oldFile, log))
                        if diffRel > 0.05:
                            logging.error(toPrint)
                        elif diffRel > 0.0001:
                            logging.warning(toPrint)
                    else:
                        nbNewTests = nbNewTests + 1
                        versionDico["benchs"][testName] = testDico
            logging.info("\n")

    # pp.pprint(dico)
    print("~~~~~~~~~ Some Stats ~~~~~~~~~\n" +
          "Nb. of Tests: %d. Number of versions: %d. Number of new tests: %d. Number of rewrites: %d." % (nbTests, nbVers, nbNewTests, nbRewrite))
    dicoFile = open(dicoPath, 'wb')
    marshal.dump(dico, dicoFile)
    dicoFile.close()

main()
