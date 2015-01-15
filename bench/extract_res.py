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

from texttable import *

import data
## I reuse an old script (for SPEC)

logging.basicConfig(stream=sys.stdout,
                    level=logging.WARNING,
                    format="%(message)s")
rootLogger = logging.getLogger()

logFormatter = logging.Formatter("%(asctime)s [%(levelname)-5.5s] | %(message)s")
warn=logging.FileHandler("summary/LOG_warn.log")
err=logging.FileHandler("summary/LOG_err.log")
debug=logging.FileHandler("summary/LOG_debug.log")
warn.setFormatter(logFormatter)
err.setFormatter(logFormatter)
debug.setFormatter(logFormatter)
warn.setLevel(logging.WARNING)
err.setLevel(logging.ERROR)
debug.setLevel(logging.DEBUG)
rootLogger.addHandler(warn)
rootLogger.addHandler(debug)
rootLogger.addHandler(err)


isLoad = True
TESTSDICO = data.get_testsDico()
VERSDICO = data.get_versDico()

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

def findVers(call, dicoVersions):
    res = {}
    resKey = ""
    for versKey in dicoVersions:
        vers = dicoVersions[versKey]
        if (vers["call"].strip() == call.strip()):
            res = vers
            resKey = versKey
    return(resKey)

def findTest(fileName, dicoTests):
    res = {}
    resKey = ""
    for testKey in dicoTests:
        test = dicoTests[testKey]
        if (test["file"].strip() == fileName.strip()):
            res = test
            resKey = testKey
            return(resKey)

def pprintMatrix(matrix):
    lm = len(matrix[0])-1
    table = Texttable()
    firstWidth = 15
    width = 12
    # table.set_cols_align(["l", "r", "c"])
    # table.set_deco(Texttable.HEADER)
    table.set_deco(Texttable.BORDER | Texttable.HEADER)
    table.set_precision(2)
    table.set_cols_width([firstWidth]+ ([width]*lm))
    table.set_cols_align(["l"] + (["c"]*lm))
    table.set_cols_dtype(['t'] +  # text 
                         (['a']*lm)) # automatic
    # table.set_cols_valign(["t", "m", "b"])
    table.add_rows(matrix)
    return(table.draw())

def extractResults(dicoV, sortedV, dicoT, keyT):
    # First column of the line:
    res = [keyT]
    for keyV in sortedV:
        versionDico = dicoV[keyV]
        versionBenchs = versionDico["benchs"]
        found = False
        for bench in versionBenchs:
            if (not(found) and
                 versionBenchs[bench]["file"].strip() == dicoT[keyT]["file"].strip()):
                #res.append((versionBenchs[bench]["time"], versionBenchs[bench]["nbExplo"]))
                if versionBenchs[bench]["res"] != dicoT[keyT]["res"]:
                    res.append("> X <")
                else:
                    res.append(versionBenchs[bench]["time"])
                found = True
        if not(found):
            res.append(".")
    return(res)

def fromVersToTests(dicoVersions, dicoTests):
    sortedVersions = ['ref', 'old_comp', 'comp_no_impr', 'comp',  'old_red',  'red_no_2', 'red_no_impr', 'red_no_nouse', 'red']
    listTestsKey = sorted(dicoTests.keys())
    listTestsFile = map(lambda x: dicoTests[x]['file'], listTestsKey)
    # first line of the matrix:
    matrix = [[" / "] + sortedVersions]
    for i in range(len(listTestsFile)):
        keyTest = listTestsKey[i]
        fileName = listTestsFile[i]
        listResults = extractResults(dicoVersions, sortedVersions, dicoTests, keyTest)
        matrix.append(listResults)
    return(pprintMatrix(matrix))


# --------------------------- MAIN ------------------------------------------ #
def main():
    nameFile = "summary"
    log_all = open("summary/" + nameFile + ".log", "a")
    def print_all(s):
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
    dicoPath = "summary/DumpRes.json"
    TestsDico = TESTSDICO
    if isLoad:
        dicoFile = open(dicoPath, 'rb')
        VersionsDico = marshal.load(dicoFile)
        dicoFile.close()
    else:
        VersionsDico = VERSDICO
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
            versionKey = findVers(version, VersionsDico)
            versionDico =VersionsDico[versionKey] 
            versionName = versionDico["name"]
            logging.debug(" ----- NEW version: " + versionName + " ----- ")
            listTests = extractTests(benchVers)
            # listTests: list of pairs (test, raw bench)
            for el in listTests:
                (test, benchTests) = el
                if not("obtained" in benchTests):
                    logging.debug("new test: " + test)
                    logging.debug("Test is not yet finished...")
                else:
                    nbTests = nbTests + 1
                    testName = test.split(".")[0]
                    testFile = test.strip() + ".txt"
                    isTrue = ("true" in benchTests)
                    date = benchTests.splitlines()[1].strip()
                    testKey = findTest(testFile, TestsDico)
                    if testKey == "":
                        logging.error("The tests %s cannot be found.\n" % testFile)
                        return()
                    testDico = TestsDico[testKey]
                    if testDico['res'] != isTrue:
                        logging.error("NOT EXPECTED RESULT. The version %s on test %s answerd %s."
                                      % (versionName, testName, str(isTrue)))
                    if "explorations:" in benchTests:
                        nbExplo = int(benchTests.split("explorations:")[1].split(".")[0])
                    else:
                        nbExplo = -1
                    time = float(benchTests.split("obtained in")[1].split(" seconds")[0])
                    logging.debug("New test: " + testName + "|: True? " + str(isTrue) + ", nbExplo: " + str(nbExplo) +
                                 ", date: " + date + ", time: " + str(time) + "  |  ")
                    testDico = {
                        "file": testFile,    # str
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
                        isOverWrite = False
                        if (date > oldDate): # NE MARCHE PAS SI ON COMPARE DS LA MEME JOURNEE !!
                            nbRewrite = nbRewrite + 1
                            overWrite = " --> OVERWRITTEN! "
                            versionDico["benchs"][testName] = testDico
                            isOverWrite = True
                        toPrint = (("Diff rel: %f%s--- Clash for version %s on test %s.   --- Difference: %f.\n" +
                                    " " * 30 + "OLD/NEW for time: %f/%f, Date: %s / %s" +
                                    ", logFile: %s/%s.") %
                                   (diffRel, overWrite, versionKey, testName, diff, oldTime, time, oldDate, date, oldFile, log))
                        if diffRel > 0.07 or isOverWrite:
                            logging.error(toPrint)
                        elif diffRel > 0.0001:
                            logging.warning(toPrint)
                    else:
                        nbNewTests = nbNewTests + 1
                        versionDico["benchs"][testName] = testDico
            logging.debug("\n")

    # pp.pprint(dico)
    def print2(s):
        print(s)
        logging.info(s)

    print2("\n~~~~~~~~~ Some Stats ~~~~~~~~~\n" +
          "Nb. of Tests: %d. Number of versions: %d. Number of new tests: %d. Number of rewrites: %d." % (nbTests, nbVers, nbNewTests, nbRewrite))
    dicoFile = open(dicoPath, 'wb')
    marshal.dump(VersionsDico, dicoFile)
    dicoFile.close()

    print2("\n~~~~~~~~~ Results ~~~~~~~~~")
    print2(fromVersToTests(VersionsDico, TestsDico))
    print2("Captions: [> X <] if the returned result is false, [.] if is there is no benchmark.")
    logging.error("#" * 80 + "\n")
main()


# LIB TEXTTABLE:
#     # table.set_deco(Texttable.HEADER)
