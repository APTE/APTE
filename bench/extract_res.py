#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
import os
import sys
import glob                     # for regexp
from time import clock, time
import subprocess
from datetime import datetime, timedelta
import shlex
import argparse
import pprint
import logging
import math
import marshal

import dateutil.parser
from rainbow_logging_handler import RainbowLoggingHandler
from texttable import *

import data
from utils import *

isLoad = True
dateMajorPatch = dateutil.parser.parse('2015-01-26 19:20:38.616735')

# -- LOGGING --
rootLogger = logging.getLogger()
rootLogger.setLevel(logging.DEBUG)
DATEFMT_L = "%m-%d %H:%M:%S"
DATEFMT_S = "%d %H:%M:%S"
# logging.basicConfig(stream=sys.stdout,
#                     level=logging.WARNING,
#                     format="%(message)s")
err=logging.FileHandler("summary/LOG_err.log")
warn=logging.FileHandler("summary/LOG_warn.log")
debug=logging.FileHandler("summary/LOG_debug.log")

logFormatter = logging.Formatter("%(asctime)s [%(levelname)-5.5s] | %(message)s",
                                 datefmt=DATEFMT_L)
warn.setFormatter(logFormatter)
err.setFormatter(logFormatter)
debug.setFormatter(logFormatter)
warn.setLevel(logging.WARNING)
err.setLevel(logging.ERROR)
debug.setLevel(logging.DEBUG)

# Files
rootLogger.addHandler(warn)
rootLogger.addHandler(debug)
rootLogger.addHandler(err)

# Stdout
handler = RainbowLoggingHandler(sys.stderr,
                                datefmt=DATEFMT_S)
handler.setFormatter(logFormatter)
handler.setLevel(logging.WARNING)
rootLogger.addHandler(handler)

# -- OPTIONS AND DATA (from data.py) --
TESTSDICO = data.get_testsDico()
VERSDICO = data.get_versDico()

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def print2(s):
    print(s)
    logging.debug(s)


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
    VersionsDico = setNoNew(VersionsDico)
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
                elif "Simple_wmf" == test.strip():
                    logging.debug("We do not consider test of Simple_wmf.txt anymore.")
                else:
                    nbTests = nbTests + 1
                    testName = test.split(".")[0]
                    testFile = test.strip() + ".txt"
                    isTrue = ("true" in benchTests)
                    date = benchTests.splitlines()[1].strip()
                    testKey = findTest(testFile, TestsDico)
                    if testKey == "" or testKey == None:
                        logging.critical("The tests %s cannot be found.\n" % testFile)
                        return()
                    testDico = TestsDico[testKey]
                    if testDico['res'] != isTrue:
                        if ((dateutil.parser.parse(date) < dateutil.parser.parse('2015-01-21 14:13:38.616735')) and
                            not(versionKey[0:3] == "ref" or versionKey[0:3] == "old")):
                            logging.info("NOT EXPECTED RESULT. But this is normal since this version was before the major patch."
                                         "The version %s on test %s answerd %s."
                                         % (versionName, testName, str(isTrue)))
                        else:
                            logging.critical("NOT EXPECTED RESULT. The version %s on test %s answerd %s."
                                             % (versionName, testName, str(isTrue)))
                    if "explorations:" in benchTests:
                        nbExplo = int(benchTests.split("explorations:")[1].split(".")[0])
                    else:
                        nbExplo = -1
                    time = float(benchTests.split("obtained in")[1].split(" seconds")[0])
                    logging.debug("New test: " + testName + "|: True? " + str(isTrue) + ", nbExplo: " + str(nbExplo) +
                                 ", date: " + date + ", time: " + str(time) + "  |  ")
                    testDico = {
                        "new" : True,        # bool
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
                        comm = ""
                        if ((dateutil.parser.parse(date) < dateMajorPatch or (dateutil.parser.parse(oldDate) <dateMajorPatch)) and
                            not(versionKey[0:3] == "ref" or versionKey[0:3] == "old")):
                            comm = "Not surprising, we compare two benchs on a reduced version before and after the major patch! -- "
                        if (dateutil.parser.parse(date) > dateutil.parser.parse(oldDate)):
                            nbRewrite = nbRewrite + 1
                            overWrite = " --> OVERWRITTEN! "
                            versionDico["benchs"][testName] = testDico
                            isOverWrite = True
                        toPrint = (("Diff rel: %f%s--- %sClash for version %s on test %s.   --- Difference: %f.\n" +
                                    " " * 30 + "OLD/NEW for time: %f/%f, Date: %s / %s" +
                                    ", logFile: %s/%s.") %
                                   (diffRel, comm, overWrite, versionKey, testName, diff, oldTime, time, oldDate, date, oldFile, log))
                        if comm != "":
                            logging.info(toPrint)
                        elif diffRel > 0.2:
                            logging.critical(toPrint)
                        elif diffRel > 0.07:
                            logging.error(toPrint)
                        elif isOverWrite:
                            logging.warning(toPrint)
                        elif diffRel > 0.0001:
                            logging.debug(toPrint)
                    else:
                        if ((dateutil.parser.parse(date) < dateMajorPatch) and
                            not(versionKey[0:3] == "ref" or versionKey[0:3] == "old")):
                            logging.info("We do not take this test into account since it concerns an old version of red*.")
                        else:
                            nbNewTests = nbNewTests + 1
                            versionDico["benchs"][testName] = testDico
                            logging.critical(("----------------------------------------------- NEW RESULT:"
                                              "Version %s on test %s. Time: %f, nbExplo: %d.")
                                             % (versionName, testName, time, nbExplo))
            logging.debug("\n")


    print2("\n~~~~~~~~~ Some Stats ~~~~~~~~~\n" +
          "Nb. of Tests: %d. Number of versions: %d. Number of new tests: %d. Number of rewrites: %d." % (nbTests, nbVers, nbNewTests, nbRewrite))

    print2("\n~~~~~~~~~ Results ~~~~~~~~~")
    toPrint = fromVersToTests(VersionsDico, TestsDico)
    logging.debug(toPrint)
    toPrintColor = toPrint
    toPrintColor = toPrintColor.replace(" >", " >" + bcolors.FAIL)
    toPrintColor = toPrintColor.replace("< ", bcolors.ENDC + "< ")
    toPrintColor = toPrintColor.replace("-->", "-->" + bcolors.WARNING)
    toPrintColor = toPrintColor.replace("<--", bcolors.ENDC + "<--")
    toPrintColor = toPrintColor.replace(" [", " [" + bcolors.HEADER)
    toPrintColor = toPrintColor.replace("] ", bcolors.ENDC + "] ")
    toPrintColor = toPrintColor.replace(" . ", bcolors.OKBLUE + " . " + bcolors.ENDC)


    print(toPrintColor)
    print2("Captions: [> X <] if the returned result is false, [.] if is there is no benchmark, [--> t <--] for new tests and [[t]] if test performed in the last 2 hours.")
    logging.error("#" * 80 + "\n")

    dicoFile = open(dicoPath, 'wb')
    marshal.dump(VersionsDico, dicoFile)
    dicoFile.close()

main()
