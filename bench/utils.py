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

def extractBench(text):
    lastBench = text.split("=============== STARTING A NEW BENCHMARK ===============")[-1]
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
    if "_4_" in call:
        call = call.replace("_4_red_fix", "_1_new_all")
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
    width = 14
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
                elif versionBenchs[bench]["new"]:
                    res.append("-->" + str(versionBenchs[bench]["time"]) + "<--")
                elif dateutil.parser.parse(versionBenchs[bench]["date"]) > datetime.now() + timedelta(hours=-2):
                    res.append("[" + str(versionBenchs[bench]["time"]) + "]")
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

def setNoNew(dico):
    for versKey in dico:
        for testKey in dico[versKey]["benchs"]:
            dico[versKey]["benchs"][testKey]["new"] = False
    return(dico)
