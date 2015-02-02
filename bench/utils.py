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
import re

import dateutil.parser
from rainbow_logging_handler import RainbowLoggingHandler
from texttable import *
from tabulate import tabulate

import data

# SETTINGS FOR PRETTY PRINTINGS
# First Column' width
firstWidth = 17
# Others Columns' width
width = 13


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
    nb = re.findall(r'\d+', fileName)
    for testKey in dicoTests:
        test = dicoTests[testKey]
        if (test["file"].strip() == fileName.strip()):
            res = test
            resKey = testKey
            return(resKey)
        if (test["file"].strip().replace("XX", str(nb)) == fileName.strip()):
            res = test
            resKey = testKey
            return(resKey)


def printLatexMatrix(matrix):
    return(tabulate(matrix[1:], matrix[0], tablefmt="latex"))

def pprintMatrix(matrix):
    lm = len(matrix[0])-1
    table = Texttable()
    # table.set_cols_align(["l", "r", "c"])
    # table.set_deco(Texttable.HEADER)
    table.set_deco(Texttable.BORDER | Texttable.HEADER)
    table.set_precision(2)
    table.set_cols_width([firstWidth]+ ([width]*lm))
    table.set_cols_align(["l"] + (["c"]*lm))
    table.set_cols_dtype(['t'] +  # text 
                         (['t']*lm)) # automatic
    # table.set_cols_valign(["t", "m", "b"])
    table.add_rows(matrix)
    return(table.draw())

def prettyFloat(f):
    return("%.2E" % f)

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
                    res.append("->" + prettyFloat(versionBenchs[bench]["time"]) + "<-")
                elif not(None == versionBenchs[bench].get("killed")) and versionBenchs[bench]["killed"]:
#                    res.append(">(" + prettyFloat(versionBenchs[bench]["time"]) + ")")
                    res.append(">(NonTerm)")
                elif dateutil.parser.parse(versionBenchs[bench]["date"]) > datetime.now() + timedelta(hours=-2):
                    res.append("[" + prettyFloat(versionBenchs[bench]["time"]) + "]")
                else:
                    res.append(prettyFloat(versionBenchs[bench]["time"]))
                found = True
        if not(found):
            res.append(".")
    return(res)

def cmpGraph(ex1, ex2):
    if "Graph" in ex1 and "Graph" in ex2:
        n1 = (int(ex1.split("Graph_")[1].split("_par")[0]))
        n2 = (int(ex2.split("Graph_")[1].split("_par")[0]))
        return(cmp(n1,n2))
    else:
        return(cmp(ex1,ex2))

def fromVersToTests(dicoVersions, dicoTests, toLatex=False, vers="all"):
    sortedVersions = ['ref', 'old_comp', 'comp_no_impr', 'comp',  'old_red',  'red_no_2', 'red_no_impr', 'red_no_nouse', 'red']
    listTestsKey = sorted(dicoTests.keys(), cmp = cmpGraph)
    listTestsFile = map(lambda x: dicoTests[x]['file'], listTestsKey)
    # first line of the matrix:
    fstLine = [" / "] + sortedVersions
    if vers != "all":
        fstLine = [fstLine[0], fstLine[1], fstLine[4], fstLine[9]]
    matrix = [fstLine]
    for i in range(len(listTestsFile)):
        keyTest = listTestsKey[i]
        fileName = listTestsFile[i]
        listResults = extractResults(dicoVersions, sortedVersions, dicoTests, keyTest)
        if vers=="all":
            matrix.append(listResults)
        else:
            matrix.append([listResults[0], listResults[1], listResults[4], listResults[9]])                      
    if toLatex:
        return(printLatexMatrix(matrix))
    else:
        return(pprintMatrix(matrix))

def setNoNew(dico):
    for versKey in dico:
        for testKey in dico[versKey]["benchs"]:
            dico[versKey]["benchs"][testKey]["new"] = False
    return(dico)

def filterData(path, dico):
    fileName = path.split("/")[-1]
    resKey = findTest(fileName, dico)
    if resKey == None or resKey == "":
        return(False)
    else:
        return(True)
