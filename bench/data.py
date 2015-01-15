#!/usr/bin/python
# -*- coding: iso-8859-1 -*-

TESTS = {
    'AS_RPC_a' : {
        'res' : False,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_auth.txt',
    },
    'AS_RPC_BAN_auth' : {
        'res' : False,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_BAN_auth.txt',
    },
    'AS_RPC_BAN_sec' : {
        'res' : True,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_BAN_sec.txt',
    },
    'AS_RPC_sec' : {
        'res' : True,
        'name': 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_sec.txt',
    },
    'BAC_FR' : {
        'res' : False,
        'name': 'Basic Accress Control FR',
        'file' : 'Simple_Basic_Access_Control_protocol_FR.txt',
    },
    'BAC_UK' : {
        'res' : False,
        'name' : 'Basic Accress Control UK',
        'file' : 'Simple_Basic_Access_Control_protocol_UK.txt',
    },
    'Bench_10' : {
        'res' : True,
        'name': 'Many parallels - 10',
        'file' : 'Simple_bench_10par.txt',
    },
    'Bench_3' : {
        'res' : True,
        'name' : 'Many parallels - 3',
        'file' : 'Simple_bench_3par.txt',
    },
    'Bench_5' : {
        'res' : True,
        'name' : 'Many parallels - 5',
        'file' : 'Simple_bench_5par.txt',
    },
    'Bench_7' : {
        'res' : True,
        'name' : 'Many parallels - 7',
        'file' : 'Simple_bench_7par.txt',
    },
    'Passport_PA' : {
        'res' : True,
        'name': 'Private Authentification - Unlinkability',
        'file' : 'Simple_Passport_PA_Unlinkability.txt',
    },
    'PA_2' : {
        'res' : True,
        'name': 'Private Authentification - 2 sessions',
        'file' : 'Simple_Private_Authentication_Protocol_2_sessions.txt',
    },
    'PA_3' : {
        'res' : True,
        'name': 'Private Authentification - 3 sessions',
        'file' : 'Simple_Private_Authentication_Protocol_3_sessions.txt',
    },
    'PA_1' : {
        'res' : True,
        'name': 'Private Authentification - 1 session',
        'file' : 'Simple_Private_Authentication_Protocol.txt',
    },
    'Bench_C_4' : {
        'res' : True,
        'name' : 'Many parallels, complex tests - 4',
        'file' : 'Simple_tests_h_4par.txt',
    },
    'Bench_C_7' : {
        'res' : True,
        'name' : 'Many parallels, complex tests - 7',
        'file' : 'Simple_tests_h_7par.txt',
    },
    'WMF' : {
        'res' : True,
        'name' : 'Wide Mouth Frog - Strong secrecy',
        'file' : 'Simple_wmf.txt',
    },
}

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
    'comp_no_impr' : {
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
    'red_no_impr' : {
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
    'red_no_2' : {
        "name" : "Reduction (- killing improper - NoUse criterion)",
        "call" : "apte_1_new_all -with_por red",
        "branch" : "",
        "benchs": {}
    }
}

def get_versDico():
    return(DICO)

def get_testsDico():
    return(TESTS)
