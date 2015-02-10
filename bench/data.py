#!/usr/bin/python
# -*- coding: iso-8859-1 -*-

DICO = {
    'ref' : {
        "name" : "Apte without POR (reference version)",
        "call" : "apte",
        "branch" : "",
        "benchs": {
            "TEST": {
                "new" : False,
                "file": "TEST.txt",
                "res" : True,
                "date" : "1263",
                "time": "453",
                "nbExplo" : "4674",
                "fileFrom" : "BENCH.log"
            }
        }
    },
    'comp' : {
        "name" : "Compression (+ killing improper)",
        "call" : "apte -with_por compr improper",
        "branch" : "",
        "benchs": {}
    },
    'red' : {
        "name" : "Reduction (+ killing improper + NoUse criterion)",
        "call" : "apte -with_por red improper nouse",
        "branch" : "",
        "benchs": {}
    },
}

TESTS = {
    'Yahalom-stef-6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'yahalom-s-6.txt',
        'cat' : 9,
    },
    'Yahalom-stef-modif' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'yahalom-s-modified.txt',
        'cat' : 9,
    },

    'Yahalom-3' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-3.txt',
        'cat' : 9,
    },
    'Yahalom-4' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-4.txt',
        'cat' : 9,
    },
    'Yahalom-5' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-5.txt',
        'cat' : 9,
    },
    'Yahalom-6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-6.txt',
        'cat' : 9,
    },
    'Yahalom-7' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-7.txt',
        'cat' : 9,
    },
    'Yahalom-8' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-8.txt',
        'cat' : 9,
    },
    'Yahalom-9' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'Yahalom-shared-key-9.txt',
        'cat' : 9,
    },

    'PrivateAuth-2' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-2.txt',
        'cat' : 9,
    },
    'PrivateAuth-3' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-3.txt',
        'cat' : 9,
    },
    'PrivateAuth-4' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-4.txt',
        'cat' : 9,
    },
    'PrivateAuth-5' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-5.txt',
        'cat' : 9,
    },
    'PrivateAuth-6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-6.txt',
        'cat' : 9,
    },
    'PrivateAuth-7' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-7.txt',
        'cat' : 9,
    },
    'PrivateAuth-8' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-8.txt',
        'cat' : 9,
    },
    'PrivateAuth-9' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-9.txt',
        'cat' : 9,
    },

    'PrivateAuth-5+' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-5+.txt',
        'cat' : 9,
    },
    'PrivateAuth-6-' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-6-.txt',
        'cat' : 9,
    },
    'PrivateAuth-4-diff' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-4-diff.txt',
        'cat' : 9,
    },
    'PrivateAuth-6-diff' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'PrivateAuth-pub-key-6-diff.txt',
        'cat' : 9,
    },

    'DS-Shared-3' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-3.txt',
        'cat' : 9,
    },
    'DS-Shared-4' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-4.txt',
        'cat' : 9,
    },
    'DS-Shared-5' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-5.txt',
        'cat' : 9,
    },
    'DS-Shared-6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-6.txt',
        'cat' : 9,
    },
    'DS-Shared-7' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-7.txt',
        'cat' : 9,
    },
    'DS-Shared-8' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-8.txt',
        'cat' : 9,
    },
    'DS-Shared-9' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-9.txt',
        'cat' : 9,
    },
    'DS-Shared-10' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-10.txt',
        'cat' : 9,
    },
    'DS-Shared-11' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-11.txt',
        'cat' : 9,
    },
    'DS-Shared-12' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'DS-shared-key-12.txt',
        'cat' : 9,
    },

    'NS-Shared_diff_4' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-4-diff.txt',
        'cat' : 9,
    },
    'NS-Shared_diff_5' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-5-diff.txt',
        'cat' : 9,
    },
    'NS-Shared_diff_6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-6-diff.txt',
        'cat' : 9,
    },
    'NS-Shared_diff_7' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-7-diff.txt',
        'cat' : 9,
    },
    'NS-Shared_diff_8' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-8-diff.txt',
        'cat' : 9,
    },
    'NS-Shared_diff_9' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'NS-shared-key-9-diff.txt',
        'cat' : 9,
    },

    'WMF_SS_diff_4' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-4-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_5' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-5-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_6' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-6-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_7' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-7-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_8' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-8-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_9' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-9-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_10' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-10-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_11' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-11-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_12' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-12-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_13' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-13-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_14' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-14-diff.txt',
        'cat' : 9,
    },
    'WMF_SS_diff_15' : {
        'res' : True,
        'name' : "TODO",
        'file' : 'WMF-shared-key-15-diff.txt',
        'cat' : 9,
    },

    

    'WMF_SS_3' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 3 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-3.txt', 
        'cat' : 3,
    },
    'WMF_SS_4' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 4 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-4.txt',
        'cat' : 3,
    },
    'WMF_SS_5' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 5 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-5.txt',
        'cat' : 3,
    },
    'WMF_SS_6' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 6 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-6.txt',
        'cat' : 3,
    },
    'WMF_SS_7' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 7 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-7.txt',
        'cat' : 3,
    },
    'WMF_SS_8' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 8 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-8.txt',
        'cat' : 3,
    },
    'WMF_SS_9' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 9 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-9.txt',
        'cat' : 3,
    },
    'WMF_SS_10' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 10 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-10.txt',
        'cat' : 3,
    },
    'WMF_SS_11' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 11 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-11.txt',
        'cat' : 3,
    },
    'WMF_SS_12' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 12 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-12.txt',
        'cat' : 3,
    },
    'WMF_SS_13' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 13 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-13.txt',
        'cat' : 3,
    },
    'WMF_SS_14' : {
        'res' : True,
        'name' : "Wide Mouth Frog: Strong secrecy of the shared key kab - 14 agents among [Alice | Server | Bob]",
        'file' : 'WMF-shared-key-14.txt',
        'cat' : 3,
    },
    'NS_SharedK_3' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 3 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-3.txt',
        'cat' : 3,
    },
    'NS_SharedK_4' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 4 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-4.txt',
        'cat' : 3,
    },
    'NS_SharedK_5' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 5 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-5.txt',
        'cat' : 3,
    },
    'NS_SharedK_6' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 6 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-6.txt',
        'cat' : 3,
    },
    'NS_SharedK_7' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 7 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-7.txt',
        'cat' : 3,
    },
    'NS_SharedK_8' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 8 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-8.txt',
        'cat' : 3,
    },
    'NS_SharedK_9' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Strong Secrecy of shared key - 9 agents among [Alice | Server | Bob]",
        'file' : 'NS-shared-key-9.txt',
        'cat' : 3,
    },
    'AKA_3G_s__2' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 2 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-2.txt',
        'cat' : 3,
    },
    'AKA_3G_s__4' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 4 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-4.txt',
        'cat' : 3,
    },
    'AKA_3G_s__6' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 6 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-6.txt',
        'cat' : 3,
    },
    'AKA_3G_s__8' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 8 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-8.txt',
        'cat' : 3,
    },
    'AKA_3G_s__10' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 10 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-10.txt',
        'cat' : 3,
    },
    'AKA_3G_s__12' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 12 agent among [Mobile Station, Network]",
        'file' : '3G-AKA-s-12.txt',
        'cat' : 3,
    },

    'AKA_3G_2' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 2 agent among [Mobile Station, Network]",
        'file' : '3G_PPAuthentication_sec_2.txt',
        'cat' : 3,
    },
    'AKA_3G_3' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 3 agent among [Mobile Station, Network]",
        'file' : '3G_PPAuthentication_sec_3.txt',
        'cat' : 3,
    },
    'AKA_3G_4' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 4 agent among [Mobile Station, Network]",
        'file' : '3G_PPAuthentication_sec_4.txt',
        'cat' : 3,
    },
    'AKA_3G_5' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 5 agent among [Mobile Station, Network]",
        'file' : '3G_PPAuthentication_sec_5.txt',
        'cat' : 3,
    },
    'AKA_3G_6' : {
        'res' : True,
        'name' : "3G AKA protocol: we test strong secrecy of the agreed key - 6 agent among [Mobile Station, Network]",
        'file' : '3G_PPAuthentication_sec_6.txt',
        'cat' : 3,
    },
     'Pass-PA_ano_2' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 2 agents among [Reader, Passport]',
        'file' : 'PA_ano_2.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_3' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 3 agents among [Reader, Passport]',
        'file' : 'PA_ano_3.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_4' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 4 agents among [Reader, Passport]',
        'file' : 'PA_ano_4.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_5' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 5 agents among [Reader, Passport]',
        'file' : 'PA_ano_5.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_6' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 6 agents among [Reader, Passport]',
        'file' : 'PA_ano_6.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_7' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 7 agents among [Reader, Passport]',
        'file' : 'PA_ano_7.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_8' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 8 agents among [Reader, Passport]',
        'file' : 'PA_ano_8.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_9' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 9 agents among [Reader, Passport]',
        'file' : 'PA_ano_9.txt',
        'cat' : 2,
    },
     'Pass-PA_ano_10' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport - 10 agents among [Reader, Passport]',
        'file' : 'PA_ano_10.txt',
        'cat' : 2,
    },

'Bench_Graph_1' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 1 sessions of very simple processes',
        'file' : 'Simple_1_par.txt',
        'cat' : 4,
    },
'Bench_Graph_2' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 2 sessions of very simple processes',
        'file' : 'Simple_2_par.txt',
        'cat' : 4,
    },
'Bench_Graph_3' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 3 sessions of very simple processes',
        'file' : 'Simple_3_par.txt',
        'cat' : 4,
    },
'Bench_Graph_4' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 4 sessions of very simple processes',
        'file' : 'Simple_4_par.txt',
        'cat' : 4,
    },
'Bench_Graph_5' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 5 sessions of very simple processes',
        'file' : 'Simple_5_par.txt',
        'cat' : 4,
    },
'Bench_Graph_6' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 6 sessions of very simple processes',
        'file' : 'Simple_6_par.txt',
        'cat' : 4,
    },
'Bench_Graph_7' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 7 sessions of very simple processes',
        'file' : 'Simple_7_par.txt',
        'cat' : 4,
    },
'Bench_Graph_8' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 8 sessions of very simple processes',
        'file' : 'Simple_8_par.txt',
        'cat' : 4,
    },
'Bench_Graph_9' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 9 sessions of very simple processes',
        'file' : 'Simple_9_par.txt',
        'cat' : 4,
    },
'Bench_Graph_10' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 10 sessions of very simple processes',
        'file' : 'Simple_10_par.txt',
        'cat' : 4,
    },
'Bench_Graph_11' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 11 sessions of very simple processes',
        'file' : 'Simple_11_par.txt',
        'cat' : 4,
    },
'Bench_Graph_12' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 12 sessions of very simple processes',
        'file' : 'Simple_12_par.txt',
        'cat' : 4,
    },
'Bench_Graph_13' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 13 sessions of very simple processes',
        'file' : 'Simple_13_par.txt',
        'cat' : 4,
    },
'Bench_Graph_14' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 14 sessions of very simple processes',
        'file' : 'Simple_14_par.txt',
        'cat' : 4,
    },
'Bench_Graph_15' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 15 sessions of very simple processes',
        'file' : 'Simple_15_par.txt',
        'cat' : 4,
    },
'Bench_Graph_16' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 16 sessions of very simple processes',
        'file' : 'Simple_16_par.txt',
        'cat' : 4,
    },
'Bench_Graph_17' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 17 sessions of very simple processes',
        'file' : 'Simple_17_par.txt',
        'cat' : 4,
    },
'Bench_Graph_18' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 18 sessions of very simple processes',
        'file' : 'Simple_18_par.txt',
        'cat' : 4,
    },
'Bench_Graph_19' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 19 sessions of very simple processes',
        'file' : 'Simple_19_par.txt',
        'cat' : 4,
    },
'Bench_Graph_20' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 20 sessions of very simple processes',
        'file' : 'Simple_20_par.txt',
        'cat' : 4,
    },
'Bench_Graph_21' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 21 sessions of very simple processes',
        'file' : 'Simple_21_par.txt',
        'cat' : 4,
    },
'Bench_Graph_22' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 22 sessions of very simple processes',
        'file' : 'Simple_22_par.txt',
        'cat' : 4,
    },
'Bench_Graph_23' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 23 sessions of very simple processes',
        'file' : 'Simple_23_par.txt',
        'cat' : 4,
    },
'Bench_Graph_24' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 24 sessions of very simple processes',
        'file' : 'Simple_24_par.txt',
        'cat' : 4,
    },
}

def get_versDico():
    return(DICO)

def get_testsDico():
    return(TESTS)
