#!/usr/bin/python
# -*- coding: iso-8859-1 -*-

DICO = {
    'ref' : {
        "name" : "Apte without POR (reference version)",
        "call" : "apte_1_new_all",
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

TESTS = {
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
        'file' : 'Simple_Benchmarks_Graph_1_par.txt',
        'cat' : 4,
    },
'Bench_Graph_2' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 2 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_2_par.txt',
        'cat' : 4,
    },
'Bench_Graph_3' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 3 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_3_par.txt',
        'cat' : 4,
    },
'Bench_Graph_4' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 4 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_4_par.txt',
        'cat' : 4,
    },
'Bench_Graph_5' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 5 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_5_par.txt',
        'cat' : 4,
    },
'Bench_Graph_6' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 6 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_6_par.txt',
        'cat' : 4,
    },
'Bench_Graph_7' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 7 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_7_par.txt',
        'cat' : 4,
    },
'Bench_Graph_8' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 8 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_8_par.txt',
        'cat' : 4,
    },
'Bench_Graph_9' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 9 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_9_par.txt',
        'cat' : 4,
    },
'Bench_Graph_10' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 10 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_10_par.txt',
        'cat' : 4,
    },
'Bench_Graph_11' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 11 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_11_par.txt',
        'cat' : 4,
    },
'Bench_Graph_12' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 12 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_12_par.txt',
        'cat' : 4,
    },
'Bench_Graph_13' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 13 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_13_par.txt',
        'cat' : 4,
    },
'Bench_Graph_14' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 14 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_14_par.txt',
        'cat' : 4,
    },
'Bench_Graph_15' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 15 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_15_par.txt',
        'cat' : 4,
    },
'Bench_Graph_16' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 16 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_16_par.txt',
        'cat' : 4,
    },
'Bench_Graph_17' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 17 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_17_par.txt',
        'cat' : 4,
    },
'Bench_Graph_18' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 18 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_18_par.txt',
        'cat' : 4,
    },
'Bench_Graph_19' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 19 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_19_par.txt',
        'cat' : 4,
    },
'Bench_Graph_20' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 20 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_20_par.txt',
        'cat' : 4,
    },
'Bench_Graph_21' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 21 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_21_par.txt',
        'cat' : 4,
    },
'Bench_Graph_22' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 22 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_22_par.txt',
        'cat' : 4,
    },
'Bench_Graph_23' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 23 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_23_par.txt',
        'cat' : 4,
    },
'Bench_Graph_24' : {
        'res' : True,
        'name' : 'Benchmarks for POR - 24 sessions of very simple processes',
        'file' : 'Simple_Benchmarks_Graph_24_par.txt',
        'cat' : 4,
    },


    # TO REMOVE!
    'NSL_L_1-old' : {
        'res' : True,
        'name' : "Needham Shroeder Love Public Key: Strong secrecy of the nonce N_b (Bob's nonce) - 1 session of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Schroeder_Love_strongSec_1s.txt',
        'cat' : 3,
    },
    'NSL_L_2-old' : {
        'res' : True,
        'name' : "Needham Shroeder Love Public Key: Strong secrecy of the nonce N_b (Bob's nonce) - 2 sessions of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Schroeder_Love_strongSec_2s.txt',
        'cat' : 3,
    },
    'NS_Sym_1s-old' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Authentication of Bob - 1 session of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Shroeder_sym_auth_1s.txt',
        'cat' : 3,
    },
    'NS_Sym_2s-old' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Authentication of Bob - 2 sessions of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Shroeder_sym_auth_2s.txt',
        'cat' : 3,
    },
    'NS_Sym_3s-old' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Authentication of Bob - 3 sessions of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Shroeder_sym_auth_3s.txt',
        'cat' : 3,
    },
    'NS_Sym_4s-old' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Authentication of Bob - 4 sessions of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Shroeder_sym_auth_4s.txt',
        'cat' : 3,
    },
    'NS_Sym_5s-old' : {
        'res' : True,
        'name' : "Needham Shroeder Symmetric: Authentication of Bob - 5 sessions of (Alice | Server | Bob)",
        'file' : 'Auth_Needham_Shroeder_sym_auth_5s.txt',
        'cat' : 3,
    },
    'WMF_1s_old' : {
        'res' : True,
        'name' : 'Wide Mouth Frog - 1 session - authentfication',
        'file' : 'Simple_wmf_1s.txt',
        'cat' : 1,
    },
    'WMF_2s_old' : {
        'res' : True,
        'name' : 'Wide Mouth Frog - 2 sessions - authentfication',
        'file' : 'Simple_wmf_2s.txt',
        'cat' : 1,
    },
    'WMF_3s_old' : {
        'res' : True,
        'name' : 'Wide Mouth Frog - 3 sessions - authentfication',
        'file' : 'Simple_wmf_3s.txt',
        'cat' : 1,
    },
    'AS_RPC_a_old' : {
        'res' : False,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_auth.txt',
    },
    'AS_RPC_BAN_auth_old' : {
        'res' : False,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_BAN_auth.txt',
    },
    'AS_RPC_BAN_sec_old' : {
        'res' : True,
        'name' : 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_BAN_sec.txt',
    },
    'AS_RPC_sec_old' : {
        'res' : True,
        'name': 'Andrew Secure RPC - auth',
        'file' : 'Simple_Andrew_Secure_RPC_sec.txt',
    },
    'BAC_FR_old' : {
        'res' : False,
        'name': 'Basic Accress Control FR',
        'file' : 'Simple_Basic_Access_Control_protocol_FR.txt',
    },
    'BAC_UK_old' : {
        'res' : False,
        'name' : 'Basic Accress Control UK',
        'file' : 'Simple_Basic_Access_Control_protocol_UK.txt',
    },
    'Bench_10_old' : {
        'res' : True,
        'name': 'Many parallels - 10',
        'file' : 'Simple_bench_10par.txt',
    },
    'Bench_3_old' : {
        'res' : True,
        'name' : 'Many parallels - 3',
        'file' : 'Simple_bench_3par.txt',
    },
    'Bench_5_old' : {
        'res' : True,
        'name' : 'Many parallels - 5',
        'file' : 'Simple_bench_5par.txt',
    },
    'Bench_7_old' : {
        'res' : True,
        'name' : 'Many parallels - 7',
        'file' : 'Simple_bench_7par.txt',
    },
    'Passport_PA_old' : {
        'res' : True,
        'name': 'Private Authentification - Unlinkability',
        'file' : 'Simple_Passport_PA_Unlinkability.txt',
    },
    'PA_2_old' : {
        'res' : True,
        'name': 'Private Authentification - 2 sessions',
        'file' : 'Simple_Private_Authentication_Protocol_2_sessions.txt',
    },
    'PA_3_old' : {
        'res' : True,
        'name': 'Private Authentification - 3 sessions',
        'file' : 'Simple_Private_Authentication_Protocol_3_sessions.txt',
    },
    'PA_1_old' : {
        'res' : True,
        'name': 'Private Authentification - 1 session',
        'file' : 'Simple_Private_Authentication_Protocol.txt',
    },
    'Bench_C_4_old' : {
        'res' : True,
        'name' : 'Many parallels, complex tests - 4',
        'file' : 'Simple_tests_h_4par.txt',
    },
    'Bench_C_7_old' : {
        'res' : True,
        'name' : 'Many parallels, complex tests - 7',
        'file' : 'Simple_tests_h_7par.txt',
    },

   'Pass_PA_ano_0_old' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport + 0 symmetric session P+R',
        'file' : 'Passport_PassiveAuthentification_ano_0s.txt',
        'cat' : 2,
    },
    'Pass_PA_ano_1_old' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport + 1 symmetric session P+R',
        'file' : 'Passport_PassiveAuthentification_ano_1s.txt',
        'cat' : 2,
    },
    'Pass_PA_ano_2_old' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport + 2 symmetric session P+R',
        'file' : 'Passport_PassiveAuthentification_ano_2s.txt',
        'cat' : 2,
    },
    'Pass_PA_ano_3_old' : {
        'res' : True,
        'name' : 'Passport: Passive Authentification - Anonymity of one Passport + 3 symmetric session P+R',
        'file' : 'Passport_PassiveAuthentification_ano_3s.txt',
        'cat' : 2,
    },
 
}

def get_versDico():
    return(DICO)

def get_testsDico():
    return(TESTS)
