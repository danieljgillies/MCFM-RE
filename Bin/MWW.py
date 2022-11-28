#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 27 10:57:15 2022

@author: dg376
"""


import matplotlib.pyplot as plt

import numpy as np

import pandas as pd

import matplotlib.gridspec as gridspec


# Program One

# Pandas read file
MCFM_o6_MWW_nocuts_zerofive = pd.read_excel(
    "o6_nocuts_mWW_zerofive.xlsx",
    header=None,
    names=[
        "mWW_1",
        "xsec_1",
        "err_xsec_1",
        "mWW_2",
        "xsec_2",
        "err_xsec_2",
        "mWW_3",
        "xsec_3",
        "err_xsec_3",
        "mWW_4",
        "xsec_4",
        "err_xsec_4",
        "mWW_5",
        "xsec_5",
        "err_xsec_5",
    ],
)


num = 5
MCFM_o6_MWW_nocuts_zerofive_average = (
    MCFM_o6_MWW_nocuts_zerofive["xsec_1"]
    + MCFM_o6_MWW_nocuts_zerofive["xsec_2"]
    + MCFM_o6_MWW_nocuts_zerofive["xsec_3"]
    + MCFM_o6_MWW_nocuts_zerofive["xsec_4"]
    + MCFM_o6_MWW_nocuts_zerofive["xsec_5"]
) / num
MCFM_o6_MWW_nocuts_zerofive_err = []
for i in range(0, len(MCFM_o6_MWW_nocuts_zerofive["xsec_1"])):
    MCFM_o6_MWW_nocuts_zerofive_err.append(
        np.std(
            [
                MCFM_o6_MWW_nocuts_zerofive["xsec_1"][i],
                MCFM_o6_MWW_nocuts_zerofive["xsec_2"][i],
                MCFM_o6_MWW_nocuts_zerofive["xsec_3"][i],
                MCFM_o6_MWW_nocuts_zerofive["xsec_4"][i],
                MCFM_o6_MWW_nocuts_zerofive["xsec_5"][i],
            ]
        )
        / np.sqrt(num)
    )
MCFM_o6_MWW_nocuts_zerofive_err = np.array(MCFM_o6_MWW_nocuts_zerofive_err)

MCFM_higgs_MWW_nocuts_zerofive = pd.read_excel(
    "heavyhiggs_nocuts_mWW_zerofive.xlsx",
    header=None,
    names=[
        "mWW_1",
        "xsec_1",
        "err_xsec_1",
        "mWW_2",
        "xsec_2",
        "err_xsec_2",
        "mWW_3",
        "xsec_3",
        "err_xsec_3",
        "mWW_4",
        "xsec_4",
        "err_xsec_4",
        "mWW_5",
        "xsec_5",
        "err_xsec_5",
    ],
)
num = 5
MCFM_higgs_MWW_nocuts_zerofive_average = (
    MCFM_higgs_MWW_nocuts_zerofive["xsec_1"]
    + MCFM_higgs_MWW_nocuts_zerofive["xsec_2"]
    + MCFM_higgs_MWW_nocuts_zerofive["xsec_3"]
    + MCFM_higgs_MWW_nocuts_zerofive["xsec_4"]
    + MCFM_higgs_MWW_nocuts_zerofive["xsec_5"]
) / num
MCFM_higgs_MWW_nocuts_zerofive_err = []
for i in range(0, len(MCFM_higgs_MWW_nocuts_zerofive["xsec_1"])):
    MCFM_higgs_MWW_nocuts_zerofive_err.append(
        np.std(
            [
                MCFM_higgs_MWW_nocuts_zerofive["xsec_1"][i],
                MCFM_higgs_MWW_nocuts_zerofive["xsec_2"][i],
                MCFM_higgs_MWW_nocuts_zerofive["xsec_3"][i],
                MCFM_higgs_MWW_nocuts_zerofive["xsec_4"][i],
                MCFM_higgs_MWW_nocuts_zerofive["xsec_5"][i],
            ]
        )
        / np.sqrt(num)
    )
MCFM_higgs_MWW_nocuts_zerofive_err = np.array(MCFM_higgs_MWW_nocuts_zerofive_err)


# Program Two
# Data below in a numppy array

xData = np.array(
    [
        10.0,
        30.0,
        50.0,
        70.0,
        90.0,
        110.0,
        130.0,
        150.0,
        170.0,
        190.0,
        210.0,
        230.0,
        250.0,
        270.0,
        290.0,
        310.0,
        330.0,
        350.0,
        370.0,
        390.0,
        410.0,
        430.0,
        450.0,
        470.0,
        490.0,
        510.0,
        530.0,
        550.0,
        570.0,
        590.0,
        610.0,
        630.0,
        650.0,
        670.0,
        690.0,
        710.0,
        730.0,
        750.0,
        770.0,
        790.0,
        810.0,
        830.0,
        850.0,
        870.0,
        890.0,
        910.0,
        930.0,
        950.0,
        970.0,
        990.0,
        1010.0,
        1030.0,
        1050.0,
        1070.0,
        1090.0,
        1110.0,
        1130.0,
        1150.0,
        1170.0,
        1190.0,
        1210.0,
        1230.0,
        1250.0,
        1270.0,
        1290.0,
        1310.0,
        1330.0,
        1350.0,
        1370.0,
        1390.0,
        1410.0,
        1430.0,
        1450.0,
        1470.0,
        1490.0,
        1510.0,
        1530.0,
        1550.0,
        1570.0,
        1590.0,
        1610.0,
        1630.0,
        1650.0,
        1670.0,
        1690.0,
        1710.0,
        1730.0,
        1750.0,
        1770.0,
        1790.0,
        1810.0,
        1830.0,
        1850.0,
        1870.0,
        1890.0,
        1910.0,
        1930.0,
        1950.0,
        1970.0,
    ]
)
mg_o6_MWW_nocuts_zerofive_1 = np.array(
    [
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.000589245671492306,
        0.013035069886932142,
        0.029502093971302875,
        0.051439552565389485,
        0.07906243781146431,
        0.11542845017774402,
        0.15564845094678906,
        0.21008999342527174,
        0.27271721746933675,
        0.3464923525958879,
        0.4292495772237658,
        0.5223026044433616,
        0.6296966118625306,
        0.7429511776787742,
        0.8672182786285612,
        1.0096410604892072,
        1.1585525443060154,
        1.3290832808461728,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
    ]
)


num = 1
mg_o6_MWW_nocuts_zerofive_average = mg_o6_MWW_nocuts_zerofive_1 / 10
# mg_o6_MWW_nocuts_zerofive_average=(mg_o6_MWW_nocuts_zerofive_1+mg_o6_MWW_nocuts_zerofive_2+mg_o6_MWW_nocuts_zerofive_3)/num
mg_o6_MWW_nocuts_zerofive_err = 0
# for i in range(0,len(mg_o6_MWW_nocuts_zerofive_1)):
#    mg_o6_MWW_nocuts_zerofive_err.append(np.std([mg_o6_MWW_nocuts_zerofive_1[i],mg_o6_MWW_nocuts_zerofive_2[i],mg_o6_MWW_nocuts_zerofive_3[i]])/np.sqrt(num))
# mg_o6_MWW_nocuts_zerofive_err=np.array(MCFM_o6_MWW_nocuts_zerofive_err)

total_err = (
    (mg_o6_MWW_nocuts_zerofive_err / mg_o6_MWW_nocuts_zerofive_average) ** 2
    + (MCFM_o6_MWW_nocuts_zerofive_err / MCFM_o6_MWW_nocuts_zerofive_average) ** 2
) ** 0.5


# fac=(4.2*(10**-4))*20*(1000)*(1/(3.66*(10**-3)))*(1/np.pi)*(10/2)
# fac=(4.29*(10**-3))*(1/(3.66*(10**-3)))*1000
fac = 20

print(np.sum(20 * MCFM_o6_MWW_nocuts_zerofive_average))
print(np.sum(mg_o6_MWW_nocuts_zerofive_average / 10))
plt.figure()
plt.errorbar(
    MCFM_o6_MWW_nocuts_zerofive["mWW_1"],
    fac * MCFM_o6_MWW_nocuts_zerofive_average,
    xerr=10,
    yerr=fac * MCFM_o6_MWW_nocuts_zerofive_err,
    fmt=".",
    label="MCFM-RE",
)

plt.errorbar(
    MCFM_higgs_MWW_nocuts_zerofive["mWW_1"],
    fac * MCFM_higgs_MWW_nocuts_zerofive_average,
    xerr=10,
    yerr=fac * MCFM_higgs_MWW_nocuts_zerofive_err,
    fmt=".",
    label="MCFM-RE",
)


plt.errorbar(
    xData,
    mg_o6_MWW_nocuts_zerofive_average,
    xerr=10,
    yerr=mg_o6_MWW_nocuts_zerofive_err,
    fmt=".",
    label="MG",
)
plt.semilogy()
plt.figure()
plt.errorbar(
    MCFM_o6_MWW_nocuts_zerofive["mWW_1"],
    (fac) * MCFM_o6_MWW_nocuts_zerofive_average / mg_o6_MWW_nocuts_zerofive_average,
    xerr=10,
    yerr=(total_err * fac)
    * MCFM_o6_MWW_nocuts_zerofive_average
    / mg_o6_MWW_nocuts_zerofive_average,
    fmt=".",
    label="MCFM-RE/MG",
)
plt.xlabel("mWW")
plt.semilogy()

plt.show()
