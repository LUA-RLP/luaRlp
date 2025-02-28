---
title: "Input data for this App"
output: html_document
date: "2025-02-28"
editor_options: 
  markdown: 
    wrap: 72
---

# About This App

This Shiny app is designed for evaluating qPCR data. It currently only
works for SURE COV-2 and INF A/B and RSV data, which comes in two files:

## Data Input

Both files should be stored in and read from
`O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/`

## How these CSVs are generated

Raw Bio-Rad qPCR files are found in:
`H:\Analysenetz\NETLDAS0004_01\PCR Ergebnisse` and/or
`H:\Analysenetz\NETLDAS0004\PCR Läufe\PCR Ergebnisse`. Open them (one
after the other) with Bio-Rad CFX manager Software. Select "Export \>
Custom Export..." Like this:. ![Figure 1](figure1.png)

Then leave all the settings at default values. This should look like
this:

![Figure 2](figure2.png)

Remember to save the CSVs at
`O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/`!

## How to Use

1.  Navigate to the "Upload & Table" tab.
2.  Select and upload your qPCR result files (one for COV-2, one for
    Flu).
3.  View and evaluate the results of the qPCR.
