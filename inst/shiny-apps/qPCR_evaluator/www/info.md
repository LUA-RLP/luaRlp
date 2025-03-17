---
title: "Input data for this App"
output: html_document
date: "2025-02-28"
editor_options: 
  markdown: 
    wrap: 72
---

# **Über diese App**

Diese Shiny-App dient zur Auswertung von qPCR-Daten. Derzeit unterstützt
sie entweder eine generelle Porzessierung ohne spezielle
Spalten-Namen-Sortierung oder eine angepasste Sortierung für SURE COV-2,
INF A/B und RSV. Die Daten können in einer Datei oder in zwei separaten
Dateien vorliegen. Liegen die Daten in zwei Dateien vor, können sie (für
passende Probennamen) zusammengeführt werden.

### **Dateninput**

Dateien müssen im folgenden Verzeichnis gespeichert und von dort
eingelesen werden:\
📂 **O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/**

### **Erstellung der CSV-Dateien**

Die Rohdaten der Bio-Rad qPCR befinden sich in:\
📂 **H:\\Analysenetz\\NETLDAS0004_01\\PCR Ergebnisse**\
📂 **H:\\Analysenetz\\NETLDAS0004\\PCR Läufe\\PCR Ergebnisse**

Um die Daten zu exportieren:

1.  Öffnen Sie die Dateien **nacheinander** mit der **Bio-Rad CFX
    Manager Software**.

2.  Wählen Sie **"Export \> Custom Export..."**

3.  Konfigurieren Sie den Export entsprechend (Einstellungen wie
    abgebildet sollten Standard sein).

4.  Speicher unter 📂 **O:/Abteilung Humanmedizin (AHM)/Referat
    32/32_6/qPCR_CSVs/**

![Figure 1](figure1.png)

Then leave all the settings at default values. This should look like
this:

![](figure2.png)

### **Anleitung zur Nutzung**

### 🔬 **qPCR-Datenanalyse – Anleitung**

🔹 **So gehen Sie vor:**

🔷 **1.** Entscheiden Sie, ob Sie **eine oder zwei Dateien** hochladen
möchten.\
*Zwei Dateien mit übereinstimmenden Probennamen werden zusammengeführt.*

🔷 **2.** Wählen Sie Ihre **qPCR-Ergebnisdateien** aus und laden Sie sie
hoch (**eine oder zwei Dateien**).

🔷 **3.** Wählen Sie die **Spaltenauswahl**:\
- **Generelle Spalten** *(Standardverarbeitung)*\
- **Spezielle Cov-Flu-Sortierung** *(entsprechend der Prüfberichte)*

🔷 **4.** Drücken Sie **"Daten verarbeiten"**.

🔷 **5.** Sehen Sie sich die Ergebnisse an und werten Sie die
**qPCR-Daten** aus.

📌 **Hinweis:** Falls Sie Fragen haben oder Unterstützung benötigen,
wenden Sie sich bitte an die **Prüfleitung (Heitlinger, Holländer)**. 🚀
