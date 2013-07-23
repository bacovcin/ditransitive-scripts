ditransitive-scripts
====================

Scripts related to the study of ditransitive sentences (especially in English)

1) Run Full.q on all of the English corpora (PPCME2,PCEEC,PPCEME, and PPCMBE), except for YCOE

2) Run the RemoveDup.py from my corpus-tools repository on Full.out

3) Run the external.c coding query from my corpus-tools repository on Full.psd

4) Run Verbs.c on external.cod

5) Run OldEngAct.q on all of the psd files from YCOE

6) Run OldEngPas.q on all the psd files from YCOE

7) Run the RemoveDup.py from my corpus-tools repository on OldEngAct.out and OldEngPas.out

8) Run the external.c coding query from my corpus-tools repository on OldEngAct.psd and OldEngPas.psd, naming the file oldeng_external.cod (to distinguish it from the other external.cod)

9) Run OEVerbs.c on oldeng_external.cod

10) Run internal.c on both Verbs.cod and OEVerbs.cod simultaneously

11) Copy internal.cod to dit.cod

12) Run the only-coding.q query from my corpus-tools repository on dit.cod

13) Go to the corpus-tools directory and run fill-in-externals.py with dit.cod.ooo as an argument

14) Open dit.csv and copy the headings for internal info into csv (at the end of the first line): ,Clause,IOType,ObjOrder,Dat,Acc,IONum,AccNum,IOCP,AccCP,Pas,AccOrder,DatOrder,Verb,Nom

15) Run the data.R script in the same directory as the dit.csv just generated.  This will create a dit.RData, with only the data from actual ditransitive tokens.

16) Run the prior.R script in the same directory as dit.RData, which will create the prior.RData, which contains the priors for the STAN model.
