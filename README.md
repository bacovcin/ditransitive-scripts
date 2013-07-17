ditransitive-scripts
====================

Scripts related to the study of ditransitive sentences (especially in English)

1) Run Full.q on all of the English corpora (PPCME2,PCEEC,PPCEME, and PPCMBE), except for YCOE

2) Run the RemoveDup.py from my corpus-tools repository on Full.out

3) Run the external.c coding query from my corpus-tools repository on Full.psd

4) Run Verbs.c on external.cod

5) Run OldEng.q on all of the psd files from YCOE

6) Run the RemoveDup.py from my corpus-tools repository on OldEng.out

7) Run the external.c coding query from my corpus-tools repository on OldEng.psd, naming the file oldeng_external.cod (to distinguish it from the other external.cod)

8) Run OEVerbs.c on oldeng_external.cod

9) Run internal.c on both Verbs.cod and OEVerbs.cod simultaneously

10) Copy internal.cod to dit.cod

11) Run the only-coding.q query from my corpus-tools repository on dit.cod

12) Go to the corpus-tools directory and run fill-in-externals.py with dit.cod.ooo as an argument

13) Open dit.csv and copy the headings for internal info into csv (at the end of the first line): ,Clause,IOType,ObjOrder,IO,Acc,IONum,AccNum,IOCP,AccCP,Pas,AccOrder,DatOrder,Verb
