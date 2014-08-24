ditransitive-scripts
====================

Scripts related to the study of ditransitive sentences (especially in English)

1) Run the add-cp.q from my corpus-tools repository on the English corpora

2) Run the remove-dup-cp.q from my corpus-tools repository on the output of the previous query.

3) Run Full.q on the output of the previous query.

2) Run the RemoveDup.py from my corpus-tools repository on Full.out

3) Run the external.c coding query from my corpus-tools repository on Full.psd, and make sure that the external.cod file is in the same folder as this file.

4a) Run the runCS.sh script, and then go to step 21.

4b) Run Verbs.c on external.cod

5) Run clausetype.c

5) Run PP.c

6) Run NomType.c

7) Run DatType.c

8) Run AccType.c

9) Run NomVerbOrd.c

10) Run DatVerbOrd.c

11) Run AccVerbOrd.c

12) Run NomDatOrd.c

13) Run NomAccOrd.c

14) Run AccDatOrd.c

15) Run pas.c

16) Run hascp.c

17) Run nomsize.c

18) Run datsize.c

19) Run accsize.c

20) Run adj.c

21) Run the only-coding.q query from my corpus-tools repository on dit.cod

22) Go to the corpus-tools directory and run fill-in-externals.py with dit.cod.ooo as an argument

23) Open dit.csv and copy the headings for internal info into csv (at the end of the first line): 
	Verb	Clause	PP	Nom	Dat	Acc	NomVerb	DatVerb	AccVerb	NomDat	NomAcc	DatAcc	Pas	NomCP	DatCP	AccCP	NomSize	DatSize	AccSize	Adj

24) Run the data.R script in the same directory as the dit.csv just generated.  This will create a dit.RData, with only the data from actual ditransitive tokens.

16)* Run the prior.R script in the same directory as dit.RData, which will create the prior.RData, which contains the priors for the STAN model.

17) Run the stan.R script in the same directory as prior.RData, which will create the following set of model posteriors:

sep.RData - all of the binomial parameters are drawn from independent distributions

m2uh.RData - same as sep, except that the slope of the first logistic in the DA equation is drawn from the same distribution as for the slope of the AD equation

m3uh.RData - same as sep, except that the slope of the second logistic in the DA equation is drawn from the same distribution as for the slope of the PAS equation

m23uh.RData - combines m2uh.RData and m3uh.RData

m2up.RData - same as sep, except that the slope of the first logistic in the DA equation is forced to be identical to the slope of the AD equation

m2up.RData - same as sep, except that the slope of the second logistic in the DA equation is forced to be identical to the slope of the PAS equation

m23up.RData - combines m2up.RData and m3up.RData

* pas_noun has scripts and data in which only passive sentences in which the recipient is a noun phrase (as opposed to a pronoun) are included
