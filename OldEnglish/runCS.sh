#!/bin/bash

CS() {
	    java -classpath "$(cygpath -w "/cygdrive/c/Corpora/CS_2.003.04.jar")" csearch/CorpusSearch $(cygpath -w $@)
    }

CS Verbs.c external.cod;
CS clausetype.c Verbs.cod;
CS PP.c clausetype.cod;
CS NomType.c PP.cod;
CS DatType.c NomType.cod;
CS AccType.c DatType.cod;
CS NomVerbOrd.c AccType.cod;
CS DatVerbOrd.c NomVerbOrd.cod;
CS AccVerbOrd.c DatVerbOrd.cod;
CS NomDatOrd.c AccVerbOrd.cod;
CS NomAccOrd.c NomDatOrd.cod;
CS AccDatOrd.c NomAccOrd.cod;
CS pas.c AccDatOrd.cod;
CS hascp.c pas.cod;
CS nomsize.c hascp.cod;
CS datsize.c nomsize.cod;
CS accsize.c datsize.cod;
CS adj.c accsize.cod;
