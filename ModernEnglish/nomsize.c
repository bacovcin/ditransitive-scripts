node: CP*

define: Ditrans.def

coding_query:

18: {
        NA: (IP* iDoms nom) AND (nom iDoms \**)	
	\1: (IP*  iDoms nom) AND (nom DomsWords 1)
	\2: (IP*  iDoms nom) AND (nom DomsWords 2)
	\3: (IP*  iDoms nom) AND (nom DomsWords 3)
	\4: (IP*  iDoms nom) AND (nom DomsWords 4)
	\5: (IP*  iDoms nom) AND (nom DomsWords 5)
	\6: (IP*  iDoms nom) AND (nom DomsWords 6)
	\7: (IP*  iDoms nom) AND (nom DomsWords 7)
	\8: (IP*  iDoms nom) AND (nom DomsWords 8)
	\9: (IP*  iDoms nom) AND (nom DomsWords 9)
	\10: (IP*  iDoms nom) AND (nom DomsWords 10)
	\11: (IP*  iDoms nom) AND (nom DomsWords 11)
	\12: (IP*  iDoms nom) AND (nom DomsWords 12)
	\13: (IP*  iDoms nom) AND (nom DomsWords 13)
	\14: (IP*  iDoms nom) AND (nom DomsWords 14)
	\15: (IP*  iDoms nom) AND (nom DomsWords 15)
	\16: (IP*  iDoms nom) AND (nom DomsWords 16)
	\17: (IP*  iDoms nom) AND (nom DomsWords 17)
	\18: (IP*  iDoms nom) AND (nom DomsWords 18)
	\19: (IP*  iDoms nom) AND (nom DomsWords 19)
	\20: (IP*  iDoms nom) AND (nom DomsWords 20)
	MORE: (IP*  iDoms nom) AND (nom DomsWords> 20)
	NA: ELSE
}
