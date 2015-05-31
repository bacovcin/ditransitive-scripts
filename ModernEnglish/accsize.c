node: CP*

define: Ditrans.def

coding_query:

20: {
	NA: (IP* iDoms acc) AND (acc iDoms \**)
	\1: (IP*  iDoms acc) AND (acc DomsWords 1)
	\2: (IP*  iDoms acc) AND (acc DomsWords 2)
	\3: (IP*  iDoms acc) AND (acc DomsWords 3)
	\4: (IP*  iDoms acc) AND (acc DomsWords 4)
	\5: (IP*  iDoms acc) AND (acc DomsWords 5)
	\6: (IP*  iDoms acc) AND (acc DomsWords 6)
	\7: (IP*  iDoms acc) AND (acc DomsWords 7)
	\8: (IP*  iDoms acc) AND (acc DomsWords 8)
	\9: (IP*  iDoms acc) AND (acc DomsWords 9)
	\10: (IP*  iDoms acc) AND (acc DomsWords 10)
	\11: (IP*  iDoms acc) AND (acc DomsWords 11)
	\12: (IP*  iDoms acc) AND (acc DomsWords 12)
	\13: (IP*  iDoms acc) AND (acc DomsWords 13)
	\14: (IP*  iDoms acc) AND (acc DomsWords 14)
	\15: (IP*  iDoms acc) AND (acc DomsWords 15)
	\16: (IP*  iDoms acc) AND (acc DomsWords 16)
	\17: (IP*  iDoms acc) AND (acc DomsWords 17)
	\18: (IP*  iDoms acc) AND (acc DomsWords 18)
	\19: (IP*  iDoms acc) AND (acc DomsWords 19)
	\20: (IP*  iDoms acc) AND (acc DomsWords 20)
	MORE: (IP*  iDoms acc) AND (acc DomsWords> 20)
	NA: ELSE
}
