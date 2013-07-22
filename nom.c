node: IP*
coding_query:
16: {
	NomDefinite: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms *$|*^G|PRO$*|NPR*|NR*)
	NomPronoun: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms PRO*)
	NomDPronoun: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDomsOnly D)
	NomEmptyWH: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms \*T*) AND (\*T* SameIndex WNP*) AND (WNP* iDoms \0)
	NomWH: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms \*T*) AND (\*T* SameIndex WNP*)
	NomEmpty: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms \**)
	NomDefinite: (IP*  iDoms NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms D) AND (D iDoms +da|+dare|+dat|+Dat|+de|+des|+dese|+dessere|+dis|+Dis|+do|se|Se|+t+as|+t+at|+t=e=|+t=t=|+ta|+Ta|+tan|+tane|+tat|+Tat|+tatt|+Tatt|+te|+Te|$+te|+teise|+ten|+tene|+tenne|+teo|+teos|+teose|+tes|+Tes|+tese|+Tese|+tess|+tet|+Tet|+ti|+tie|+tin|+tir|+Tir|+tis|+Tis|+tise|+tiss|+Tiss|+tisse|+to|$+to|+ton|+tone|+too|+tt|+tus|+tys|+Tys|+tyse|$te|te|th'|thaese|than|thase|that|That|thatt|the|The|$the|THE|the.|theas|thease|thees|theese|theis|theise|Theise|theke|thes|Thes|these|These|$these|thes~|thies|Thies|thiese|thiis|thir|thire|this|This|this.|thise|tho|thoo|thos|those|Those|thoss|thoys|thus|thyes|Thyes|thys|Thys|thyse|tiss|tys|y=e=|Y=e=|y=e=.|y=t=|ye|yt)
	NomIndefinite: (IP*  iDoms NP-NOM*|NP-SBJ*)
	NomNull: ELSE
}
