node: IP*

coding_query:

// date of author's birth - century

3: {
   MAT: (IP-MAT* IsRoot)
   SUB: (IP-SUB* IsRoot)
   IMP: (IP-IMP* IsRoot)
   INF: (IP-INF* IsRoot)
   _: ELSE
}

5: {
   DA: (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-DAT*|NP-DTV*|NP-OB2* Pres NP-ACC*|NP-OB1*)
   AD: (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* Pres NP-DAT*|NP-DTV*|NP-OB2*)
   DA: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (PP Pres NP-ACC*|NP-OB1*)
   AD: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* Pres PP)
   MONO: ELSE
}

6: {
	IODefinite: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDoms *$|*^G|PRO$*)
	IOPronoun: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDoms PRO*)
	IODPronoun: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDomsOnly D)
	IOEmpty: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDoms \**)
	IODefinite: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDoms D) AND (D iDoms +da|+dare|+dat|+Dat|+de|+des|+dese|+dessere|+dis|+Dis|+do|se|Se|+t+as|+t+at|+t=e=|+t=t=|+ta|+Ta|+tan|+tane|+tat|+Tat|+tatt|+Tatt|+te|+Te|$+te|+teise|+ten|+tene|+tenne|+teo|+teos|+teose|+tes|+Tes|+tese|+Tese|+tess|+tet|+Tet|+ti|+tie|+tin|+tir|+Tir|+tis|+Tis|+tise|+tiss|+Tiss|+tisse|+to|$+to|+ton|+tone|+too|+tt|+tus|+tys|+Tys|+tyse|$te|te|th'|thaese|than|thase|that|That|thatt|the|The|$the|THE|the.|theas|thease|thees|theese|theis|theise|Theise|theke|thes|Thes|these|These|$these|thes~|thies|Thies|thiese|thiis|thir|thire|this|This|this.|thise|tho|thoo|thos|those|Those|thoss|thoys|thus|thyes|Thyes|thys|Thys|thyse|tiss|tys|y=e=|Y=e=|y=e=.|y=t=|ye|yt)
	IOName: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* iDoms NPR*|NR*)
	IOIndefinite: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*)
	IODefinite: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms *$|*^G|PRO$*)
 	IOPronoun: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms PRO*)
 	IODPronoun: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDomsOnly D)
	IODefinite: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms D) AND (D iDoms +da|+dare|+dat|+Dat|+de|+des|+dese|+dessere|+dis|+Dis|+do|se|Se|+t+as|+t+at|+t=e=|+t=t=|+ta|+Ta|+tan|+tane|+tat|+Tat|+tatt|+Tatt|+te|+Te|$+te|+teise|+ten|+tene|+tenne|+teo|+teos|+teose|+tes|+Tes|+tese|+Tese|+tess|+tet|+Tet|+ti|+tie|+tin|+tir|+Tir|+tis|+Tis|+tise|+tiss|+Tiss|+tisse|+to|$+to|+ton|+tone|+too|+tt|+tus|+tys|+Tys|+tyse|$te|te|th'|thaese|than|thase|that|That|thatt|the|The|$the|THE|the.|theas|thease|thees|theese|theis|theise|Theise|theke|thes|Thes|these|These|$these|thes~|thies|Thies|thiese|thiis|thir|thire|this|This|this.|thise|tho|thoo|thos|those|Those|thoss|thoys|thus|thyes|Thyes|thys|Thys|thyse|tiss|tys|y=e=|Y=e=|y=e=.|y=t=|ye|yt)
	IOName: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*) AND (NP-NOM*|NP-SBJ* iDoms NPR*|NR*)
	IOIndefinite: (IP*  iDoms PP) AND (PP iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*|QP*) AND (NP*|QP* iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*)
	IODefinite: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDoms *$|*^G|PRO$*)
	IOPronoun: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDoms PRO*)
	IODPronoun: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDomsOnly D)
	IOEmpty: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDoms \**)
	IODefinite: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDoms D) AND (D iDoms +da|+dare|+dat|+Dat|+de|+des|+dese|+dessere|+dis|+Dis|+do|se|Se|+t+as|+t+at|+t=e=|+t=t=|+ta|+Ta|+tan|+tane|+tat|+Tat|+tatt|+Tatt|+te|+Te|$+te|+teise|+ten|+tene|+tenne|+teo|+teos|+teose|+tes|+Tes|+tese|+Tese|+tess|+tet|+Tet|+ti|+tie|+tin|+tir|+Tir|+tis|+Tis|+tise|+tiss|+Tiss|+tisse|+to|$+to|+ton|+tone|+too|+tt|+tus|+tys|+Tys|+tyse|$te|te|th'|thaese|than|thase|that|That|thatt|the|The|$the|THE|the.|theas|thease|thees|theese|theis|theise|Theise|theke|thes|Thes|these|These|$these|thes~|thies|Thies|thiese|thiis|thir|thire|this|This|this.|thise|tho|thoo|thos|those|Those|thoss|thoys|thus|thyes|Thyes|thys|Thys|thyse|tiss|tys|y=e=|Y=e=|y=e=.|y=t=|ye|yt)
	IOName: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*) AND (NP* iDoms NPR*|NR*)
	IOIndefinite: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P HasSister NP*|QP*)
	IONull: ELSE
}

7: {
	AccDefinite: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms *$|*^G|PRO$*|NPR*|NR*)
	AccPronoun: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms PRO*)
	AccDPronoun: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDomsOnly D)
	AccEmptyWH: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms \*T*) AND (\*T* SameIndex WNP*) AND (WNP* iDoms \0)
	AccWH: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms \*T*) AND (\*T* SameIndex WNP*)
	AccEmpty: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms \**)
	AccDefinite: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* iDoms D) AND (D iDoms +da|+dare|+dat|+Dat|+de|+des|+dese|+dessere|+dis|+Dis|+do|se|Se|+t+as|+t+at|+t=e=|+t=t=|+ta|+Ta|+tan|+tane|+tat|+Tat|+tatt|+Tatt|+te|+Te|$+te|+teise|+ten|+tene|+tenne|+teo|+teos|+teose|+tes|+Tes|+tese|+Tese|+tess|+tet|+Tet|+ti|+tie|+tin|+tir|+Tir|+tis|+Tis|+tise|+tiss|+Tiss|+tisse|+to|$+to|+ton|+tone|+too|+tt|+tus|+tys|+Tys|+tyse|$te|te|th'|thaese|than|thase|that|That|thatt|the|The|$the|THE|the.|theas|thease|thees|theese|theis|theise|Theise|theke|thes|Thes|these|These|$these|thes~|thies|Thies|thiese|thiis|thir|thire|this|This|this.|thise|tho|thoo|thos|those|Those|thoss|thoys|thus|thyes|Thyes|thys|Thys|thyse|tiss|tys|y=e=|Y=e=|y=e=.|y=t=|ye|yt)
	AccIndefinite: (IP*  iDoms NP-ACC*|NP-OB1*)
	AccCP: (IP* iDoms CP-THT*)
	AccINF: (IP* iDoms IP-INF*)
	AccNull: ELSE
}

8: {
	\1: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 1)
	\1: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 1)
	\2: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 2)
	\2: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 2)
	\3: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 3)
	\3: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 3)
	\4: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 4)
	\4: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 4)
	\5: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 5)
	\5: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 5)
	\6: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 6)
	\6: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 6)
	\7: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 7)
	\7: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 7)
	\8: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 8)
	\8: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 8)
	\9: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 9)
	\9: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 9)
	\10: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 10)
	\10: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 10)
	\11: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 11)
	\11: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 11)
	\12: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 12)
	\12: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 12)
	\13: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 13)
	\13: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 13)
	\14: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 14)
	\14: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 14)
	\15: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 15)
	\15: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 15)
	\16: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 16)
	\16: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 16)
	\17: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 17)
	\17: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 17)
	\18: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 18)
	\18: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 18)
	\19: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 19)
	\19: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 19)
	\20: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 20)
	\20: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 20)
	\21: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 21)
	\21: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 21)
	\22: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 22)
	\22: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 22)
	\23: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 23)
	\23: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 23)
	\24: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 24)
	\24: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 24)
	\25: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 25)
	\25: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 25)
	\26: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 26)
	\26: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 26)
	\27: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 27)
	\27: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 27)
	\28: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 28)
	\28: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 28)
	\29: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 29)
	\29: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 29)
	\30: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 30)
	\30: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 30)
	\31: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 31)
	\31: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 31)
	\32: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 32)
	\32: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 32)
	\33: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 33)
	\33: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 33)
	\34: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 34)
	\34: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 34)
	\35: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 35)
	\35: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 35)
	\36: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 36)
	\36: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 36)
	\37: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 37)
	\37: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 37)
	\38: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 38)
	\38: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 38)
	\39: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 39)
	\39: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 39)
	\40: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 40)
	\40: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 40)
	\41: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 41)
	\41: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 41)
	\42: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 42)
	\42: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 42)
	\43: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 43)
	\43: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 43)
	\44: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 44)
	\44: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 44)
	\45: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 45)
	\45: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 45)
	\46: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 46)
	\46: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 46)
	\47: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 47)
	\47: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 47)
	\48: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 48)
	\48: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 48)
	\49: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 49)
	\49: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 49)
	\50: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords 50)
	\50: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords 50)
	MORE: (IP*  iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* DomsWords> 50)
	\50: (IP*  iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*) AND (NP* DomsWords> 50)
	\0: ELSE
}

9: {
	\1: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 1)
	\2: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 2)
	\3: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 3)
	\4: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 4)
	\5: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 5)
	\6: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 6)
	\7: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 7)
	\8: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 8)
	\9: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 9)
	\10: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 10)
	\11: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 11)
	\12: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 12)
	\13: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 13)
	\14: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 14)
	\15: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 15)
	\16: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 16)
	\17: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 17)
	\18: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 18)
	\19: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 19)
	\20: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 20)
	\21: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 21)
	\22: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 22)
	\23: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 23)
	\24: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 24)
	\25: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 25)
	\26: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 26)
	\27: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 27)
	\28: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 28)
	\29: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 29)
	\30: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 30)
	\31: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 31)
	\32: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 32)
	\33: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 33)
	\34: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 34)
	\35: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 35)
	\36: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 36)
	\37: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 37)
	\38: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 38)
	\39: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 39)
	\40: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 40)
	\41: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 41)
	\42: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 42)
	\43: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 43)
	\44: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 44)
	\45: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 45)
	\46: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 46)
	\47: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 47)
	\48: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 48)
	\49: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 49)
	\50: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords 50)
	MORE: (IP*  iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* DomsWords> 50)
}

10: {
	IOHasCP: (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* Doms CP*)
	IOHasCP: (IP* iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*|QP*) AND (NP*|QP* Doms CP*)
	IONoCP: (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* Doms !CP*)
	IONoCP: (IP* iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP* iDoms NP*|QP*) AND (NP*|QP* Doms !CP*)
	NOIO: ELSE
}

11: {
	AccHasCP: (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* Doms CP*)	
	AccNoCP: (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* Doms !CP*)	
	NOAcc: ELSE
}

12: {
        THEME: (IP*  iDoms BE*) AND (IP*  iDoms *VBN*|*VAN*) AND (IP* iDoms NP-DTV*|NP-OB2*|NP-DAT*) AND (IP* iDoms NP-SBJ*|NP-NOM*)
        REC: (IP*  iDoms BE*) AND (IP*  iDoms *VBN*|*VAN*) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (IP* iDoms NP-SBJ*|NP-NOM*)
        THEME: (IP*  iDoms BE*) AND (IP*  iDoms *VBN*|*VAN*) AND (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo)
	REC: (IP*  iDoms BE*) AND (IP*  iDoms *VBN*|*VAN*) AND (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms PP*) AND (PP* iDomsMod PP|CONJ* P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (P hasSister NP) AND (NP iDoms \*-*) AND (\*-* SameIndex NP-NOM*|NP-SBJ*)
        ACT: ELSE
}

13: {
  AN: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-ACC*|NP-OB1* Pres NP-SBJ*|NP-NOM*)
  NA: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms NP-ACC*|NP-OB1*) AND (NP-SBJ*|NP-NOM* Pres NP-ACC*|NP-OB1*)
  NOACC: ELSE
}

14: {
   DN: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-DAT*|NP-DTV*|NP-OB2* Pres NP-SBJ*|NP-NOM*)
   ND: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*) AND (NP-SBJ*|NP-NOM* Pres NP-DAT*|NP-DTV*|NP-OB2*)
   DN: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (PP Pres NP-SBJ*|NP-NOM*)
   ND: (IP* iDoms NP-SBJ*|NP-NOM*) AND (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto|towe|too|toe|til|Til|till|Till|tille|to|To|$to|TO|te|ta|tu|tho|ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr|zuo) AND (NP-SBJ*|NP-NOM* Pres PP)
}