node: IP*
coding_query:
4: {
  DP: (IP* iDoms NP-DAT*|NP-DTV*|NP-OB2*)
  TO: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms towe|too|toe|to|To|$to|TO|te|ta|tu|zuo)
  FOR: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ffor|Ffor|ffore|foore|Foore|for|For|$for|FOR|fore|$fore|Fore|forr|Forr)
  UNTO: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms ynto|vnto|Vnto|$vnto|vntoo|vn-to|unto|Unto)
  TIL: (IP* iDoms PP) AND (PP iDomsMod PP|CONJP P) AND (P iDoms til|Til|till|Till|tille)
  PP: (IP* iDoms PP)
  NoIO: ELSE
}

15: {
  OLDENG: ELSE
}
