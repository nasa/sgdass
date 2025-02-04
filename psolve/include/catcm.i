      Integer*2 lucat1
      Integer*2 lucat2
      Integer*2 luhash
      Integer*2 ikey
!
      INTEGER*4 IDOUB,nbytes
      INTEGER*4 NW,NSA,NREC,NKEY,NWORDS,icnbf,ifrst,ilast, &
     &          IBUFL,LUO,KYREC,ICURR,NUMRC, &
     &          ICRAC,IKSPT,IKSLN,JCRT2,IFAST,IUP, &
     &          IMAX,LREC,IKEY1,IHSH, &
     &          ITERM,IHLEN,IHINIT,NUMFS,INTFS
      Integer*2     bufi(500),key(5),ick(5),ifs(5),iubuf(150)
      Integer*4     bufj(250),                     jubuf(75)
      Integer*4 bytes_in_user_area, i4_words_in_user_area
      Character*10 ick_c, ifs_c
      Character*(*) bufc*1000                     ,cubuf*300
      equivalence (ick,ick_c), (ifs,ifs_c)
      Equivalence (bufi,bufj,bufc)
      Equivalence (iubuf,jubuf,cubuf)
      Integer*2 laces,lbuf,ifnam(3),jbuf(200),iks(200,6)
!
      COMMON /CHAIN/ &
     & bufi,NW,NSA,NREC,KEY,NKEY, &
     & NWORDS,nbytes, &
     & IKEY,IBUFL,LUO,KYREC,ICURR,ICK,IFS,NUMRC,ICNBF,IFRST, &
     & ILAST,IUBUF,ICRAC,IKS,IKSPT,IKSLN,JCRT2,IFAST,IUP, &
     & LACES,IMAX,LBUF,LREC,IKEY1,IFNAM,IHSH, &
     & ITERM,IHLEN,IHINIT,NUMFS,INTFS,IDOUB,LUCAT1,LUCAT2, &
     & LUHASH,JBUF,bytes_in_user_area, i4_words_in_user_area
