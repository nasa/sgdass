        LOGICAL FUNCTION DIG_MEN(C)
! ************************************************************************
! *                                                                      *
! *     לןחי‏וףכבס זץמכדיס  DIG_MEN  נעימיםבופ תמב‏ומיו  .TRUו., וףלי    *
! *     גבךפןקבס נועוםוממבס  C  יםוופ תמב‏ומיו דיזעש י .FALSE., וףלי  C  *
! *     --  מו דיזעב.                                                    *
! *                                                                      *
! ************************************************************************
        INTEGER*1 C
        DIG_MEN=.TRUE.
        IF(C.LT.48) DIG_MEN=.FALSE.
        IF(C.GT.57) DIG_MEN=.FALSE.
        RETURN
        END  !#!  DIG_MEN  #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE IFOR_MEN ( STR_I, I, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ ףפעןכץ  STR_I  י נעוןגעבתץופ וו ק דולןו   *
! *     ‏יףלן I. וףלי üפןחן ףהולבפר מו ץהבופףס, פן נועוםוממןך IERR       *
! *     נעיףקביוקבופףס תמב‏ומיו 1                                        *
! *           קשתשקבופףס נןהנעןחעבםםב IRFOR                              *
! *                                                                      *
! ************************************************************************
        INTEGER*4, EXTERNAL :: ILEN
        CHARACTER STR_I*(*), STR*40
!
!!        LOGICAL LW$STR
!!        CALL VER$ARG ( 3 )
        I=-1111111111
!!        IF ( .NOT.LR$STR ( STR_I ) ) THEN
!!              IERR=2
!!              RETURN
!!        END IF
        IF ( ILEN ( STR_I ).EQ.0 ) THEN
              IERR=3
              RETURN
        END IF
        CALL CLRCH ( STR )
        STR=STR_I
        CALL CHASHL ( STR )
        N=ILEN ( STR )
!
        IF ( STR(1:1).EQ.'+' ) THEN
             STR(1:1)=' '
             CALL CHASHL ( STR )
             N=N-1
             IF ( N.EQ.0 ) THEN
                  I=0
                  RETURN
             END IF
        END IF
!
        IERR=0
        I=0
        CALL IRFOR_MEN ( STR, R, IERR )
        IF(R.GT.2.E9 .OR. R.LT.-2.E9) IERR=1
        IF(IERR.GT.0) RETURN
        IF(R.GT.0.) I=INT(R+1.0E-6)
        IF(R.LT.0.) I=INT(R-1.0E-6)
        IF(R.EQ.0.) I=0
        RETURN
        END  !#!  IFOR_MEN  #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE LFOR_MEN ( STR_I, L, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ ףפעןכץ  STR_I  י נעוןגעבתץופ וו ק         *
! *     לןחי‏וףכץא נועוםוממץא L . וףלי üפןחן ףהולבפר מו ץהבופףס, פן      *
! *     נועוםוממןך IERR נעיףקביוקבופףס תמב‏ומיו 1                        *
! *                                                                      *
! ************************************************************************
        INTEGER*4, EXTERNAL :: ILEN
        LOGICAL*4 L
        CHARACTER STR_I*(*), STR*40, S1*1
        L=.FALSE.
!
!!        LOGICAL LW$STR
!!        CALL VER$ARG ( 3 )
!!        IF ( .NOT.LR$STR ( STR_I ) ) THEN
!!             IERR=2
!!             RETURN
!!        END IF
        IF ( ILEN ( STR_I ).EQ.0 ) THEN
             IERR=3
             RETURN
        END IF
        CALL CLRCH ( STR )
        STR=STR_I
        CALL CHASHL ( STR )
        N=ILEN ( STR )
!
        N=ILEN( STR )
        IERR=0
        S1=STR(1:1)
            L=.TRUE.
        IF( S1.EQ.'T' .OR. S1.EQ.'t'  .OR. &
     &      S1.EQ.'פ' .OR. S1.EQ.'Ô' )      THEN
            RETURN
        END IF
!
        IF( S1.EQ.'F' .OR. S1.EQ.'f'  .OR. &
     &      S1.EQ.'ז' .OR. S1.EQ.'Æ' )      THEN
            L=.FALSE.
            RETURN
        END IF
        IERR=1
        RETURN
        END  !#!  LFOR_MEN   #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE IRFOR_MEN( STR, R, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ ףפעןכץ  STR ,כןפןעבס ףןףפןיפ פןלרכן ית    *
! *     דיזע י נעוןגעבתץופ וו ק נועוםוממץא R פינב REAL*4. וףלי üפןחן     *
! *     ףהולבפר מו ץהבופףס, נועוםוממןך IERR נעיףקביוקבופףס תמב‏ומיו 1    *
! *         קשתשקבאפףס זץמכדיס DIG_MEN.                                  *
! *                                                                      *
! ************************************************************************
        INTEGER*4, EXTERNAL :: ILEN
        INTEGER*1  C, BCHAR
        LOGICAL DIG_MEN
        CHARACTER STR*(*)
        N=ILEN( STR )
        IERR=0
        R10=1.
        R=0.
        DO 410 J1=1,N
           KK=N-J1+1
           C=BCHAR( STR(KK:KK) )   ! ן‏ועוהמןך ףיםקןל
           IF(.NOT.DIG_MEN(C)) GOTO 810  ! נעןקועכב: דיזעב לי üפן
           IR=C-48         ! IR  -- תמב‏ומיו דיזעש
           R=IR*R10+R      ! ץקולי‏ומיו קשטןהמןחן ‏יףלב נעי נעיניףשקבמיי
!                          ! כ מוםש ןהמןך דיזעש ףנעבקב
           R10=R10*10.     ! R10  --  נעןסהןכ ‏יףלב ק ףפונומי 10
           GOTO 410
  810      IF(J1.NE.N) GOTO 820  ! נעןקועכב: מו תמבכ לי üפן
!
! -------- בלרפועמבפיקב: תמבכ
!
           IF ( STR(KK:KK).EQ.'+' .OR. STR(KK:KK).EQ.'-' ) R=-1.*R
           IF ( STR(KK:KK).EQ.'+' .OR. STR(KK:KK).EQ.'-' ) RETURN
!
! -------- בלרפועמבפיקב: מי תמבכ מי דיזעב
!
  820      IERR=1
           RETURN
  410   CONTINUE
        RETURN
        END  !#!  IRFOR_MEN  #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE RFOR_MEN ( STR_I, R, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ ףפעןכץ  STR_I  י נעוןגעבתץופ וו ק         *
! *     נועוםוממץא R פינב REAL*4. וףלי üפןחן ףהולבפר מו ץהבופףס,         *
! *     נועוםוממןך IERR נעיףקביוקבופףס תמב‏ומיו 1                        *
! *           קשתשקבופףס נןהנעןחעבםםב IRFOR                              *
! *                                                                      *
! ************************************************************************
        INTEGER*4, EXTERNAL :: ILEN
        CHARACTER STR_I*(*), STR*40, S2*40, S3*40
!
!!        LOGICAL LW$STR
!!        CALL VER$ARG ( 3 )
        R=-1.111111E11
!!        IF ( .NOT.LR$STR ( STR_I ) ) THEN
!!             IERR=2
!!             RETURN
!!        END IF
        N=ILEN ( STR_I )
        IF ( N.EQ.0 ) THEN
             IERR=3
             RETURN
        END IF
        IF ( N.GT.40 ) THEN
             IERR=1
             RETURN
        END IF
        CALL CLRCH ( STR )
        STR=STR_I
        CALL CHASHL ( STR )
        N=ILEN ( STR )
!
        IF ( STR(1:1).EQ.'+' ) THEN
             STR(1:1)=' '
             CALL CHASHL ( STR )
             N=N-1
             IF ( N.EQ.0 ) THEN
                  R=0.0
                  RETURN
             END IF
        END IF
!
        IERR=0
        IP=0
        IE=0
        EX=1.
        R1=0.
        R2=0.
        R=0.
!
! ----- ‏יףלן עבתגיקבופףס מב 3 ‏בףפי:
! ----- 1) דולץא               --  תמב‏ומיו  R1
! ----- 2) העןגמץא             --  תמב‏ומיו  R2
! ----- 3) üכדנןמומדיבלרמץא    --  תמב‏ומיו נןכבתבפולס ףפונומי  I3
!
        DO 410 J1=1,N
           IF(STR(J1:J1).EQ.'.') IP=J1   !  IP  --  מןםוע ףיםקןלב '.'
           IF(STR(J1:J1).EQ.'E') IE=J1   !  IE  --  מןםוע ףיםקןלב 'E' ילי 'D'
           IF(STR(J1:J1).EQ.'ו') IE=J1
           IF(STR(J1:J1).EQ.'e') IE=J1
           IF(STR(J1:J1).EQ.'Å') IE=J1
           IF(STR(J1:J1).EQ.'D') IE=J1
           IF(STR(J1:J1).EQ.'d') IE=J1
           IF(STR(J1:J1).EQ.'Ä') IE=J1
           IF(STR(J1:J1).EQ.'ה') IE=J1
  410   CONTINUE
        IF ( N.EQ.1 .AND. ( IP.EQ.1 .OR. IE.EQ.1 ) ) THEN
!
! ---------- ףפעןכב ףןףפןיפ ית ןהמןחן ףיםקןלב י üפןפ ףיםקןל מו דיזעב
!
             IERR=1
             RETURN
        END IF
!
! ----- בלרפועמבפיקב: מופ מי üכףנןמומדיבלרמןך מי העןגמןך ‏בףפי
!
        IF ( .NOT. ( IP.EQ.0 .AND. IE.EQ.0 ) ) GOTO 810
          CALL IRFOR_MEN( STR(1:N), R, IERR )
          RETURN
  810   NP=IP-1
        IF(IP.EQ.0) NP=IE-1
        IF(IP.EQ.1) GOTO 820
        CALL IRFOR_MEN( STR(1:NP), R1, IERR )  !  R1  --  תמב‏ומיו דולןך ‏בףפי
        IF(IERR.GT.0) RETURN
  820   NBE=IP+1
        IF(IP.EQ.N) GOTO 830
        IF(IP.EQ.0) GOTO 830
        IF(IE.EQ.(IP+1)) GOTO 830
        NEE=IE-1
        IF(IE.EQ.0) NEE=N
        IF(IE.EQ.N) IERR=1
        IF(IE.EQ.N) RETURN
        J=0
!
! ----- קשהולומיו העןגמןך ‏בףפי
!
        DO 420 J2=NBE,NEE
           J=J+1
           S2(J:J)=STR(J2:J2)
  420   CONTINUE
        CALL IRFOR_MEN( S2(1:J), R2, IERR )  !  R2  --  תמב‏ומיו העןגמןך ‏בףפי
        IF(R2.LT.0.) IERR=1
        IF(IERR.GT.0) RETURN
        R2=R2*EXP((-1.)*J*ALOG(10.))
  830   IF(IE.EQ.0) GOTO 840
        IF((N-IE).EQ.0) GOTO 840
!
! ------ קשהולומיו üכףנןמומדיבלרמןך ‏בףפי
!
        J=0
        DO 430 J3=IE+1,N
           J=J+1
           S3(J:J)=STR(J3:J3)
  430   CONTINUE
!
! ----- I3  --  תמב‏ומיE נןכבתבפולס ףפונומי הוףספי
!
        CALL IFOR_MEN( S3(1:J), I3, IERR )
        IF(IERR.GT.0) RETURN
        C=ALOG10(ABS(R1)+1.E-7)+I3
        IF(C.GT.38. .OR. C.LT.-37.) IERR=1
        IF(C.GT.38. .OR. C.LT.-37.) RETURN
        EX=EXP(I3*ALOG(10.))     !  EX  --  תמב‏ומיו üכףנןמומדיבלרמןך ‏בףפי
  840   CONTINUE
        IF(R1.LT.0) R=-1.*(R2-R1)*EX
        IF(R1.GT.0) R=(R2+R1)*EX
        IF(R1.EQ.0) R=R2*EX
        IF ( R1.EQ.0 .AND. STR(1:1).EQ.'-' ) R=-1.0*R
        RETURN
        END  !#!  RFOR_MEN  #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE IDFOR_MEN( STR, D, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ גבךפןקץא ףפעןכץ  STR,  כןפןעבס ףןףפןיפ    *
! *     פןלרכן ית דיזע י נעוןגעבתץופ וו ק נועוםוממץא D פינב REAL*8.      *
! *     וףלי üפןחן ףהולבפר מו ץהבופףס, נועוםוממןך IERR                   *
! *     נעיףקביוקבופףס תמב‏ומיו 1                                        *
! *           קשתשקבופףס זץמכדיס DIG_MEN                                 *
! *                                                                      *
! ************************************************************************
        INTEGER*4, EXTERNAL :: ILEN
        INTEGER*1  C, BCHAR
        LOGICAL DIG_MEN
        REAL*8 D,D10
        CHARACTER STR*(*)
!
        IERR=0
        N=ILEN( STR )
        IF(IERR.GT.0) RETURN
        IERR=0
        D10=1.
        D=0.
        DO 410 J1=1,N
           KK=N-J1+1
           C=BCHAR( STR( KK:KK ) )
           IF(.NOT.DIG_MEN(C)) GOTO 810
           IR=C-48
           D=IR*D10+D
           D10=D10*10.D0
           GOTO 410
  810      IF(J1.NE.N) GOTO 820
           IF ( STR(KK:KK).EQ.'+' .OR. STR(KK:KK).EQ.'-' ) D=-1.D0*D
           IF ( STR(KK:KK).EQ.'+' .OR. STR(KK:KK).EQ.'-' ) RETURN
  820      IERR=1
           RETURN
  410   CONTINUE
        RETURN
        END  !#!  IDFOR_MEN  #!#
!
! ----------------------------------------------------------------------------
!
        SUBROUTINE DFOR_MEN ( STR_I, D, IERR )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב נעימיםבופ ףפעןכץ  STR_I  י נעוןגעבתץופ וו ק         *
! *     נועוםוממץא D פינב REAL*8. וףלי üפןחן ףהולבפר  מו ץהבופףס,        *
! *     נועוםוממןך IERR נעיףקביוקבופףס תמב‏ומיו 1                        *
! *           קשתשקבופףס נןהנעןחעבםםב IDFOR                              *
! *                                                                      *
! ************************************************************************
        CHARACTER STR_I*(*), STR*40, S2*40, S3*40
        REAL*8 D,D1,D2,DEX
        INTEGER*4, EXTERNAL :: ILEN
!
!!        LOGICAL LW$STR
!!        CALL VER$ARG ( 3 )
        D=-1.11111111111D11
!!        IF ( .NOT.LR$STR ( STR_I ) ) THEN
!!             IERR=2
!!             RETURN
!!        END IF
        N=ILEN ( STR_I )
        IF ( N.EQ.0 ) THEN
             IERR=3
             RETURN
        END IF
        IF ( N.GT.40 ) THEN
             IERR=1
             RETURN
        END IF
        CALL CLRCH ( STR )
        STR=STR_I
        CALL CHASHL ( STR )
        N=ILEN ( STR )
!
        IF ( STR(1:1).EQ.'+' ) THEN
             STR(1:1)=' '
             CALL CHASHL ( STR )
             N=N-1
             IF ( N.EQ.0 ) THEN
                  D=0.D0
                  RETURN
             END IF
        END IF
!
        IERR=0
        IP=0
        IE=0
        DEX=1.D0
        D1=0.
        D2=0.
        D=0.
        DO 410 J1=1,N
           IF(STR(J1:J1).EQ.'.') IP=J1
           IF(STR(J1:J1).EQ.'D') IE=J1
           IF(STR(J1:J1).EQ.'d') IE=J1
           IF(STR(J1:J1).EQ.'Ä') IE=J1
           IF(STR(J1:J1).EQ.'ה') IE=J1
           IF(STR(J1:J1).EQ.'E') IE=J1
           IF(STR(J1:J1).EQ.'e') IE=J1
           IF(STR(J1:J1).EQ.'Å') IE=J1
           IF(STR(J1:J1).EQ.'ו') IE=J1
  410   CONTINUE
        IF ( N.EQ.1 .AND. ( IP.EQ.1 .OR. IE.EQ.1 ) ) THEN
!
! ---------- ףפעןכב ףןףפןיפ ית ןהמןחן ףיםקןלב י üפןפ ףיםקןל מו דיזעב
!
             IERR=1
             RETURN
        END IF
!
! ----- בלרפועמבפיקב: מופ מי üכףנןמומדיבלרמןך מי העןגמןך ‏בףפי
!
        IF(.NOT. (IP.EQ.0 .AND. IE.EQ.0)) GOTO 810
          CALL IDFOR_MEN( STR(1:N), D, IERR )
          RETURN
  810   NP=IP-1
        IF(IP.EQ.0) NP=IE-1
        IF(IP.EQ.1) GOTO 820
        IF(NP.LE.0) GOTO 820
        CALL IDFOR_MEN( STR(1:NP), D1, IERR )
        IF(IERR.GT.0) RETURN
  820   NBE=IP+1
        IF(IP.EQ.N) GOTO 830
        IF(IP.EQ.0) GOTO 830
        IF(IE.EQ.(IP+1)) GOTO 830
        NEE=IE-1
        IF(IE.EQ.0) NEE=N
        IF(IE.EQ.N) IERR=1
        IF(IE.EQ.N) RETURN
        J=0
        DO 420 J2=NBE,NEE
           J=J+1
           S2(J:J)=STR(J2:J2)
  420   CONTINUE
        CALL IDFOR_MEN( S2(1:J), D2, IERR )
        IF(D2.LT.0.) IERR=1
        IF(IERR.GT.0) RETURN
        D2=D2*DEXP((-1.D0)*J*DLOG(10.D0))
  830   IF(IE.EQ.0) GOTO 840
        IF((N-IE).EQ.0) GOTO 840
        J=0
        DO 430 J3=IE+1,N
           J=J+1
           S3(J:J)=STR(J3:J3)
  430   CONTINUE
        CALL IFOR_MEN( S3(1:J), I3, IERR )
        IF(IERR.GT.0) RETURN
        C=DLOG10(DABS(D1)+1.D-7)+I3
        IF(C.GT.38. .OR. C.LT.-37.) IERR=1
        IF(C.GT.38. .OR. C.LT.-37.) RETURN
        DEX=DEXP(I3*DLOG(10.D0))
  840   CONTINUE
        IF(D1.LT.0) D=-1.D0*(D2-D1)*DEX
        IF(D1.GT.0) D=(D2+D1)*DEX
        IF(D1.EQ.0) D=D2*DEX
        IF( D1.EQ.0 .AND. STR(1:1).EQ.'-' ) D=-1.D0*D
        RETURN
        END  !#!  DFOR_MEN  #!#
