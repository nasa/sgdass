#include <mk5_preprocessor_directives.inc>
      FUNCTION  PAUSE ( STR )
! ************************************************************************
! *                                                                      *
! *   Function PAUSE prints a line STR in the screen and waits for       *
! *   a user hitting any key.                                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 17-JAN-1989     PAUSE     v1.0 (d)  L. Petrov  10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4 PAUSE
      CHARACTER STR*(*)
      CHARACTER ASIM
      INTEGER*4 ISIM
      INTEGER*4, EXTERNAL :: INSIM
!
      CALL NEG
      CALL PRCH ( '---  PAUSE  ---' )
      CALL UN_NEG
      CALL CURR ( 2 )
      IF ( LOC(STR) .NE. 0 ) CALL PRCH ( STR )
      CALL CURR ( 2 )
      PAUSE = INSIM ( ASIM, ISIM )
      CALL CLSTR ()
      RETURN
      END  !#!  PAUSE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRCH ( STR )
      CHARACTER  STR*(*)
      INTEGER*4, EXTERNAL :: I_LEN
      WRITE ( 6, 110 ) STR(1:I_LEN(STR))
 110  FORMAT ( A,$ )
      CALL FLUSH ( 6 )
      RETURN
      END  !#!  PRCH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRI_BUF ( BUF, NBUF, LSTR_I, ICAR_I )
      CHARACTER  BUF(NBUF)*(*)
      INTEGER*4  ICAR, LSTR
      INTEGER*4, EXTERNAL :: I_LEN
!
      ICAR=1
      IF ( LOC(ICAR_I) .NE. 0 ) ICAR=ICAR_I
      IF ( ICAR .NE. 0  .AND.  ICAR .NE. 1 ) ICAR=1
!
      LSTR=LEN(BUF(1))
      IF ( LOC(LSTR_I) .NE. 0 ) LSTR=LSTR_I
      DO 410 J1=1,NBUF
         LN=I_LEN(BUF(J1))
         IF ( LN .GT. LSTR ) LN=LSTR
         IF ( ICAR .EQ. 0 ) THEN
              CALL PRCH ( BUF(J1)(1:LN) )
           ELSE IF ( ICAR .EQ. 1 ) THEN
              CALL PRCH ( CHAR(10)//BUF(J1)(1:LN)//CHAR(13) )
         END IF
 410  CONTINUE
      RETURN
      END  !#!  PRI_BUF  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION HIT_CONT ( STR, NPAR )
! ************************************************************************
! *                                                                      *
! *   Function HIT_CONT
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 13-MAR-1993   HUIT_CONT   v1.2 (d)  L. Petrov  11-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  HIT_CONT, ISIM, NPAR
      CHARACTER  STR*(*), ASIM*1
      LOGICAL*4  FL_OUT_TERM
      INTEGER*8  ADR_LOC
      INTEGER*4  ADR_LOC_I4(2)
      INTEGER*4, EXTERNAL :: INSIM
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif
      ADR_LOC = LOC(STR)
      CALL MEMCPY ( ADR_LOC_I4, ADR_LOC, %VAL(8) )
      IF ( ADR_LOC_I4(1) == 0 .AND. ADR_LOC_I4(2) == 0 ) THEN
           CALL PRCH ( 'Hit any key to proceed  '//CHAR(1) )
       ELSE
           CALL PRCH ( STR )
      END IF
#ifdef SUN
        FL_OUT_TERM = FUNC_ISATTY ( 1 ) ! Flag whether the unit 5 is a terminal
#else
#ifdef GNU
        FL_OUT_TERM = ISATTY      ( 5 ) ! Flag whether the unit 5 is a terminal
#else
        FL_OUT_TERM = FUNC_ISATTY ( 5 ) ! Flag whether the unit 5 is a terminal
#endif
#endif
      IF ( FL_OUT_TERM ) THEN
           HIT_CONT = INSIM ( ASIM, ISIM )
           CALL CLSTR ()
         ELSE
           HIT_CONT = 13
      END IF
      RETURN
      END  !#!  HIT_CONT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE INSTR ( PROMPT, IBG, BEGIN, STR, IUER )
! ************************************************************************
! *                                                                      *
! *     ðïäðòïçòáííá  INSTR  þéôáåô óôòïëõ ó õóôòïêóô÷á SYS$INPUT.       *
! *     åóìé úáäáî ðáòáíåôò PROMPT, ôï ó îï÷ïê óôòïëé ÷ù÷ïäéôóñ óôòïëá   *
! *     PROMPT ( ÷ PROMPT è÷ïóôï÷ùå ðòïâåìù éçîïòéòõàôóñ ). äáìåå, åóìé  *
! *     BEGIN úáäáîá, ÷ù÷ïäéôóñ IBG óéí÷ïìï÷ óôòïëé-ûáâìïîá. ëõòóïò      *
! *     ïóôáôóñ õ óáíïçï ðòá÷ïçï óéí÷ïìá óôòïëé. ðòïâåìá. åóìé          *
! *     úîáþåîéåí ìïçéþåóëïçï éíåîé SYS$INPUT  ñ÷ìñåôóñ éíñ ôåòíéîáìá,   *
! *     ôï ëïíðøàôåò ïöéäáåô ÷÷ïäá óôòïëé. ÷÷ïä úá÷åòûáåôóñ ðï îáöáôéé   *
! *     ëìá÷éûé "÷ïú÷òáô ëáòåôëé" éìé ðï úáðïìîåîéé ÷óåê óôòïëé STR.     *
! *     åóìé ÷÷ïä ïóõýåóô÷ìñåôóñ ó ôåòíéîáìá Microterm 5530 ( çìïâáìøîùê *
! *     óéí÷ïì TERM_TYPE="43" éìé "44" ), ôï ëìá÷éûá F20 ðåòåëìàþáåô     *
! *     òõóóëéê/LATIN áìæá÷éôù.                                          *
! *     åóìé úîáþåîéåí ìïçéþåóëïçï éíåîé SYS$INPUT ñ÷ìñåôóñ éíñ äéóëá,   *
! *     ôï þéôáåôóñ óôòïëá ïðåòáôïòí READ. ÷ üôïí óìõþáå îéëáëïê         *
! *     ðåòåëïäéòï÷ëé îå ðòïú÷ïäéôóñ.                                    *
! *                                                                      *
! * ________________________ ÷èïäîùå ðáòáíåôòù: ________________________ *
! *                                                                      *
! *     PROMPT ( CHARACTER, OPT ) -- óôòïëá îåéúíåîñåíïê ðïäóëáúëé. ðòé  *
! *                                  ÷ù÷ïäå îá ôåòíéîáì ïôóåëáàôóñ       *
! *                                  è÷ïóôï÷ùå ðòïâåìù. ðòé ä÷éöåîéé     *
! *                                  ëõòóïò îå íïöåô îáåèáôø îá óéí÷ïìù  *
! *                                  ðïäóëáúëé.                          *
! *        IBG ( INTEGER*4, OPT ) -- ëïìéþåóô÷ï óéí÷ïìï÷ ÷               *
! *                                  óôòïëå-ûáâìïîå.                     *
! *      BEGIN ( CHARACTER, OPT ) -- éúíîåîñåíáñ óôòïëá-ûáâìïî. ÷ùèïäîáñ *
! *                                  óôòïëá ïâòáúõåôóñ ðõôé éúíåîåîéñ   *
! *                                  ûáâìïîá: éúíåîåîéñ ûáâìïîá,         *
! *                                  äïðéóù÷áîéñ óéí÷ïìï÷ ÷ ëïîåã        *
! *                                  ûáâìïîá é ô.ð. ðòé ÷÷ïäå ëìá÷éûé    *
! *                                  CTRL/U ûáâìïî óôéòáåôóñ.            *
! *                                  ðï õíïìþáîéà ûáâìïîá îåô. ÷ùèïäîáñ  *
! *                                  óôòïëá âõäåô óïúäá÷áôøóñ úáîï÷ï.    *
! *                                                                      *
! * ________________________ ÷ùèïäîùå ðáòáíåôòù: _______________________ *
! *                                                                      *
! *        STR ( CHARACTER ) -- ÷÷åäîîáñ óôòïëá.                        *
! *                                                                      *
! * ___________________ íïäéæéãéòõåíùå ðáòáíåôòù: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- ðáòáíåôò ïûéâëé:                       *
! *             ÷èïäîïå úîáþåîéå  --  òåöéí ïâòáâïôëé ïûéâëé:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- ÷ïú÷òáýåîéå ëïäá ïûéâëé.                             *
! *      IUER=-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ é ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ        *
! *                 ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.                       *
! *      IUER<-1 -- ÷ïú÷òáýåîéå ëïäá IUER=0 ÷ óìõþáå îïòíáìøîïçï         *
! *                 úá÷åòûåîéñ, ÷ù÷ïä äéáçîïóôéþåóëïçï óïïâýåîéñ é       *
! *                 úá÷åòûåîéå ïâòáúá ÷ óìõþáå ÷ïúîéëîï÷åîéñ ïûéâëé.     *
! *      åóìé IUER ïðõýåî, ôï ÷èïäîïå úîáþåîéå ðòéîéíáåôóñ òá÷îùí -1     *
! *             ÷ùèïäîïå úîáþåîéå  --  ëïä ïûéâëé ( åóìé IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             äïóôõðåî äìñ úáðéóé ):                                   *
! *      IUER=0  --  îïòíáìøîïå úá÷åòûåîéå.                              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  09-JUN-92     INSTR     V2.0  (d) ðÅÔÒÏ× ì.à.  20-MAR-95  ###  *
! *
! ************************************************************************
        IMPLICIT   NONE 
        CHARACTER  PROMPT*(*), BEGIN*(*), STR*(*)
        CHARACTER  ISTR*132, BEG*132, OSTR*132
        INTEGER*4  IBG, IUER
        INTEGER*4  IP, IZ, IBB, IBG_R, I1, LGT
        INTEGER*4, EXTERNAL :: ILEN, I_LEN, INTX
!
        CALL CLRCH ( ISTR )
        IF ( LOC(PROMPT) .NE. 0 ) THEN
             ISTR = PROMPT
!
! ---------- õúáí äìéîõ óôòïëé
!
             IF ( LEN(PROMPT) .GT. 0 ) THEN
                  CALL PRCH ( CHAR(10)//CHAR(13)//ISTR ) ! ÷ù÷ïä îá üëòáî
                  LGT=LEN(PROMPT)-ILEN(PROMPT)
                  IF ( LGT .GT. 0 ) THEN
                       CALL CLRCH ( OSTR )
                       IF ( LGT .GT. 131 ) LGT=131
                       OSTR(LGT+1:)=CHAR(1)
                       CALL PRCH ( OSTR )
                  END IF
             END IF
        END IF
        IF ( LOC(IBG) .NE. 0 ) THEN
             IBG_R=IBG
             IF ( IBG_R .GT.132 ) IBG_R = 132
             IF ( IBG_R .LT.0   ) IBG_R = 0
          ELSE
             IBG_R=0
        END IF
!
! ----- ëïðéòï÷áîéå óôòïëé BEGIN ÷ BEG óï ÷óñþåóëéíé ðòåäïóôïòïöîïóôñíé
!
        IF ( IBG_R.GT.0 ) THEN
             IF ( LOC(BEGIN) .NE. 0 ) THEN
                  BEG=BEGIN
               ELSE
                  CALL CLRCH ( BEG )
             END IF
             CALL PRCH ( BEG(1:IBG_R) ) ! ÷ù÷ïä îá üëòáî
             IBB = IBG_R
          ELSE
             IBB = 1
        END IF
!
! ----- õúîáí äìéîõ óôòïëé STR
!
        IP=LEN ( STR )
        IF ( IP.GT.132 ) IP=132
!
! ----- óïâóô÷åîîï ÷÷ïä óôòïëé ó ôåòíéîáìá
!
        IZ=INTX  ( IBG_R, BEG(1:IBB), OSTR(1:IP) )
!#        IF ( IZ.EQ.18888 ) THEN
!
! ---------- õóôòïêóô÷ï  SYS$INPUT  --  îå ôåòíéîáì. þôåîéå ó äéóëá
!
!#             READ ( 5, FMT='(A)', IOSTAT=I1, ERR=710 ) OSTR
!#          ELSE IF ( IZ.NE.1 ) THEN
!#             IF ( IZ.EQ.0 ) IZ=1111
!#             CALL ERR_LOG ( IZ, IUER, 'INSTR', 'ïûéâëá þôåîéñ ó '//
!#     $                      'ôåòíéîáìá' )
!#             RETURN
!#        END IF
!
! ----- ëïðéòï÷áîéå óôòïëé OSTR ÷ STR óï ÷óñþåóëéíé ðòåäïóôïòïöîïóôñíé
!
        STR=OSTR
        CALL ERR_LOG ( 0, IUER )
        RETURN
!
  710   CONTINUE
        CALL ERR_LOG ( I1, IUER, 'INSTR', 'Error in reading' )
        RETURN
        END  !#!  INSTR  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION INTX ( IB, BEG, ZAG )
! ************************************************************************
! *                                                                      *
! *     ðïäðòïçòáííá  INTX  ðòéîéíáåô ó ôåòíéîáìá óôòïëõ ZAG ÷ òåöéíå    *
! *     ðïúîáþîïê áëôé÷áãéé. ÷÷ïä úá÷åòûáåôóñ ìéâï ðòé îáöáôéé ëìá÷éûé   *
! *     <÷ë>, ìéâï ðï úáðïìîåîéé ÷óåê óôòïëé. åóìé ÷÷ïä ïóõýåóô÷ìñåôóñ ó *
! *     ôåòíéîáìá Microterm 5530 ( çìïâáìøîùê óéí÷ïì TERM_TYPE="43",     *
! *     "44" , "33" éìé "34" ), ôï ëìá÷éûá F20 ðåòåëìàþáåô òõóóëéê/LATIN *
! *     áìæá÷éôù.                                                        *
! *     îáöáôéå <CTRL/U> ÷ï ÷òåíñ ÷÷ïäá ïþéýáåô óôòïëõ. òáâïôáåô ëìá÷éûá *
! *     <DEL>.                                                           *
! *                                                                      *
! * ________________________ ÷èïäîùå ðáòáíåôòù: ________________________ *
! *                                                                      *
! *      IB ( INTEGER*4 ) -- äìéîá óôòïëé-ûáâìïîá.                       *
! *     BEG ( CHARACTER ) -- óôòïëá-ûáâìïî.                              *
! *                                                                      *
! * ________________________ ÷ùèïäîùå ðáòáíåôòù: _______________________ *
! *                                                                      *
! *     ZAG ( CHARACTER ) -- ÷ùèïäîáñ óôòïëá.                            *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  18-APR-90      INTX     V3.0  (d) ðÅÔÒÏ× ì.à.  20-MAR-95  ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER ZAG*(*), BEG*(*)
        CHARACTER CC*1
!
!#        NA=NUM$ARG ( )
!#        IF ( NA.NE.3 ) CALL VER$ARG ( 3 )
!
        LIM=LEN(ZAG)
        CALL CLRCH ( ZAG )
        IF ( IB.GE.LIM ) THEN
             ZAG=BEG
             RETURN
        END IF
        IF ( IB.NE.0 ) ZAG(1:IB)=BEG(1:IB)
!
! ----- K  --  ëïìéþåóô÷ï õöå ÷÷åäåîîùè óéí÷ïìï÷
!
        K=IB
        DO 410 J1=1,1024
  910      CONTINUE
           INTX=INSIM ( CC, ICC )  !  ïöéäáîéå ÷÷ïäá ó ôåòíéîáìá óéí÷ïìA CC
!#           IF ( INTX.NE.1 ) RETURN
           IF ( ICC.EQ.10 .OR. ICC.EQ.13 ) GOTO 810   !  <÷ë>
           IF ( ICC.EQ.21 ) THEN       !  CTRL/U
                IF ( K.EQ.0 ) GOTO 410
                CALL CURL   ( K )      !  ëõòóïò ÷ìå÷ï îá K ðïúéãéê
                CALL CLRCH  ( ZAG )   !  ïþéóôëá óôòïëé
                ZAG(K:K) =CHAR(1)
                CALL PRCH   ( ZAG )
                CALL CLRCH  ( ZAG )   !  ïþéóôëá óôòïëé
                CALL ITTOUT ( ' ' )
                CALL CURL ( K )      !  ëõòóïò ÷ìå÷ï îá K ðïúéãéê
                K=0
                GOTO 410
           END IF
           IF ( ICC.EQ.127 .OR. ICC.EQ.8 ) THEN
!
! ------------ òåáëãéñ ðïäðòïçòáííù îá ÷÷ïä
! ------------ óéí÷ïìá 127  <DEL> éìé <BS> ("úáâïê")
!
               IF ( K.EQ.0 ) GOTO 910
               CALL CURL(1)      ! ëõòóïò ÷ìå÷ï
               ZAG(K:K) = ' '
               CALL ITTOUT ( ZAG(K:K) )  ! óôéòáîéå ÷÷åäåîîïçï òáîåå óéí÷ïìá
               CALL CURL(1)      ! ëõòóïò ÷ìå÷ï
               K=K-1             ! äåëòåíåîô ëïìéþåóô÷á ÷÷åäååîîù óéí÷ïìï÷
               GOTO 410
           END IF
           IF ( ICC.LT.32 .OR. ICC.GE.256 ) THEN
!
! -------------- ÷÷åäî îåëïòòåëôîùê óéí÷ïì
!
                 CALL BELL(1)
                 GOTO 910
           END IF
!
! -------- ïâîõìåîéå ZAG ÷ óìõþáå, åóìé üôï îáþáìï ðòéåíá óéí÷ïìï÷
!
           K=K+1
           ZAG(K:K)=CC
           IF ( CC.EQ.' ' ) CALL ITTOUT ( ' ' )  !  ÷ù÷ïä ðòïâåìá
           CALL PRCH ( CC )      !  ÷ù÷ïä óéí÷ïìá
           IF ( K.EQ.LIM ) GOTO 810
  410   CONTINUE
  810   CONTINUE
        RETURN
        END  !#!  INTX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIB$WAIT ( RTIME )
      REAL*8     RTIME
      INTEGER*4  ITIME, IS
      ADDRESS__TYPE  ARR1(2), ARR2(2)
#if defined LINUX || defined DARWIN
      ARR1(1) = RTIME
      ARR1(2) = (RTIME - ARR1(1))*1.E9
      IS = NANOSLEEP ( ARR1, ARR2 )
#else
      ITIME = RTIME
      IF ( RTIME .LT. 1.0D0 ) ITIME = 1
      CALL FUNC_SLEEP ( ITIME )
#endif
      RETURN
      END  !#!  LIB$WAIT  #!#
