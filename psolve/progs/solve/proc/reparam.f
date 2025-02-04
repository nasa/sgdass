      SUBROUTINE REPARAM ( U_STA, UIS_STA, U_BAS, UIS_BAS, R_SOU, RIS_SOU, &
     &                     R_STA, RIS_STA, R_BAS, RIS_BAS, IT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REPARAM  deselects from the solution                      *
! *   sources/stations/baselines from the specified lists and makes      *
! *   reparameterization of the solution.                                *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   U_STA ( INTEGER*4 ) -- Number of stations to have been used in     *
! *                          solution before singularity check.          *
! * UIS_STA ( INTEGER*4 ) -- List of stations to have been used in       *
! *                          solution before singularity check. Array    *
! *                          contains the indices of the station names   *
! *                          in the array ISITN_CHR from prfil.i         *
! *                          Length of the list is U_STA elements.       *
! *   U_BAS ( INTEGER*4 ) -- Number of baselines used in solution before *
! *                          singularity check.                          *
! * UIS_BAS ( INTEGER*4 ) -- List of the baselines used in solution      *
! *                          before singularity check. Array contains    *
! *                          the baseline codes. Length of the list is   *
! *                          U_BAS elements.                             *
! *   R_SOU ( INTEGER*4 ) -- Number of sources to be deselected.         *
! * RIS_SOU ( INTEGER*4 ) -- List of the sources which status            *
! *                          "estimate at least one coordinate" is       *
! *                          forcible tuened off. Array contains the     *
! *                          indices of the source names in the array    *
! *                          ISTRN_CHR from prfil.i  Length of the list  *
! *                          is R_SOU elements.                          *
! *   R_STA ( INTEGER*4 ) -- Number of stations to be deselected         *
! * RIS_STA ( INTEGER*4 ) -- List of the stations to be deselected.      *
! *                          Array contains the indices of the station   *
! *                          names in the array ISITN_CHR from prfil.i   *
! *                          Length of the list is R_STA elements.       *
! *   R_BAS ( INTEGER*4 ) -- Number of baselines to be deselected        *
! * RIS_BAS ( INTEGER*4 ) -- List of the baselines to be deselected.     *
! *                          Array contains the baseline codes.          *
! *                          Length of the list is R_BAS elements.       *
! *      IT ( INTEGER*4 ) -- Verbosity level. If IT=0 no messages will   *
! *                          be written in the spool- and error-files    *
! *                          and at the screen.                          *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  07-JUL-98    REPARAM    v3.2 (c)  L. Petrov  03-OCT-2005  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'prfil.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'obser.i'
      INCLUDE    'precm.i'
      INCLUDE    'cnstr.i'
      INCLUDE    'glbcm.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  U_STA, UIS_STA(*), U_BAS, UIS_BAS(*), R_SOU, RIS_SOU(*), &
     &           R_STA, RIS_STA(*), R_BAS, RIS_BAS(*), IUER
      CHARACTER  OUT*65536, FNAME*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19
      INTEGER*4  L_STA, LIS_STA(MO_STA), L_BAS, LIS_BAS(MO_BAS), &
     &           L_TRI, LIS_TRI(3,MO_TRI), IP, IP1, IP2, IT, IREF(M_GPA), &
     &           IER
      INTEGER*2  NO_PAR, NN_PAR
      CHARACTER  CO_PAR(M_GPA)*20, CN_PAR(M_GPA)*20
      LOGICAL*4  L_BC, ULCC_EXIST, FL_REPAR
      LOGICAL*2  FL_TRUE_L2, FL_FALSE_L2, KBIT
      PARAMETER  ( FL_TRUE_L2  = .TRUE.  )
      PARAMETER  ( FL_FALSE_L2 = .FALSE. )
      CHARACTER  GET_DBNAME*10
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: DATYP_INQ, CHECK_STABIT
      INTEGER*4, EXTERNAL :: ILEN, IFIND_PL
!
! --- Check whether the file with user local constraint exists
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ULCC'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=ULCC_EXIST  )
      IF ( ULCC_EXIST ) THEN
!
! -------- In the case if a file with user local constrait exists we have
! -------- to get current (old) parameter list
!
           CALL GET_NAMES ( CO_PAR, INT2(20), M_GPA, NO_PAR, FL_TRUE_L2, &
     &                      FL_FALSE_L2 )
      END IF
!
! === SOURCES
!     ~~~~~~~
!
      IF ( R_SOU .GT. 0 ) THEN
!
! -------- Lifting flag "estimation of coordinates" from the sources
! -------- which appeared to be in the list RIS_SOU
!
           DO 410 J1=1,NUMSTR
              DO 420 J2=1,R_SOU
                 IF ( RIS_SOU(J2) .EQ. J1  ) THEN
!
! ------------------- Lifting bit "estimation" from both right ascension and
! ------------------- declination
!
                      CALL SBIT ( LSTAR(1,1), INT2(J1), INT2(0) )
                      CALL SBIT ( LSTAR(1,2), INT2(J1), INT2(0) )
                 END IF
 420          CONTINUE
 410       CONTINUE
!
           IF ( IT .GT. 0 ) THEN
!
! ------------- Generating the line with information message
!
                CALL CLRCH ( OUT )
                IF ( R_SOU .EQ. 1 ) THEN
                     OUT = 'REPARAM: coodrinates of the source'
                   ELSE
                     OUT = 'REPARAM: coodrinates of the sources'
                END IF
!
                DO 430 J3=1,R_SOU
                   IP = ILEN(OUT)+2
                   OUT = OUT(1:IP-1)//ISTRN_CHR(RIS_SOU(J3))
 430            CONTINUE
                IP = ILEN(OUT)+2
!
                IP = ILEN(OUT)+2
                OUT = OUT(1:IP-1)//'are not estimated'
!
! ------------- Print the line and write at the screen and in spool-file
!
                CALL WRITE_LONG (  6, 79, OUT )
                CALL WRITE_LONG ( 23, 79, OUT )
           END IF
      END IF
!
! === STATIONS
!     ~~~~~~~~
!
      IF ( R_STA .GT. 0 ) THEN
!
! -------- Station deselection
!
           DO 440 J4=1,NUMSTA-1
              DO 450 J5=J4+1,NUMSTA
                 DO 460 J6=1,R_STA
                    IF ( RIS_STA(J6) .EQ. J4  .OR.  RIS_STA(J6) .EQ. J5 ) THEN
                         IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! --------------------------- Lifting bit: participation in phase delay
! --------------------------- solution from the baselines at the station under
! --------------------------- consideration
!
                              CALL SBIT ( IBLSEL_P(1,J4), INT2(J5), INT2(0) )
                              CALL SBIT ( IBLSEL_P(1,J5), INT2(J4), INT2(0) )
                           ELSE
!
! --------------------------- Lifting bit: participation in group delay
! --------------------------- solution from the baselines at the station under
! --------------------------- consideration
!
                              CALL SBIT ( IBLSEL_G(1,J4), INT2(J5), INT2(0) )
                              CALL SBIT ( IBLSEL_G(1,J5), INT2(J4), INT2(0) )
                         END IF
                    END IF
 460             CONTINUE
 450          CONTINUE
 440       CONTINUE
!
! -------- Setting bit fields of participation of the stations
! -------- in the solutions.
!
           CALL SET_STABIT ( INT2(2) )
!
           IF ( IT .GT. 0 ) THEN
!
! ------------- Generating the line with information message
!
                CALL CLRCH ( OUT )
                IF ( R_STA .EQ. 1 ) THEN
                     OUT = 'REPARAM: station'
                   ELSE
                     OUT = 'REPARAM: stations'
                END IF
!
                DO 470 J7=1,R_STA
                   IP = ILEN(OUT)+2
                   OUT = OUT(1:IP-1)//ISITN_CHR(RIS_STA(J7))
 470            CONTINUE
                IP = ILEN(OUT)+2
!
                IF ( R_STA .EQ. 1 ) THEN
                     OUT = OUT(1:IP-1)//'is'
                   ELSE
                     OUT = OUT(1:IP-1)//'are'
                END IF
!
                IP = ILEN(OUT)+2
                OUT = OUT(1:IP-1)//'excluded from solution '//DBNAME_CH
!
! ------------- Print the line and write at the screen and in spool-file
!
                CALL WRITE_LONG (  6, 79, OUT )
                CALL WRITE_LONG ( 23, 79, OUT )
           END IF
      END IF
!
! === BASELINES
!     ~~~~~~~~~
!
      IF ( R_BAS .GT. 0 ) THEN
!
! -------- Station deselection
!
           DO 480 J8=1,NUMSTA-1
              DO 490 J9=J8+1,NUMSTA
                 DO 4100 J10=1,R_BAS
                    CALL NBAST ( RIS_BAS(J10), IP1, IP2 )
                    IF ( ( IP1 .EQ. J8  .AND.  IP2 .EQ. J9 ) .OR. &
     &                   ( IP1 .EQ. J9  .AND.  IP2 .EQ. J8 )      ) THEN
!
                         IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! --------------------------- Lifting bit: participation in phase delay
! --------------------------- solution from the baselines at the station under
! --------------------------- consideration. Lifting also bits of estimation
! --------------------------- of baseline-dependent clocks
!
                              CALL SBIT ( IBLSEL_P(1,J8), INT2(J9), INT2(0) )
                              CALL SBIT ( IBLSEL_P(1,J9), INT2(J8), INT2(0) )
                              CALL SBIT ( ICLOCK(1,J8),   INT2(J9), INT2(0) )
                              CALL SBIT ( ICLOCK(1,J9),   INT2(J8), INT2(0) )
                           ELSE
!
! --------------------------- Lifting bit: participation in group delay
! --------------------------- solution from the baselines at the station under
! --------------------------- consideration. Lifting also bits of estimation
! --------------------------- of baseline-dependent clocks
!
                              CALL SBIT ( IBLSEL_G(1,J8), INT2(J9), INT2(0) )
                              CALL SBIT ( IBLSEL_G(1,J9), INT2(J8), INT2(0) )
                              CALL SBIT ( ICLOCK(1,J8),   INT2(J9), INT2(0) )
                              CALL SBIT ( ICLOCK(1,J9),   INT2(J8), INT2(0) )
                         END IF
                    END IF
 4100             CONTINUE
 490          CONTINUE
 480       CONTINUE
!
! -------- Setting bit fields of participation of the stations
! -------- in the solutions.
!
           CALL SET_STABIT ( INT2(2) )
!
           IF ( IT .GT. 0 ) THEN
!
! ------------- Generating the line with information message
!
                CALL CLRCH ( OUT )
                IF ( R_BAS .EQ. 1 ) THEN
                     OUT = 'REPARAM: baseline'
                   ELSE
                     OUT = 'REPARAM: baselines'
                END IF
!
                DO 4110 J11=1,R_BAS
                   IP = ILEN(OUT)+2
                   CALL NBAST ( RIS_BAS(J11), IP1, IP2 )
                   OUT = OUT(1:IP-1)//ISITN_CHR(IP1)//'/'//ISITN_CHR(IP2)//' ,'
 4110           CONTINUE
                IP = ILEN(OUT)
!
                IF ( R_BAS .EQ. 1 ) THEN
                     OUT = OUT(1:IP-2)//' is'
                   ELSE
                     OUT = OUT(1:IP-2)//' are'
                END IF
!
                IP = ILEN(OUT)+2
                OUT = OUT(1:IP-1)//'excluded from solution'
!
! ------------- Print the line and write at the screen and in spool-file
!
                CALL WRITE_LONG (  6, 79, OUT )
                CALL WRITE_LONG ( 23, 79, OUT )
           END IF
      END IF
!
      IF ( R_STA .GT. 0  .OR.  R_BAS .GT. 0 ) THEN
!
! -------- Check: if baseline dependent clocks are estimated at least once
! -------- then L_BC is .TRUE.
!
           L_BC = .FALSE.
           DO 4120 J12=1,INT4(NUMSTA)
              IF ( CHECK_STABIT( INT2(J12) )  ) THEN
                   DO 4130 J13=J12+1,INT4(NUMSTA)
                      IF ( CHECK_STABIT( INT2(J13) )  ) THEN
                           IF ( KBIT( ICLOCK(1,INT2(J13)), INT2(J12) ) .OR. &
     &                          KBIT( ICLOCK(1,INT2(J12)), INT2(J13) )    ) THEN
!
                                L_BC = .TRUE.
                           END IF
                      END IF
 4130              CONTINUE
              END IF
 4120      CONTINUE
!
           IF ( L_BC ) THEN
!
! ------------- Well. We have such a case: the number of baselines were changed
! ------------- and baseline dependent clocks were estimated for at least
! ------------- one baseline. We need re-parameterize setup of baseline
! ------------- dependent clocks. We use "maximal" algorithm to select max
! ------------- number of baseline dependent clocks which still doesn't make
! ------------- normal matrix singular.
!
! ------------- Frist, build the list of stations and baselines which will
! ------------- be used in the solution after re-parameterization.
!
                IF ( R_STA .GT. 0 ) THEN
                     L_STA = 0
                     DO 4140 J14=1,U_STA
                        IF ( IFIND_PL ( R_STA, RIS_STA, UIS_STA(J14) ) .LE. &
     &                       0 )THEN
!
! -------------------------- You see: we add to the list LIS_STA only the
! -------------------------- stations which are not in the RIS_STA list
!
                             CALL ADD_LIS ( MO_STA, L_STA, LIS_STA, &
     &                                      UIS_STA(J14), -3 )
                        END IF
 4140                CONTINUE
                   ELSE
                     L_STA = U_STA
                     CALL COPY_I4 ( L_STA, UIS_STA, LIS_STA )
                END IF
!
! ------------- The same with baseline list
!
                IF ( R_BAS .GT. 0 ) THEN
                     L_BAS = 0
                     DO 4150 J15=1,U_BAS
                        IF ( IFIND_PL ( R_BAS, RIS_BAS,  UIS_BAS(J15) ) .LE. 0 .AND. &
     &                       IFIND_PL ( R_BAS, RIS_BAS, -UIS_BAS(J15) ) .LE. 0       ) THEN
!
! -------------------------- You see: we add to the list LIS_BAS only the
! -------------------------- baselines which are not in the RIS_BAS list
!
                             CALL ADD_LIS ( MO_BAS, L_BAS, LIS_BAS, &
     &                                      UIS_BAS(J15), -3 )
                        END IF
 4150                CONTINUE
                   ELSE
                     L_BAS = U_BAS
                     CALL COPY_I4 ( L_BAS, UIS_BAS, LIS_BAS )
                END IF
!
! ------------- Then, build list of linearly independent closed triangles
!
                CALL ERR_PASS ( IUER, IER )
                CALL TRI_GRP  ( L_STA, LIS_STA, L_BAS, LIS_BAS, MO_TRI, &
     &                          L_TRI, LIS_TRI, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8431, IUER, 'REPARAM', 'Error during '// &
     &                   'building list of closed triangles' )
                     RETURN
                END IF
!
! ------------- Then set baseline dependent clocks
!
                CALL ERR_PASS   ( IUER, IER )
                CALL SET_BASCLK ( L_TRI, LIS_TRI, L_BAS, LIS_BAS, L_STA, &
     &                            LIS_STA, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8432, IUER, 'REPARAM', 'Error during '// &
     &                   'setting up estimation flag for baseline dependent '// &
     &                   'clocks' )
                     RETURN
                END IF
!
                IF ( IT .GT. 0 ) THEN
                     CALL CLRCH ( OUT )
                     OUT = 'REPARAM: baseline-dependent clocks were reset up'
                     CALL WRITE_LONG (  6, 79, OUT )
                     CALL WRITE_LONG ( 23, 79, OUT )
                END IF
           END IF
      END IF
!
! --- Recalculate the number of parameters
!
      CALL PARCN()
!
! --- ... and to write them in socom block
!
      CALL USE_COMMON ( 'OWC' )
!
      IF ( ULCC_EXIST ) THEN
!
! -------- In the case if there were constraints imposed on local parameters
! -------- we have to check whether parameters list was changed.
! -------- First get the new parameters list
!
           CALL NOUT_I4   ( M_GPA, IREF )
           CALL GET_NAMES ( CN_PAR, INT2(20), M_GPA, NN_PAR, FL_TRUE_L2, &
     &                      FL_FALSE_L2 )
           FL_REPAR = .FALSE.
!
! -------- Now build the corss reference talbe IREF (from old to new)
!
           DO 4160 J16=1,NO_PAR
              DO 4170 J17=1,NN_PAR
                 DO 4180 J18=1,20
                    IF ( CN_PAR(J17)(J18:J18) .NE. CO_PAR(J16)(J18:J18)) &
     &                   GOTO 4170
 4180            CONTINUE
                 IREF(J16) = J17
                 GOTO 4160
 4170         CONTINUE
              IF ( IREF(J16) .NE. J16 ) FL_REPAR = .TRUE.
 4160     CONTINUE
!
! ------- If total number of paramters was changed or at least on pameter
! ------- was changed then we have to repair parameters iindexing in user
! ------- local constraint data structure
!
          IF ( NO_PAR .NE. NN_PAR ) FL_REPAR = .TRUE.
!
          IF ( FL_REPAR ) THEN
!
! ------------ Initialize object CNSTROBJ
!
               CALL ERR_PASS ( IUER, IER )
               CALL INIT_CNS ( CNI__ULC, GET_DBNAME(), CNSTROBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8433, IUER, 'REPARAM', 'Failure in an '// &
     &                  'attempt to initialize CNSTROBJ' )
                    RETURN
               END IF
!
! ------------ Read usre constraints
!
               CALL ERR_PASS   ( IUER, IER )
               CALL READ_CNSTR ( CNSTROBJ, CNI__ULC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8434, IUER, 'REPARAM', 'Error in an '// &
     &                  'attempt to read user local constraints in CNSTROBJ '// &
     &                  'object from scratch file '//FNAME )
                    RETURN
               END IF
!
! ------------ ... and replace the old index of the parameter with the
! ------------ new one. Of course, it will work if this parameter was not
! ------------ eliminated during re-parameterization
!
               DO 4190 J19=1,CNSTROBJ%N_ECNST
                  IF ( IREF(CNSTROBJ%EQU_INP(J19)) .LE. 0 ) THEN
                       CALL ERR_LOG ( 8435, IUER, 'REPARAM', 'Trap of '// &
     &                     'internal control: user program has imposed '// &
     &                     'constraint on parameter '// &
     &                      CO_PAR(CNSTROBJ%EQU_INP(J19))//' but this '// &
     &                     'parameter was eliminated during '// &
     &                     'reparameterization. Solve currently does not '// &
     &                     'support such a situation' )
                       RETURN
                  END IF
                  CNSTROBJ%EQU_INP(J19) = IREF(CNSTROBJ%EQU_INP(J19))
 4190          CONTINUE
!
! ------------ And eventually write updated list back
!
               CALL ERR_PASS   ( IUER, IER )
               CALL WRITE_CNSTR ( CNSTROBJ, CNI__ULC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8436, IUER, 'REPARAM', 'Error in an '// &
     &                  'attempt to read user local constraints in '// &
     &                  'CNSTROBJ object from scratch file '//FNAME )
                    RETURN
               END IF
!
          END IF
      END IF
!
      IF ( USER_PROG_NAME(1:1) .NE. ' '       .AND.  &
     &     USER_PROG_NAME(1:4) .NE. 'NONE'    .AND.  &
     &     INDEX ( USER_PROG_BUFF, '&' ) > 0         ) THEN
!
           IF ( USER_PROG_BUFF(1:1) .EQ. 'Y' ) THEN
                CALL USE_BUFFER ( %REF(USER_PROG_BUFF(2:81)), INT2(40), 'OWC' )
           ENDIF
!
! -------- Add '1' to argument list to indicate that program name
! -------- includes entire path  (MWH - 910418)
!
           CALL RUN_PROG ( USER_PROG_NAME, 'WAIT', INT2(1) )
!
! -------- Re-read socom and parfil since user program may change it
!
           CALL USE_COMMON ( 'ORC' )
           SOCOM_PLUS_FIRST = SPL__UNDF
           CALL SOCOM_EXT()
           CALL USE_PARFIL ( 'ORC' )
           CALL PARCN()
         ELSE 
!
! -------- Update socom_plus block
!
           SOCOM_PLUS_FIRST = SPL__UNDF 
           CALL SOCOM_EXT ()
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPARAM  #!#
