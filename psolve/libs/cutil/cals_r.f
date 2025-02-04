      SUBROUTINE CALS_R ( NUMDB2, IVRB, IREG, CAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CALS_R  reads NAMFIL and fills fields data structure      *
! *   CALS which keep information about contributions/calibrations       *
! *   of SOLVE, their availability and status (applied or not applied).  *
! *   CALS_R  transforms information about calibration from very         *
! *   peculiar form as it stored in SOLVE to the form which can be       *
! *   easily used for programming.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  NUMDB2 ( INTEGER*2 ) -- Internal number of the database in scratch  *
! *                          file.                                       *
! *    IVRB ( INTEGER*4 ) -- Verbosity level.                            *
! *                          IVRB=0 -- silent mode;                      *
! *                          IVRB=1 -- debugging information is printed  *
! *                          in screen.                                  *
! *    IREG ( INTEGER*4 ) -- If ( IREG = 1 ) then all text information   *
! *                          in CAL data structures will be transformed  *
! *                          to upper register.                          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     CAL ( RECORD    ) -- Data structure which keeps: 1) station      *
! *                          names; 2) Station-dependent calibration     *
! *                          names, lcodes associated with calibrations, *
! *                          their status for each station; 3) Zenith    *
! *                          calibrations, their status for each         *
! *                          station; 4) observation-dependent           *
! *                          contributions names, lcodes associated with *
! *                          contributions, their status; 5) mode        *
! *                          calibration names, lcodes associated with   *
! *                          mode calibrations, their status;            *
! *                          6) ionosphere calibration status.           *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  01-SEP-97     CALS_R     v3.1  (c)  L. Petrov 06-MAR-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'cals.i'
      INTEGER*2  NUMDB2
      INTEGER*4  IUER
      TYPE ( CALS_STRU ) ::  CAL
!C
      INTEGER*4  IVRB, IREG
      INTEGER*2  IER2, NCAL2, NFCAL2, NZCAL2, NOBCAL2, NMCAL2
      INTEGER*2  ION_STS, ICAL_AV, ICAL_AP, JCAFFL(7), OBCAVL, OBCAPL, &
     &           MCAVL, MCAPL
      CHARACTER  NAMBUF*120, STR*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IO, NN
!
! --- Initialization
!
      CAL%L_STA  = 0
      CAL%L_CONT = 0
      CAL%L_SCAL = 0
      CAL%L_ZENC = 0
      CAL%L_MCAL = 0
      CAL%STATUS = CAL__UNF
!
! --- Openning NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Reading card with the number of calibtrations
!
      CALL GETCARD ( NUMDB2, 'CLCT', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6401, IUER, 'CALS_R', 'Error in reading CLCT card' )
           RETURN
      END IF
      READ ( NAMBUF, "(4X,5(1X,I3),46X)", IOSTAT = IO ) NCAL2, NFCAL2, NZCAL2, &
     &                                                  NOBCAL2, NMCAL2
      IF ( IO .NE. 0 ) THEN
           CALL ERR_LOG ( 6402, IUER, 'CALS_R', 'Error in deciphering '// &
     &                   'CLCT card' )
           RETURN
      END IF
!
      CAL%L_CONT = NOBCAL2
      CAL%L_SCAL = NCAL2
      CAL%L_ZENC = NFCAL2
      CAL%L_MCAL = NMCAL2
      CAL%NZCAL2_SAVE = NZCAL2
!
! --- Reading the first card CALS with station-dependent calibrations
!
      CALL GETCARD ( NUMDB2, 'CALS', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6403, IUER, 'CALS_R', 'Error in reading CALS card' )
           RETURN
      END IF
      DO 410 J1=1,MAXC_STA
!
! ------ Deciphering the previous card
!
         READ ( NAMBUF, FMT='(5X, A8, 1X, 3I7, 35X)', IOSTAT = IO) &
     &          STR, ION_STS, ICAL_AV, ICAL_AP
         IF ( IO .NE. 0 ) THEN
              CALL ERR_LOG ( 6404, IUER, 'CALS_R', 'Error in deciphering '// &
     &                      'CALS card' )
              RETURN
         END IF
         CAL%L_STA = J1
         IF ( IREG .EQ. 1 ) THEN
              CALL TRAN ( 11, STR, CAL%STANAM(J1) ) ! transf. to upper registr
            ELSE
              CAL%STANAM(J1) = STR
         END IF
!
! ------ Reading the bits of station-dependent CALIBRATION availability and
! ------ apply status for this station
!
         DO 420 J2=1,MC_SCAL
            CAL%SCAL_AVL(J2,J1) = BTEST ( ICAL_AV, J2-1 )
            CAL%SCAL_APL(J2,J1) = BTEST ( ICAL_AP, J2-1 )
 420     CONTINUE
!
! ------ Getting ionopshere status
!
         CAL%GION_AVL(J1) = BTEST ( ION_STS, 1-1 )
         CAL%PION_AVL(J1) = BTEST ( ION_STS, 2-1 )
         CAL%GION_APL(J1) =       BTEST ( ION_STS, 4-1 ) .AND. &
     &                      .NOT. BTEST ( ION_STS, 5-1 )
         CAL%PION_APL(J1) = .NOT. BTEST ( ION_STS, 4-1 ) .AND. &
     &                            BTEST ( ION_STS, 5-1 )
!
! ------ Reading the next card
!
         CALL GETCARD ( NUMDB2, 'CALS', INT2(0), NAMBUF, IER2 )
         IF ( IER2 .EQ. INT2(1) ) GOTO 810
         IF ( IER2 .NE. 0 ) THEN
              CALL ERR_LOG ( 6405, IUER, 'CALS_R', 'Error in reading '// &
     &                      'CALS card' )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Reading CALN-card -- the names of station-dependent calibrations
!
      CALL GET_CLN_CARD ( NUMDB2, 'CALN', NCAL2, CAL%SCAL, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6406, IUER, 'CALS_R', 'Error in reading CALN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Reading CALI-card -- the lcodes of station-dependent calibrations
!
      CALL GET_CLN_CARD ( NUMDB2, 'CALI', NCAL2, CAL%SCAL_LCODE, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6407, IUER, 'CALS_R', 'Error in reading CALN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Reading the first card of zenith calibrations functions
!
      CALL GETCARD ( NUMDB2, 'FCLS', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6408, IUER, 'CALS_R', 'Error in reading CALS card' )
           RETURN
      END IF
      DO 430 J3=1,MAXC_STA
!
! ------ Deciphering the previous card of zenith calibrations functions
!
         READ ( NAMBUF, FMT='(5X,A8, 7(1X,I7), 1X)', IOSTAT = IO) &
     &          STR, ( JCAFFL(NN), NN=1,7 )
         IF ( IO .NE. 0 ) THEN
              CALL ERR_LOG ( 6409, IUER, 'CALS_R', 'Error in deciphering '// &
     &                      'CALS card' )
              RETURN
         END IF
!
! ------ Reading bytes of zenith calibrations
!
         DO 440 J4=1,MC_ZENC
            CAL%ZENC_AVL(J4,J3) = .TRUE.
            CAL%ZENC_APL(J4,J3) = BTEST ( JCAFFL(1), J4-1 )
 440     CONTINUE
!
! ------ Reading the next card of zenith calibrations
!
         CALL GETCARD ( NUMDB2, 'FCLS', INT2(0), NAMBUF, IER2 )
         IF ( IER2 .EQ. INT2(1) ) GOTO 830
         IF ( IER2 .NE. 0 ) THEN
              CALL ERR_LOG ( 6410, IUER, 'CALS_R', 'Error in reading '// &
     &                      'CALS card' )
              RETURN
         END IF
 430  CONTINUE
 830  CONTINUE
!
! --- Reading card FCLN -- names of zenith calibrations
!
      CALL GET_CLN_CARD ( NUMDB2, 'FCLN', NFCAL2, CAL%ZENC, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6411, IUER, 'CALS_R', 'Error in reading CALN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Reading card CONT -- status of observation-dependent contributions
!
      CALL GETCARD ( NUMDB2, 'CONT', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6412, IUER, 'CALS_R', 'Error in reading CONT '// &
     &                       'card' )
           RETURN
      END IF
      READ ( NAMBUF, FMT='(5X, 4I7, 37X)', IOSTAT = IO ) OBCAVL, OBCAPL, &
     &                                                   MCAVL, MCAPL
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6413, IUER, 'CALS_R', 'Error in deciphering CONT '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Reading bits of observation-dependent contributions status
!
      DO 450 J5=1,NOBCAL2
         CAL%CONT_AVL(J5) = BTEST ( OBCAVL, J5-1 )
         CAL%CONT_APL(J5) = BTEST ( OBCAPL, J5-1 )
 450  CONTINUE
!
      IF ( CAL%L_MCAL .GT. 0 ) THEN
!
! -------- Reading bits of mode calibration status
!
           DO 460 J6=1,NMCAL2
              CAL%MCAL_AVL(J6) = BTEST ( MCAVL, J6-1 )
              CAL%MCAL_APL(J6) = BTEST ( MCAPL, J6-1 )
 460       CONTINUE
      END IF
!
! --- Rerading CNTN-card -- the names of observation-dependent contributions
!
      CALL GET_CLN_CARD ( NUMDB2, 'CNTN', NOBCAL2, CAL%CONT, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6414, IUER, 'CALS_R', 'Error in reading CNTN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Rerading CNTI-card -- the lcodes of observation-dependent contributions
!
      CALL GET_CLN_CARD ( NUMDB2, 'CNTI', NOBCAL2, CAL%CONT_LCODE, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6415, IUER, 'CALS_R', 'Error in reading CNTN '// &
     &                       'card' )
           RETURN
      END IF
!
      IF ( CAL%L_MCAL .GT. 0 ) THEN
!
! -------- Reading CNTN-card -- the names of observation-dependent
! -------- contributions
!
           CALL GET_CLN_CARD ( NUMDB2, 'MCAL', NMCAL2, CAL%MCAL, IER2 )
           IF ( IER2 .NE. 0 ) THEN
                CALL ERR_LOG ( 6416, IUER, 'CALS_R', 'Error in reading MCAL '// &
     &                         'card' )
                RETURN
           END IF
!
! -------- Reading MCLI-card -- the lcodes of observation-dependent
! -------- contributions
!
           CALL GET_CLN_CARD ( NUMDB2, 'MCLI', NMCAL2, CAL%MCAL_LCODE, IER2 )
           IF ( IER2 .NE. 0 ) THEN
                CALL ERR_LOG ( 6417, IUER, 'CALS_R', 'Error in reading MCLI '// &
     &                        'card' )
                RETURN
           END IF
      END IF
!
      CAL%STATUS = CAL__DONE
!
! --- Transformation of all names to the upper registr
!
      IF ( IREG .EQ. 1 ) THEN
           DO 470 J7=1,CAL%L_SCAL
              CALL TRAN ( 11, CAL%SCAL(J7), CAL%SCAL(J7) )
 470       CONTINUE
!
           DO 480 J8=1,CAL%L_ZENC
              CALL TRAN ( 11, CAL%ZENC(J8), CAL%ZENC(J8) )
 480       CONTINUE
!
           DO 490 J9=1,CAL%L_CONT
              CALL TRAN ( 11, CAL%CONT(J9), CAL%CONT(J9) )
 490       CONTINUE
!
           IF ( CAL%L_MCAL .GT. 0 ) THEN
                DO 4100 J10=1,CAL%L_MCAL
                   CALL TRAN ( 11, CAL%MCAL(J10), CAL%MCAL(J10) )
 4100           CONTINUE
           END IF
      END IF
!
! --- End of work!
!
      IF ( IVRB .GE. 1 ) THEN
         WRITE ( 6, 110 )  ( NN, CAL%SCAL(NN), CAL%SCAL_LCODE(NN), NN=1, &
     &   CAL%L_SCAL )
 110     FORMAT ( 1X,' I= ',I2,'  CAL%SCAL   >>',A, &
     &                         '<<   CALS.SCAL_LCODE   >>',A,'<<' )
!
         WRITE ( 6, 120 )  ( NN, CAL%ZENC(NN), NN=1,CAL%L_ZENC )
 120     FORMAT ( 1X,' I= ',I2,'  CAL%ZENC   >>',A,'<<  ' )
!
         WRITE ( 6, 130 )  ( NN, CAL%CONT(NN), CAL%CONT_LCODE(NN), CAL%CONT_AVL(NN), &
     &               CAL%CONT_APL(NN), NN=1,CAL%L_CONT )
 130     FORMAT ( 1X,' I= ',I2,'  CAL%CONT >>',A,'<<   CAL%CONT_LCODE >>',A, &
     &               '<<  AVL: ',L1,'  APL: ',L1 )
!
         WRITE ( 6, 140 )
 140     FORMAT ( '   N   Station     SCAL_AVL  ','    SCAL_APL      ZENC_APL   |', &
     &            ' GION   PION' )
         DO 510 J1=1,CAL%L_STA
            WRITE ( 6, 150 )  J1, CAL%STANAM(J1), ( CAL%SCAL_AVL(NN,J1), &
     &                                    NN=1,MC_SCAL ),( CAL%SCAL_APL(NN, &
     &                                    J1), NN=1,MC_SCAL ),( CAL%ZENC_APL(NN,J1), NN=1,MC_ZENC ), &
     &                                      CAL%GION_AVL(J1), CAL%GION_APL(J1), &
     &                                      CAL%PION_AVL(J1), CAL%PION_APL(J1)
 150        FORMAT ( 'I=',I2,' >>',A,'<<  ',10L1,'    ',10L1,'    ',10L1,' | ', &
     &               2L1,'     ',2L1 )
 510     CONTINUE
!
         IF ( CAL%L_MCAL .GT. 0 ) THEN
!
              WRITE ( 6, 160 )  ( NN, CAL%MCAL(NN), CAL%MCAL_LCODE(NN), &
     &                        CAL%MCAL_AVL(NN), CAL%MCAL_APL(NN), &
     &                        NN=1,CAL%L_MCAL )
 160          FORMAT ( 1X,' I= ',I2,'  CAL%MCAL >>',A,'<<   CAL%MCAL_LCODE >>', &
     &                  A,'<<  AVL: ',L1,'  APL: ',L1 )
            ELSE
              WRITE ( 6, * ) ' MCAL = 0'
         END IF
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  CALS_R  #!#
