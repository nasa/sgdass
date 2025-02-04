      SUBROUTINE CALS_W ( NUMDB2, CAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CALS_W  writes data structure CALS  in NAMFIL.            *
! *   It is assumed that CAL datastructure were initially formed         *
! *   by CALS_R.                                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  NUMDB2 ( INTEGER*2 ) -- internal number of the database in scratch  *
! *                          file.                                       *
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
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  Who  When        What                                               *
! *  pet  2002.03.05  Fixed the bug; the previous versin did not write   *
! *                   CALI lcode names.                                  *
! *                                                                      *
! *  ###  20-AUG-98     CALS_W     v2.1  (c) L. Petrov  05-MAR-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'cals.i'
      INCLUDE   'namfl.i'
      INTEGER*2  NUMDB2
      INTEGER*4  IUER
      TYPE ( CALS_STRU ) ::  CAL
!C
      INTEGER*2  IER2, NCAL2, NFCAL2, NZCAL2, NOBCAL2, NMCAL2, IOP_I2
      INTEGER*2  ION_STS, ICAL_AV, ICAL_AP, JCAFFL(15), OBCAVL, OBCAPL, &
     &           MCAVL, MCAPL
      CHARACTER  NAMBUF*120
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IO, NN
!
! --- Openning NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Reading the card with the number of calibtrations
!
      CALL GETCARD ( NUMDB2, 'CLCT', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6421, IUER, 'CALS_W', 'Error in reading CLCT card' )
           RETURN
      END IF
!
      READ ( NAMBUF, "(4X,5(1X,I3),46X)", IOSTAT = IO ) NCAL2, NFCAL2, NZCAL2, &
     &       NOBCAL2, NMCAL2
      IF ( IO .NE. 0 ) THEN
           CALL ERR_LOG ( 6422, IUER, 'CALS_W', 'Error in deciphering '// &
     &                   'CLCT card' )
           RETURN
      END IF
!
      NCAL2   = CAL%L_SCAL
      NFCAL2  = CAL%L_ZENC
      NOBCAL2 = CAL%L_CONT
      NMCAL2  = CAL%L_MCAL
!
      WRITE ( NAMBUF, "(A4,5(1X,I3),46X)", IOSTAT = IO ) 'CLCT', NCAL2, NFCAL2, &
     &                  NZCAL2, NOBCAL2, NMCAL2
      IF ( IO .NE. 0 ) THEN
           CALL ERR_LOG ( 6423, IUER, 'CALS_W', 'Error in encoding '// &
     &                   'CLCT card' )
           RETURN
      END IF
!
! --- Writing card with the number of calibtrations
!
      CALL PUTCARD ( NUMDB2, 'CLCT', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           WRITE ( 6, * ) ' ier2 = ',ier2
           CALL ERR_LOG ( 6424, IUER, 'CALS_W', 'Error in writing CLCT card' )
           RETURN
      END IF
      DO 410 J1=1,CAL%L_STA
!
! ------ Setting the bits of station-dependent calibration availability and
! ------ apply status for this station
!
         ICAL_AV = 0
         ICAL_AP = 0
         DO 420 J2=1,MC_SCAL
            IF ( CAL%SCAL_AVL(J2,J1) ) CALL SBIT ( ICAL_AV, INT2(J2), INT2(1) )
            IF ( CAL%SCAL_APL(J2,J1) ) CALL SBIT ( ICAL_AP, INT2(J2), INT2(1) )
 420     CONTINUE
!
! ------ Setting ionopshere status
!
         ION_STS = 0
         IF ( CAL%GION_AVL(J1) ) CALL SBIT ( ION_STS, INT2(1), INT2(1) )
         IF ( CAL%PION_AVL(J1) ) CALL SBIT ( ION_STS, INT2(2), INT2(1) )
         IF ( CAL%GION_APL(J1) ) CALL SBIT ( ION_STS, INT2(4), INT2(1) )
         IF ( CAL%PION_APL(J1) ) CALL SBIT ( ION_STS, INT2(5), INT2(1) )
!
         WRITE ( NAMBUF, FMT='(A4, 1X, A8, 1X, 3I7, 35X)', IOSTAT = IO) &
     &           'CALS', CAL%STANAM(J1), ION_STS, ICAL_AV, ICAL_AP
         IF ( IO .NE. 0 ) THEN
              CALL ERR_LOG ( 6425, IUER, 'CALS_W', 'Error in encoding '// &
     &                      'CALS card' )
              RETURN
         END IF
!
         IF ( J1 .EQ. 1 ) THEN
              IOP_I2 = INT2(1)
            ELSE
              IOP_I2 = INT2(0)
         END IF
!
! ------ Writing the first card CALS with station-dependent calibrations
!
         CALL PUTCARD ( NUMDB2, 'CALS', IOP_I2, NAMBUF, IER2 )
         IF ( IER2 .NE. 0 ) THEN
              CALL ERR_LOG ( 6426, IUER, 'CALS_W', 'Error in putting CALS '// &
     &                                             'card' )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Writing CALN-card -- the names of station-dependent calibrations
!
      CALL PUT_CLN_CARD ( NUMDB2, 'CALN', INT2(1), NCAL2, CAL%SCAL, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6427, IUER, 'CALS_W', 'Error in writing CALN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Writing CALI-card -- the lcodes of station-dependent calibrations
!
      CALL PUT_CLN_CARD ( NUMDB2, 'CALI', INT2(1), NCAL2, CAL%SCAL_LCODE, &
     &     IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6428, IUER, 'CALS_W', 'Error in writing CALI '// &
     &                                          'card' )
           RETURN
      END IF
!
      DO 430 J3=1,CAL%L_STA
         DO 440 J4=1,15
            JCAFFL(J4) = 0
 440     CONTINUE
!
! ------ Setting bytes of mapping functions
!
         DO 450 J5=1,MC_ZENC
            IF ( CAL%ZENC_APL(J5,J3) ) CALL SBIT ( JCAFFL(1), INT2(J5), &
     &                                             INT2(1) )
 450     CONTINUE
!
! ------ Writing content of the card
!
         WRITE ( NAMBUF, FMT='(A4,1X,A8, 7(1X,I7), 1X)', IOSTAT = IO) &
     &          'FCLS', CAL%STANAM(J3), ( JCAFFL(NN), NN=1,7 )
         IF ( IO .NE. 0 ) THEN
              CALL ERR_LOG ( 6429, IUER, 'CALS_W', 'Error in encoding '// &
     &                      'CALS card' )
              RETURN
         END IF
!
         IF ( J3 .EQ. 1 ) THEN
              IOP_I2 = INT2(1)
            ELSE
              IOP_I2 = INT2(0)
         END IF
!
! ------ Writing the next card of mapping functions
!
         CALL PUTCARD ( NUMDB2, 'FCLS', IOP_I2, NAMBUF, IER2 )
         IF ( IER2 .NE. 0 ) THEN
              WRITE ( 6, * ) ' ier2 = ',ier2
              WRITE ( 6, * ) ' J3 = ', J3, ' IOP_I2 = ', IOP_I2 
              WRITE ( 6, * ) ' NUMDB2 = ', NUMDB2
              WRITE ( 6, * ) ' LCARD:  FCLS'
              WRITE ( 6, * ) ' LHOLD:  '//LHOLD
              WRITE ( 6, * ) ' NAMBUF: '//NAMBUF(1:72)
              CALL ERR_LOG ( 6430, IUER, 'CALS_W', 'Error in writing '// &
     &                      'FCLS card. If the ier2 = -7, this may '// &
     &                      'happen of the size of calibration array '// &
     &                      'exceeds the number of stations' )
              RETURN
         END IF
 430  CONTINUE
 830  CONTINUE
!
! --- Writing card FCLN -- names of mapping functions
!
      CALL PUT_CLN_CARD ( NUMDB2, 'FCLN', INT2(1), NFCAL2, CAL%ZENC, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6431, IUER, 'CALS_W', 'Error in writing CALN '// &
     &                       'card' )
           RETURN
      END IF
!
      OBCAVL = 0
      OBCAPL = 0
      MCAVL  = 0
      MCAPL  = 0
!
! --- Setting bits of contributions status
!
      DO 460 J6=1,CAL%L_CONT
         IF ( CAL%CONT_AVL(J6) ) CALL SBIT ( OBCAVL, INT2(J6), INT2(1) )
         IF ( CAL%CONT_APL(J6) ) CALL SBIT ( OBCAPL, INT2(J6), INT2(1) )
 460  CONTINUE
!
      IF ( CAL%L_MCAL .GT. 0 ) THEN
!
! -------- Setting bits of mode calibration status
!
           DO 470 J7=1,CAL%L_MCAL
              IF ( CAL%MCAL_AVL(J7) ) CALL SBIT ( MCAVL, INT2(J7), INT2(1) )
              IF ( CAL%MCAL_APL(J7) ) CALL SBIT ( MCAPL, INT2(J7), INT2(1) )
 470      CONTINUE
      END IF
!
      WRITE ( NAMBUF, FMT='(A4, 1X, 4I7, 37X)', IOSTAT = IO ) 'CONT', OBCAVL, &
     &                                                  OBCAPL, MCAVL, MCAPL
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6432, IUER, 'CALS_W', 'Error in encoding CONT '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Writing card CONT -- status of contributions
!
      CALL PUTCARD ( NUMDB2, 'CONT', INT2(1), NAMBUF, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           WRITE ( 6, * ) ' ier2 = ',ier2
           CALL ERR_LOG ( 6433, IUER, 'CALS_W', 'Error in writing CONT '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Writing CNTN-card -- the names of contributions
!
      CALL PUT_CLN_CARD ( NUMDB2, 'CNTN', INT2(1), NOBCAL2, CAL%CONT, IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6434, IUER, 'CALS_W', 'Error in writing CNTN '// &
     &                       'card' )
           RETURN
      END IF
!
! --- Writing CNTI-card -- the names lcodes of contributions
!
      CALL PUT_CLN_CARD ( NUMDB2, 'CNTI', INT2(1), NOBCAL2, CAL%CONT_LCODE, &
     &     IER2 )
      IF ( IER2 .NE. 0 ) THEN
           CALL ERR_LOG ( 6435, IUER, 'CALS_W', 'Error in writing CNTN '// &
     &                       'card' )
           RETURN
      END IF
!
      IF ( CAL%L_MCAL .GT. 0 ) THEN
!
! -------- Writing MCAL-card -- the names of contributions
!
           CALL PUT_CLN_CARD ( NUMDB2, 'MCAL', INT2(1), NMCAL2, CAL%MCAL, &
     &          IER2 )
           IF ( IER2 .NE. 0 ) THEN
                CALL ERR_LOG ( 6436, IUER, 'CALS_W', 'Error in writing MCAL '// &
     &                       'card' )
                RETURN
           END IF
!
! -------- Writing MCLI-card -- the names lcodes of contributions
!
           CALL PUT_CLN_CARD ( NUMDB2, 'MCLI', INT2(1), NMCAL2, &
     &          CAL%MCAL_LCODE, IER2 )
           IF ( IER2 .NE. 0 ) THEN
                CALL ERR_LOG ( 6437, IUER, 'CALS_W', 'Error in writing MCLI '// &
     &                         'card' )
                RETURN
           END IF
      END IF
!
      CALL CLOSENAMFIL()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CALS_W  #!#
