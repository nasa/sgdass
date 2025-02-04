      SUBROUTINE GAMB_LIREC ( OBS, GAMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine  GAMB_LIREC  recalculates lists in data structure *
! *   GAMB. It takes into account only used observations.                *
! *                                                                      *
! * ________________________ Intput parameters: ________________________ *
! *                                                                      *
! *      OBS ( RECORD    ) -- Data structure which contains              *
! *                           band-independent informatiuon: time of     *
! *                           observaion, baseline, lists of objects,    *
! *                           status flags etc.                          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     GAMB ( RECORD    ) -- Data structures for group delay ambiguty   *
! *                           resolution software.                       *
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
! *  ###  13-AUG-97   GAMB_LIREC   v1.0  (c)  L. Petrov  22-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB
      INTEGER*4  IUER
      INTEGER*4  NST1, NST2, IP_SOU, IP_ST1, IP_ST2, IP_BAS
      INTEGER*4  IER, J1, J2, J3
      INTEGER*4  I_LEN, ADC_LIS
      CHARACTER  STR*20, STR1*20
!
      GAMB%UOBS  = 0
      GAMB%L_SOU = 0
      GAMB%L_STA = 0
      GAMB%L_BAS = 0
      GAMB%L_TRI = 0
!
      DO 410 J1=1,OBS%NOBS
         IF ( GAMB%USE(J1) ) THEN
              GAMB%UOBS = GAMB%UOBS + 1
!
              CALL ERR_PASS ( IUER, IER )
              IP_SOU = ADC_LIS ( MG_SOU, GAMB%L_SOU, GAMB%LIS_SOU, GAMB%K_SOU, &
     &                           OBS%ISO(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7771, IUER, 'GAMB_LIREC', 'Error in '// &
     &                 'adding a source to the list of sources' )
                   RETURN
              END IF
!
              CALL NBAST ( OBS%IBA(J1), NST1, NST2 )
              IF ( NST1 .LE. 0  .OR.  NST2 .LE. 0 ) THEN
                   WRITE ( 6, * ) ' J1=',J1,' OBS%IBA(J1) = ',OBS%IBA(J1), &
     &                    ' NST1=',NST1,' NST2=',NST2
                   CALL ERR_LOG ( 7772, IUER, 'GAMB_LIREC', 'Trap of '// &
     &                 'internal control: wriong baseline code ' )
                   RETURN
              END IF
              CALL ERR_PASS ( IUER, IER )
              IP_ST1 = ADC_LIS ( MG_STA, GAMB%L_STA, GAMB%LIS_STA, GAMB%K_STA, &
     &                           NST1, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7773, IUER, 'GAMB_LIREC', 'Error in '// &
     &                 'adding a station to the list of sources' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              IP_ST2 = ADC_LIS ( MG_STA, GAMB%L_STA, GAMB%LIS_STA, GAMB%K_STA, &
     &                           NST2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7774, IUER, 'GAMB_LIREC', 'Error in '// &
     &                 'adding a station to the list of sources' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              IP_BAS = ADC_LIS ( MG_BAS, GAMB%L_BAS, GAMB%LIS_BAS, GAMB%K_BAS, &
     &                           OBS%IBA(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7775, IUER, 'GAMB_LIREC', 'Error in '// &
     &                 'adding a station to the list of sources' )
                   RETURN
              END IF
         END IF
 410  CONTINUE
      IF ( GAMB%UOBS .LE. OBS%MINOBS     ) THEN
           CALL CLRCH (             STR  )
           CALL INCH  ( GAMB%UOBS,  STR  )
           CALL CLRCH (             STR1 )
           CALL INCH  ( OBS%MINOBS, STR1 )
           CALL ERR_LOG ( 7776, IUER, 'GAMB_LIREC', 'Too few '// &
     &         'observatiopns is in use: '//STR(1:I_LEN(STR))// &
     &         ' -- less than MINOBS: '//STR1 )
           RETURN
      END IF
!
! --- Preparing for sorting formed lists
!
! --- Baseline list will be sorted in according woth increasing modules
! --- of baseline codes (since baseline code may be negative). To do it
! --- array GAMB.K_BAS will be spoiled temorarily: the oldest
! --- decimal digits will be occupied by cmodule of baseline code
! --- (but 5 youngest digits remained intact).
!
       DO 420 J2=1,GAMB%L_BAS
          GAMB%K_BAS(J2) = 100000*ABS(GAMB%LIS_BAS(J2)) + GAMB%K_BAS(J2)
  420  CONTINUE
!
! ---- After that we sort (in increasong order) a pair of tied arrays:
! ---- GAMB.K_BAS and GAMB.LIS_BAS in according increasing
! ---- "spoiled" array GAMB.K_BAS
!
       CALL SORT_I2 ( GAMB%L_BAS, GAMB%K_BAS, GAMB%LIS_BAS )
!
! ---- And now -- removing "spoliage" from the array GAMB.K_BAS
!
       DO 430 J3=1,GAMB%L_BAS
          GAMB%K_BAS(J3) = GAMB%K_BAS(J3) - 100000*ABS(GAMB%LIS_BAS(J3))
  430  CONTINUE
!
! --- Sorting and updating other lists. We can do it easlily
!
      CALL SORT_I2 ( GAMB%L_STA, GAMB%LIS_STA, GAMB%K_STA )
      CALL SORT_I2 ( GAMB%L_SOU, GAMB%LIS_SOU, GAMB%K_SOU )
!
! --- Calculation the number of closed trinagles
!
      GAMB%L_TRI = (GAMB%L_STA - 2) * (GAMB%L_STA - 1) / 2
      IF ( GAMB%L_STA .LT. 3 ) GAMB%L_TRI=0
!
! --- Creating the list of closed triangles (if there are any)
!
      IF ( GAMB%L_TRI .NE. 0 ) THEN
           CALL TRI_GRP ( GAMB%L_STA, GAMB%LIS_STA, GAMB%L_BAS, GAMB%LIS_BAS, &
     &                    MG_TRI, GAMB%L_TRI, GAMB%LIS_TRI, -3 )
      END IF
!
      CALL ERR_LOG ( 0 , IUER )
      RETURN
      END  !#!  GAMB_LIREC  #!#
