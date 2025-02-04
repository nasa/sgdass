      SUBROUTINE SORT_WEIGHTS ( WEIGHT_FILE, WEIGHT_TYPE_GEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SORT_WEIGHTS 
! *                                                                      *
! *  ### 09-FEB-2006  SORT_WEIGHTS  v2.0 (c) L. Petrov  04-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      CHARACTER  WEIGHT_FILE*(*), WEIGHT_TYPE_GEN*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER  STR*256, CDATE*10, SUFFIX*2
      INTEGER*4  IUER
      INTEGER*4  IOS, J1, J2, LUN, NWEI, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
#ifdef GNU
      INTEGER*4, EXTERNAL :: COMPAR_BAS_WEI, COMPAR_SIT_WEI 
#else
      INTEGER*2, EXTERNAL :: COMPAR_BAS_WEI, COMPAR_SIT_WEI 
#endif
!
      ALLOCATE ( BUF(MAX4_WEIREC), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH   ( STR ) 
           CALL IINCH   ( MAX4_WEIREC*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 6811, IUER, 'SORT_WEIGHTS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( WEIGHT_FILE, MAX4_WEIREC, BUF, NWEI, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6811, IUER, 'SORT_WEIGHTS', 'Failure to read '// &
     &          'input weight file '//WEIGHT_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
      DO 410 J1=1,NWEI
         CALL INCH ( J1, BUF(J1)(241:248) )
 410  CONTINUE 
!
      IF ( WEIGHT_TYPE_GEN == 'B' ) THEN
           CALL FOR_QSORT ( %REF(BUF), NWEI, LEN(BUF(1)), COMPAR_BAS_WEI )
        ELSE IF ( WEIGHT_TYPE_GEN == 'S' ) THEN
           CALL FOR_QSORT ( %REF(BUF), NWEI, LEN(BUF(1)), COMPAR_SIT_WEI )
      END IF
! 
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=WEIGHT_FILE, STATUS='UNKNOWN', IOSTAT=IOS ) 
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH  ( STR ) 
           CALL INCH   ( IOS, STR )
           CALL ERR_LOG ( 6812, IUER, 'SORT_WEIGHTS', 'Failure to open '// &
     &         'weight file '//WEIGHT_FILE(1:I_LEN(WEIGHT_FILE))// &
     &         ' for writing' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 420 J2=1,NWEI
         IF ( BUF(J2)(1:1) == '*' ) GOTO 420
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         IF ( BUF(J2)(1:1) == ' ' ) GOTO 420
         IF ( J2 > 1 ) THEN
              IF ( WEIGHT_TYPE_GEN == 'B' ) THEN
                   IF ( BUF(J2)(1:31) == BUF(J2-1)(1:31) ) GOTO 420
                   IF ( BUF(J2) == BUF(J2-1) ) GOTO 420
                   IF ( BUF(J2)(1:14) .NE. BUF(J2-1)(1:14) ) THEN
                        WRITE ( LUN, '(A)' ) '*' 
                   END IF
                 ELSE IF ( WEIGHT_TYPE_GEN == 'S' ) THEN
                    IF ( BUF(J2)(1:22) == BUF(J2-1)(1:22) ) GOTO 420
!                   IF ( BUF(J2)(1:13) .NE. BUF(J2-1)(1:13) ) THEN
!                        WRITE ( LUN, '(A)' ) '*' 
!                   END IF
              END IF
         END IF
         CALL CLRCH ( BUF(J2)(241:248) )
         WRITE ( LUN, '(A)' ) BUF(J2)(1:I_LEN(BUF(J2)))
 420  CONTINUE 
!
      IF ( WEIGHT_TYPE_GEN == 'B' ) THEN
           WRITE ( LUN, '(A)' ) '*' 
      END IF
      CLOSE ( UNIT=LUN ) 
      CALL FLUSH ( LUN )
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SORT_WEIGHTS  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_BAS_WEI ( ARR1, ARR2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine COMPAR_BAS_WEI 
! *                                                                      *
! * ### 09-FEB-2006  COMPAR_BAS_WEI  v2.2 (c) L. Petrov 16-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GNU
      INTEGER*4  COMPAR_BAS_WEI 
#else
      INTEGER*2  COMPAR_BAS_WEI 
#endif
      CHARACTER  STR1*256, STR2*256
      CHARACTER  CDATE1*10, CDATE2*10, SUFFIX1*2, SUFFIX2*2
      INTEGER*4  INT_STR1, INT_STR2, IND_STR1, IND_STR2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*1  ARR1(*), ARR2(*)
!
      CALL MEMCPY ( STR1, ARR1 )
      CALL MEMCPY ( STR2, ARR2 )
      CALL CHIN ( STR1(1:8),     INT_STR1 )
      CALL CHIN ( STR2(1:8),     INT_STR2 )
      CALL CHIN ( STR1(241:248), IND_STR1 )
      CALL CHIN ( STR2(241:248), IND_STR2 )
!
      IF ( STR1(1:1) == '*' ) THEN
           CDATE1  = '1970.01.01'
           SUFFIX1 = '*'
         ELSE IF ( STR1(1:1) == ' ' ) THEN
           CDATE1  = '1970.01.01'
           SUFFIX1 = ' '
         ELSE IF ( INT_STR1 > 19700101 .AND. INT_STR1 < 20700101 ) THEN
           CDATE1  = STR1(1:4)//'.'//STR1(5:6)//'.'//STR1(7:8)
           SUFFIX1 = STR1(9:10)
         ELSE
           IER = -1
           CALL PARSE_DBNAME ( STR1, CDATE1, SUFFIX1, IER )
           IF ( IER .NE. 0 ) THEN
                IER = -1
                CALL ERR_LOG ( 171, IER, 'COMPAR_BAS_WEI', 'Failure to '// &
     &              'parse line of the weight file "'//STR1(1:I_LEN(STR1))// &
     &              '" --- Please check weights file' )
                RETURN 
           END IF
      END IF
!
      IF ( STR2(1:1) == '*' ) THEN
           CDATE2  = '1970.01.01'
           SUFFIX2 = '*'
         ELSE IF ( STR2(1:1) == ' ' ) THEN
           CDATE2  = '1970.01.01'
           SUFFIX2 = ' '
         ELSE IF ( INT_STR2 > 19700101 .AND. INT_STR2 < 20700101 ) THEN
           CDATE2  = STR2(1:4)//'.'//STR2(5:6)//'.'//STR2(7:8)
           SUFFIX2 = STR2(9:10)
         ELSE
           IER = -1
           CALL PARSE_DBNAME ( STR2, CDATE2, SUFFIX2, IER )
           IF ( IER .NE. 0 ) THEN
                IER = -1
                CALL ERR_LOG ( 172, IER, 'COMPAR_BAS_WEI', 'Failure to '// &
     &              'parse line of the weight file "'//STR2(1:I_LEN(STR2))// &
     &              '" --- Please check weights file' )
                RETURN 
           END IF
      END IF
!
      IF ( CDATE1 > CDATE2 ) THEN
           COMPAR_BAS_WEI =  1
         ELSE IF ( CDATE1 < CDATE2 ) THEN
           COMPAR_BAS_WEI = -1
         ELSE 
           IF ( SUFFIX1 > SUFFIX2 ) THEN
                COMPAR_BAS_WEI =  1
             ELSE IF ( SUFFIX1 < SUFFIX2 ) THEN
                COMPAR_BAS_WEI = -1
             ELSE 
                IF ( STR1(13:31) > STR2(13:31) ) THEN
                     COMPAR_BAS_WEI =  1
                  ELSE IF ( STR1(13:31) < STR2(13:31) ) THEN
                     COMPAR_BAS_WEI = -1
                  ELSE 
                     IF ( IND_STR1 < IND_STR2 ) THEN
                          COMPAR_BAS_WEI =  1
                        ELSE IF ( IND_STR1 > IND_STR2 ) THEN
                          COMPAR_BAS_WEI = -1
                        ELSE 
                          COMPAR_BAS_WEI =  0
                     END IF
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  COMPAR_BAS_WEI  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_SIT_WEI ( ARR1, ARR2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine COMPAR_SIT_WEI 
! *                                                                      *
! * ### 09-FEB-2006  COMPAR_SIT_WEI  v2.2 (c) L. Petrov 16-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GNU
      INTEGER*4  COMPAR_SIT_WEI 
#else
      INTEGER*2  COMPAR_SIT_WEI 
#endif
      INTEGER*1  ARR1(*), ARR2(*)
      CHARACTER  STR1*256, STR2*256
      CHARACTER  CDATE1*10, CDATE2*10, SUFFIX1*2, SUFFIX2*2
      INTEGER*4  INT_STR1, INT_STR2, IND_STR1, IND_STR2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL MEMCPY ( STR1, ARR1 )
      CALL MEMCPY ( STR2, ARR2 )
      CALL CHIN ( STR1(1:8),     INT_STR1 )
      CALL CHIN ( STR2(1:8),     INT_STR2 )
      CALL CHIN ( STR1(241:248), IND_STR1 )
      CALL CHIN ( STR2(241:248), IND_STR2 )
!
      IF ( STR1(1:1) == '*' ) THEN
           CDATE1  = '1970.01.01'
           SUFFIX1 = '*'
         ELSE IF ( STR1(1:1) == '#' ) THEN
           CDATE1  = '1970.01.01'
           SUFFIX1 = '#'
         ELSE IF ( STR1(1:1) == ' ' ) THEN
           CDATE1  = '1970.01.01'
           SUFFIX1 = ' '
         ELSE IF ( INT_STR1 > 19700101 .AND. INT_STR1 < 20700101 ) THEN
           CDATE1  = STR1(1:4)//'.'//STR1(5:6)//'.'//STR1(7:8)
           SUFFIX1 = STR1(9:10)
         ELSE
           IER = -1
           CALL PARSE_DBNAME ( STR1, CDATE1, SUFFIX1, IER )
           IF ( IER .NE. 0 ) THEN
                IER = -1
                CALL ERR_LOG ( 181, IER, 'COMPAR_SIT_WEI', 'Failure to '// &
     &              'parse line of the weight file "'//STR1(1:I_LEN(STR1))// &
     &              '" --- Please check weights file' )
                RETURN 
           END IF
      END IF
!
      IF ( STR2(1:1) == '*' ) THEN
           CDATE2  = '1970.01.01'
           SUFFIX2 = '*'
         ELSE IF ( STR2(1:1) == ' ' ) THEN
           CDATE2  = '1970.01.01'
           SUFFIX2 = ' '
         ELSE IF ( STR2(1:1) == '#' ) THEN
           CDATE2  = '1970.01.01'
           SUFFIX2 = '#'
         ELSE IF ( INT_STR2 > 19700101 .AND. INT_STR2 < 20700101 ) THEN
           CDATE2  = STR2(1:4)//'.'//STR2(5:6)//'.'//STR2(7:8)
           SUFFIX2 = STR2(9:10)
         ELSE
           IER = -1
           CALL PARSE_DBNAME ( STR2, CDATE2, SUFFIX2, IER )
           IF ( IER .NE. 0 ) THEN
                IER = -1
                CALL ERR_LOG ( 182, IER, 'COMPAR_SIT_WEI', 'Failure to '// &
     &              'parse line of the weight file "'//STR2(1:I_LEN(STR2))// &
     &              '" --- Please check weights file' )
                RETURN 
           END IF
      END IF
!
      IF ( CDATE1 > CDATE2 ) THEN
           COMPAR_SIT_WEI =  1
         ELSE IF ( CDATE1 < CDATE2 ) THEN
           COMPAR_SIT_WEI = -1
         ELSE 
           IF ( SUFFIX1 > SUFFIX2 ) THEN
                COMPAR_SIT_WEI =  1
             ELSE IF ( SUFFIX1 < SUFFIX2 ) THEN
                COMPAR_SIT_WEI = -1
             ELSE 
                IF ( STR1(15:22) > STR2(15:22) ) THEN
                     COMPAR_SIT_WEI =  1
                  ELSE IF ( STR1(15:22) < STR2(15:22) ) THEN
                     COMPAR_SIT_WEI = -1
                  ELSE 
                     IF ( IND_STR1 < IND_STR2 ) THEN
                          COMPAR_SIT_WEI =  1
                        ELSE IF ( IND_STR1 > IND_STR2 ) THEN
                          COMPAR_SIT_WEI = -1
                        ELSE 
                          COMPAR_SIT_WEI =  0
                     END IF
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  COMPAR_SIT_WEI  !#!  
