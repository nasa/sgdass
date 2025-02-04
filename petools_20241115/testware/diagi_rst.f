      PROGRAM    DIAGI_RST
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  IUER
      CHARACTER  FINAM*120
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  NUMARG, ILEN
      INTEGER*4  IARGC 
!
      NUMARG = IARGC ()
      CALL CLRCH ( FINAM )
      IF ( NUMARG .GE. 1 ) THEN
           CALL GETARG ( 1, FINAM )
        ELSE
           WRITE ( 6, 110 ) 
 110       FORMAT ( 1X,'diagi_rst v1.0     file name ? '$ )
           READ ( UNIT=5, FMT='(A)' ) FINAM
           IF ( ILEN(FINAM) .EQ. 0 ) THEN
                FINAM = DIAGI_OUT//SAV_DIAGI
           END IF
      END IF
!
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4156, IUER, 'DIAGI_3E', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      IUER = -1
      CALL DIAGI_RES ( FINAM, DIAGI_S, IUER )
      IF ( IUER .EQ. 0 ) THEN
           DIAGI_S%IDEV = IDEV
           IUER = -1
           CALL DIAGI     ( DIAGI_S, IUER )
         ELSE
           STOP 'DIAGI_RST: Error in reading input file'
      END IF
      END  !#!  DIAGI_RST  #!#
