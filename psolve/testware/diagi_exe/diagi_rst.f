      PROGRAM    DIAGI_RST
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  IUER
      CHARACTER  FINAM*120
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
      IUER = -1
      CALL DIAGI_RES ( FINAM, DIAGI_S, IUER )
      IF ( IUER .EQ. 0 ) THEN
           IUER = -1
           CALL DIAGI     ( DIAGI_S, IUER )
         ELSE
           STOP 'DIAGI_RST: Error in reading input file'
      END IF
      END  !#!  DIAGI_RST  #!#
