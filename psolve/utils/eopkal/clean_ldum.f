        SUBROUTINE CLEAN_LDUM ( LDUM, ILEN )
        IMPLICIT NONE
        INTEGER*2 ILEN
        CHARACTER*255 LTEMP
        CHARACTER*(*) LDUM
!
        INTEGER*2 IOUT, I
!
        LTEMP = " "
        IOUT=1
        DO I=1,ILEN
           IF ( LDUM(I:I) .EQ. "|" ) THEN
                LTEMP(IOUT:IOUT) = " "
                IOUT=IOUT+1
              ELSE IF ( LDUM(I:I) .EQ. "-" ) THEN
                LTEMP(IOUT:IOUT+1) = " -"
                IOUT=IOUT+2
              ELSE
                LTEMP(IOUT:IOUT) = LDUM(I:I)
                IOUT=IOUT+1
           ENDIF
           IF ( IOUT .GT. ILEN-1 ) GOTO 10
        END DO
10      CONTINUE
        LDUM = LTEMP(1:ILEN)
        RETURN
        END  !#!  CLEAN_LDUM  #!#
