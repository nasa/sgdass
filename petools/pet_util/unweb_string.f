      SUBROUTINE UNWEB_STRING ( STR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary rouine UNWEB_STRING decides a string from the form how  *
! *   it was passed from the Web form to the original form how a user    *
! *   has entered it.                                                    *
! *                                                                      *
! *  ### 04-DEC-2005  UNWEB_STRING  v1.0 (c)  L. Petrov  04-DEC-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  STR*(*)
      INTEGER*4, EXTERNAL :: I_LEL, ILEN
      INTEGER*4  IC, IL, IP, J1, J2, J3
!
      IF ( ILEN(STR) == 0 ) RETURN
      IL = ILEN(STR)
!
      IL = ILEN(STR)
      DO 410 J1=1,IL
         IF ( STR(J1:J1) == '+' ) STR(J1:J1) = ' '
 410  CONTINUE
!
      DO 420 J2=1,IL
         IF ( STR(1:1) == '+' ) THEN
              STR = STR(2:)
           ELSE
              GOTO 820
         END IF
 420  CONTINUE
 820  CONTINUE
!
      IL = ILEN(STR)
      DO 430 J3=1,IL
         IP = INDEX ( STR, '%' )
         IF ( IP > 0 ) THEN
              READ ( UNIT=STR(IP+1:IP+2), FMT='(Z2)' ) IC
              IF ( IP == 1 ) THEN
                   STR = CHAR(IC)//STR(4:)
                 ELSE
                   STR = STR(1:IP-1)//CHAR(IC)//STR(IP+3:)
              END IF
              GOTO 430
         END IF
         GOTO 830
 430  CONTINUE
 830  CONTINUE
      CALL CHASHL ( STR )
!
      RETURN
      END  SUBROUTINE  UNWEB_STRING  !#!#
