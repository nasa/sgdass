      FUNCTION   PTR_CH ( CHR )
! ************************************************************************
! *                                                                      *
! *   Function PTR_CH  returns the address of the address of the first   *
! *   character of the null-terminated string which is the copy of the   *
! *   string CHR.                                                        *
! *                                                                      *
! *  ### 23-FEB-2001     PTR_CH    v1.0 (c)  L. Petrov  26-FEB-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  CHR*(*), STR*32767
      SAVE       STR
      ADDRESS__TYPE :: PTR_CH
      INTEGER*4  IL, J1
!
      IF ( LEN(CHR) .EQ. 0 ) THEN
           STR(1:1) = CHAR(0)
         ELSE
           IL = 0
           DO 410 J1=LEN(CHR),1,-1
              IF ( CHR(J1:J1) .NE. CHAR(0) .AND. CHR(J1:J1) .NE. CHAR(32) ) THEN
                   IL = J1
                   GOTO 810
              END IF
 410       CONTINUE
 810       CONTINUE
           STR = CHR(1:IL)//CHAR(0)
      END IF
      PTR_CH = LOC ( STR )
!
      RETURN
      END  !#!  PTR_CH  #!#
