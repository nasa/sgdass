      SUBROUTINE VTD_NAME_REPAIR ( NAME )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  VTD_NAME_REPAIR repaires the IVS station name:  *
! *   It removes trailing undescores if necessary and replaces blanks    *
! *   within the name with underscords.                                  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *   NAME ( CHARACTER ) -- IVS station name.                            *
! *                                                                      *
! * ### 26-JAN-2004  VTD_NAME_REPAIR  v1.0 (c) L. Petrov 26-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  NAME*(*)
      INTEGER*4  J1, J2, ILN
      EXTERNAL   ILEN
      INTEGER*4  ILEN
!
      IF ( NAME(8:8) .EQ. '_' ) THEN
           DO 410 J1=8,1,-1
              IF ( NAME(J1:J1) .EQ. '_' ) THEN
                   NAME(J1:J1) = ' '
                 ELSE
                   GOTO  810
              END IF
 410       CONTINUE 
 810       CONTINUE 
      END IF
      ILN = ILEN(NAME)
!
      DO 420 J2=1,ILN
         IF ( NAME(J2:J2) .EQ. ' ' ) NAME(J2:J2) = '_'
 420  CONTINUE 
      RETURN
      END  SUBROUTINE  VTD_NAME_REPAIR 
