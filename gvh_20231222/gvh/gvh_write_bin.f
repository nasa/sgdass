      SUBROUTINE GVH_WRITE_BIN ( FILDES, ARRAY, ARR_LEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine GVH_WRITE_BIN
! *                                                                      *
! * ### 26-NOV-2001  GVH_WRITE_BIN  v1.0 (c) L. Petrov  26-NOV-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  FILDES, ARR_LEN, IUER
      INTEGER*4  ARRAY
      CHARACTER  STR*128, STR1*128
      INTEGER*4  IC
      INTEGER*4  WRITE, I_LEN
!
      IC = WRITE ( %VAL(FILDES), ARRAY, %VAL(ARR_LEN) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR      )
           CALL INCH    ( ARR_LEN,  STR )
           CALL CLRCH   ( STR1     )
           CALL GERROR  ( STR1     )
           CALL ERR_LOG ( 4201, IUER, 'GVH_WRITE_BIN', 'Error during '// &
     &         'writing record of '//STR(1:I_LEN(STR))//' bytes: '// &
     &          STR1 )
           RETURN
      END IF
      IF ( IC .NE. ARR_LEN ) THEN
           CALL CLRCH   ( STR      )
           CALL INCH    ( ARR_LEN,  STR )
           CALL ERR_LOG ( 4202, IUER, 'GVH_WRITE_BIN', 'Error during '// &
     &         'writing record of '//STR(1:I_LEN(STR))//'bytes: not all '// &
     &         'bytes are writtten in file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_WRITE_BIN  #!#
