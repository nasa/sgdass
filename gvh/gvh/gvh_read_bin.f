      SUBROUTINE GVH_READ_BIN ( FILDES, ARRAY, ARR_LEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine GVH_READ_BIN
! *                                                                      *
! * ### 26-NOV-2001  GVH_READ_BIN  v1.0 (c) L. Petrov  26-NOV-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  FILDES, ARR_LEN, IUER
      INTEGER*1  ARRAY(*)
      CHARACTER  STR*128, STR1*128, STR2*32
      ADDRESS__TYPE  IC
      ADDRESS__TYPE, EXTERNAL :: READ 
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( ARR_LEN > 0 ) THEN
           IC = READ ( %VAL(FILDES), ARRAY, %VAL(ARR_LEN) )
           IF ( IC .NE. ARR_LEN ) THEN
                CALL CLRCH   ( STR )
                CALL INCH    ( ARR_LEN, STR )
                CALL CLRCH   ( STR1 )
                CALL GERROR  ( STR1 )
                IF ( STR1(1:7) == 'Success' ) CALL CLRCH ( STR1 )
                CALL CLRCH   ( STR2 )
                CALL INCH    ( IC, STR2 )
                CALL ERR_LOG ( 4211, IUER, 'GVH_READ_BIN', 'Error during '// &
          &         'reading the record of '//STR(1:I_LEN(STR))//' bytes: '// &
          &          STR1(1:I_LEN(STR1))//' -- only '//STR2(1:I_LEN(STR2))// &
          &         ' were actually read' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_READ_BIN  !#!#
