      SUBROUTINE GVH_PUT_INLINE ( GVH_UNIT, DIMS, TYP, IAD, LINE, ILN_BEG, &
     &                            FL_UND, NREC )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine GVH_PUT_INLINE formats lines for putting         *
! *   the element of DATA section into the output file opened on unit    *
! *   GVH_UNIT and writes them into this file.                           *
! *                                                                      *
! * ### 26-NOV-2001  GVH_PUT_INLINE  v1.2 (c)  L. Petrov 09-SEP-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  GVH_UNIT, DIMS(2), TYP, ILN_BEG, NREC
      ADDRESS__TYPE :: IAD
      CHARACTER  LINE*(*)
      LOGICAL*1  FL_UND
      INTEGER*4  ISEG, J2, ILN, ILE, I4_VAL
      INTEGER*2  I2_VAL
      REAL*4     R4_VAL
      REAL*8     R8_VAL
      INTEGER*4, EXTERNAL :: ILEN
!
      DO 410 ISEG=1,DIMS(2)
         CALL CLRCH ( LINE(ILN_BEG+1:) )
         IF ( TYP .EQ. GVH__C1 ) THEN
!
! ----------- Character type
!
              CALL CLRCH ( LINE(ILN_BEG+1:) )
              ILN = ILEN(LINE)
              CALL INCH ( 1, LINE(ILN+2:) )
              CALL CHASHR ( LINE(ILN+2:ILN+3) )
!
              ILN = ILEN(LINE)
              CALL INCH ( ISEG, LINE(ILN+2:) )
              CALL CHASHR ( LINE(ILN+2:ILN+3) )
!
              ILN = ILEN(LINE)
              CALL MEMCPY ( %REF(LINE(ILN+2:)), %VAL(IAD), %VAL(DIMS(1)) )
              ILE = ILEN(LINE)
              CALL BLANK_TO_UNDERSCORE ( LINE(ILN+2:ILE) )
              IAD = IAD + DIMS(1)
!
              WRITE ( GVH_UNIT, '(A)' ) LINE(1:ILEN(LINE))
              NREC = NREC + 1
            ELSE
!
! ----------- Not a character type
!
              DO 420 J2=1,DIMS(1)
                 CALL CLRCH ( LINE(ILN_BEG+1:) )
                 ILN = ILEN(LINE)
                 CALL INCH ( J2, LINE(ILN+2:) )
                 CALL CHASHR ( LINE(ILN+2:ILN+3) )
!
                 ILN = ILEN(LINE)
                 CALL INCH ( ISEG, LINE(ILN+2:) )
                 CALL CHASHR ( LINE(ILN+2:ILN+3) )
!
                 ILN = ILEN(LINE)
                 IF ( TYP .EQ. GVH__I2 ) THEN
!
! ------------------- Integer*2 type
!
                      CALL MEMCPY ( I2_VAL, %VAL(IAD), %VAL(GVH__I2_LEN) )
                      WRITE ( LINE(ILN+2:ILN+7), '(I6)' ) I2_VAL
                      CALL CHASHL ( LINE(ILN+2:ILN+7) )
                      IAD = IAD + GVH__I2_LEN
                    ELSE IF ( TYP .EQ. GVH__I4 ) THEN
!
! ------------------- Integer*4 type
!
                      CALL MEMCPY ( I4_VAL, %VAL(IAD), %VAL(GVH__I4_LEN) )
                      WRITE ( LINE(ILN+2:ILN+12), '(I11)' ) I4_VAL
                      CALL CHASHL ( LINE(ILN+2:ILN+12) )
                      IAD = IAD + GVH__I4_LEN
                    ELSE IF ( TYP .EQ. GVH__R4 ) THEN
!
! ------------------- Real*4 type
!
                      CALL MEMCPY ( R4_VAL, %VAL(IAD), %VAL(GVH__R4_LEN) )
                      WRITE ( LINE(ILN+2:ILN+16), '(1PE15.7)' ) R4_VAL
                      CALL CHASHL ( LINE(ILN+2:ILN+16) )
                      IAD = IAD + GVH__R4_LEN
                    ELSE IF ( TYP .EQ. GVH__R8 ) THEN
!
! ------------------- Real*8 type
!
                      CALL MEMCPY ( R8_VAL, %VAL(IAD), %VAL(GVH__R8_LEN) )
                      WRITE ( LINE(ILN+2:ILN+23), '(1PD22.15)' ) R8_VAL
                      CALL CHASHL ( LINE(ILN+2:ILN+23) )
                      IAD = IAD + GVH__R8_LEN
                 END IF
!
! -------------- Write a line into the file
!
                 WRITE ( GVH_UNIT, '(A)' ) LINE(1:ILEN(LINE))
                 NREC = NREC + 1 ! update lines counter
 420          CONTINUE
         END IF
 410  CONTINUE
      RETURN
      END  !#!  GVH_PUT_INLINE  #!#
