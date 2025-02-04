      SUBROUTINE SCALER ( MATRIX, VECTOR, SCALE, DIM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SCALER PROGRAM SPECIFICATION
!
! 1.1 Scale SOLVE format matrix and B vector.
!
! 1.2 REFERENCES:
!
! 2.  SCALER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 DIM
      REAL*8 MATRIX(*), VECTOR(DIM)
!
! DIM - Dimension of the vector
! MATRIX - SOLVE format matrix
! VECTOR - B vector
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SCALE(DIM)
!
! SCALE -
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dscal,dxmpy
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I
      INTEGER*4 NBLAS
      INTEGER*8 IJ, II
      REAL*8    LOCAL, DIV
      CHARACTER ERRSTR*74 
      INTEGER*4 IROW_D, ICOL_D
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
!
! DIV - Divisor used for scaling
! I - Loop index
! II,IJ - Array indices
! IBLAS1 -
! NBLAS -
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   EBF   92.11.15  MATRIX DECLARATION (70000), Fixed 0J
!   kdb   96.06.25  Better error message.
!
! 5.  SCALER PROGRAM STRUCTURE
!
!   scaling
!
      II=0
      DO I=1,DIM
         II = II+I
         IF ( IS_R8_NAN ( MATRIX(II) ) ) THEN
              WRITE ( 6, * ) 'Trap of internal control: NaN in SCALLER ' 
              WRITE ( 6, * ) 'I= ', I, ' DIM= ', DIM
              II = -INT8(1024*1024*1024)**2
              MATRIX(II) = -1 ! Deliberatly crash in order to unwind stack
              CALL EXIT ( 1 ) 
         END IF
         IF ( MATRIX(II) .LT. 0.D0 ) THEN
              CALL ADE ( II, IROW_D, ICOL_D )
              WRITE ( ERRSTR, '("SCALER STOP NEGATIVE A VECTOR ELEMENT ",I10, &
     &                 " = ROW ",I6," and COL ",I6)') II, IROW_D, ICOL_D
              WRITE ( 6, "(A)" ) ERRSTR
              WRITE ( 6, '("Matrix dimension = ", I6)' ) DIM
              II = -1 ; WRITE ( 6, * ) MATRIX(II) ! Delioberate crash
              CALL FERR ( INT2(214), ERRSTR, INT2(0), INT2(0) )
         END IF
         DIV = DSQRT ( MATRIX(II) )
         IF ( DIV .LT. 1.0D-19 ) THEN
              SCALE(I)=0.0D0
           ELSE
              SCALE(I)=1.0D0/DIV
         ENDIF
         MATRIX(II) = 1.0D0
         LOCAL=SCALE(I)
         IJ=1+II-I
         NBLAS=I-1
         CALL DSCAL ( NBLAS, LOCAL,    MATRIX(IJ), 1 )
         CALL DXMPY ( NBLAS, SCALE, 1, MATRIX(IJ), 1 )
      ENDDO
      NBLAS=DIM
      CALL DXMPY ( NBLAS, SCALE, 1, VECTOR, 1 )
!
      RETURN
      END  !#!  SCALER  #!#
