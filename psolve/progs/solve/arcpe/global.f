      SUBROUTINE GLOBAL ( A, B, NG, NL, ND, AT, VT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GLOBAL PROGRAM SPECIFICATION
!
! 1.1 GLOBAL does the monster matrix manipulation of the Back
!     solution, although it could be easily modified to do the
!     forward solution or any number of variants.
!
! 1.2 REFERENCES:
!
! 2.  GLOBAL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4   NG, NL, ND
      Real*8      A(*), B(ND), AT(NL,NG), VT(ND)
!
! A - Matrix
! B - Vector
! AT - First available location in the A matrix
! ND - total number of parameters
! NG - Number of global parameters
! NL - Number of arc parameters
! VT - Sigmas
!
! 2.3 OUTPUT Variables:
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: elimin
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I, J, K, IBLAS, NBLAS
      INTEGER*8, EXTERNAL :: INDX8
      REAL*8     TEMP
      REAL*8,    EXTERNAL :: DDOT
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GLOBAL PROGRAM STRUCTURE
!
!   COPY A[ig]
!
      IBLAS=1
      NBLAS=NL
      DO J=NL+1,NL+NG
         CALL DCOPY ( NBLAS, A(INDX8(1,J)), IBLAS, AT(1,J-NL), IBLAS )
      ENDDO
!
!          B[g]  = B[g] - A[gi] * inv(A[ii]) * B[i]
! locally  B[g]  = B[g] - transpose(AT[ig])  * B[i]
!
!
      DO I=NL+1,NL+NG
         TEMP = DDOT ( NBLAS, B(1), IBLAS, AT(1,I-NL), IBLAS )
         B(I)=B(I)-TEMP
      ENDDO
!
!   form the cross piece
!
!          A[ig] = - inv(A[ii]) * A[ig]
! locally  A[ig] = - A[ii]      * AT[ig]
!
      DO J=1,NL
         DO I=1,NL
            VT(I)=A(INDX8(I,J))
         ENDDO
         DO I=NL+1,NL+NG
            TEMP = DDOT ( NBLAS, VT, IBLAS, AT(1,I-NL), IBLAS )
            A(INDX8(J,I))=-TEMP
         ENDDO
      ENDDO
!
!         A[gg] = A[gg] - A[gi] * inv(A[ii]) *  A[ig]
! locally A[gg] = A[gg] + transpose(A[ig])   * AT[ig]
!
      DO J=NL+1,NL+NG
         DO I=NL+1,J
            TEMP = DDOT ( NBLAS, A(INDX8(1,J)), IBLAS, AT(1,I-NL), IBLAS )
            A(INDX8(I,J))=A(INDX8(I,J))+TEMP
         ENDDO
      ENDDO
!
      RETURN
      END  !#!  GLOBAL  #!#
