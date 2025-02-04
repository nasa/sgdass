      SUBROUTINE ARC_ARC ( A, A2, INAME, JNAME )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ARC_ARC PROGRAM SPECIFICATION
!
! 1.1 Manipulate the saved arcfile matrix of the ith arc and the
!     matrix saved from the execution of BACK on the jth arc, to
!     produce covariance elements between local parameters of
!     different arcs.
!
!     The ith arc file contains inv([A]ii)*[A]ig
!     the jth arc file contains [V]jg; a result of BACK
!
!     The product formed is
!          [V]ij = (inv([A]ii)*[A]ig)*([V]gj)
!
! 1.2 REFERENCES:
!
! 2.  ARC_ARC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 A(*),A2(*)
      CHARACTER*(*) INAME,JNAME
!
! A - Sub_set of ARC matrix
! A2 - Sub_set of CGM matrix
! INAME,JNAME - Names of input ARC/CGM files
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'baccm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covmm
!       CALLED SUBROUTINES: prnt_vect
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IWDS
      PARAMETER   (IWDS = 10)
!
      INTEGER*4 I,J,K,N,R,RMOD,L,II,IJ,JI,JJ
      INTEGER*4 IOS
      INTEGER*8 INDX8
      REAL*8 SUM
      CHARACTER XS1*1,XS2*1
!
! 4.  HISTORY
!   WHO   WHEN  WHAT
!
! 5.  ARC_ARC PROGRAM STRUCTURE
!
!   first, write header for this arc-arc block
!
      WRITE ( 88, 1101, IOSTAT=IOS) &
     &        I_ARC, END_IDX, INAME, IARCNM, END_JDX, JNAME
      CALL FERR ( INT2(IOS), "Writing covariance file", INT2(0), INT2(0) )
 1101 FORMAT('1',1X,I5,1X,I5,1X,A/ &
     &       ' ',1X,I5,1X,I5,1X,A/)
!
!   form the covariance matrix.  NOTE that we run the inner loop only
!   over those globals present in the I-arc list, since the matrix
!   elements for globals not in this i-arc arc zero.  This requires the
!   use of the cross-ref array IXJ2I to get the corect element in the
!   j-arc.
!
      DO I=1,END_IDX
         IJ=IND_ICOV(I)
         DO J=1,END_JDX
            SUM=0.D0
            JJ=IND_JCOV(J)
            DO K=I_ARC_ARCS+1,I_ARC_ARCS+I_ARC_GLBS
               II=K
               JI=IXI2J(K)
               SUM=SUM+A2(INDX8(II,IJ))*A(INDX8(JI,JJ))
            ENDDO
            VECTOR(J)=SUM
         ENDDO
!
! ------ call utility that prints w/o extraneous blank lines
!
         IF ( END_JDX .NE. 0 .AND. IJ .LE. I_ARC_ARCS ) THEN
              CALL PRNT_VECT(VECTOR,END_JDX,I )
         ENDIF
      ENDDO
      RETURN
      END
