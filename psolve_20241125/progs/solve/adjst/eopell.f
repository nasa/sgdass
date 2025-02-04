      SUBROUTINE EOPELL(IAA,NC,OorR,mat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  EOPELL PROGRAM SPECIFICATION
!
! 1.1 RECOVER ERROR ELLIPSE FOR EOP SIGMAS
!
! 1.2 REFERENCES:
!
! 2.  EOPELL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
        INTEGER*2 IAA(3), NC
        Character*(*) OorR
        real*8 mat(*)
!
!       IAA - list of the deriv array positions of the three earth orientation
!             offsets for this epoch.  A zero indicates that the given offset
!             is not being estimated in this solution.
!
!       NC - the number of earth orientation offsets being estimated for
!            this epoch
!
!       OorR - String indicating whether we are handling offset or rate
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!      include '../include/nrmcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a2jst
!       CALLED SUBROUTINES: eigsrt, jacobi
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ICMP(3),I,J,NROT,IA(3)
      INTEGER*4 INDX4,IAIIAJ,ja
      REAL*8 ALCL(3,3),D(3),V(3,3),BT(3),Z(3),RD2DG,AZM,ELD
      REAL*8 DATAN2Z,CNVRT(3),AZD
!
      DATA RD2DG/57.29577951D0/,ICMP/2,1,3/
!
! 4.  HISTORY
!  WHO  WHEN     WHAT
!
! 5.  EOPELL PROGRAM STRUCTURE
!
      ja = 3*M_GPA
      IF(NC.LT.2) RETURN !less than two eo offsets set for this epoch
!
! Determine which components were estimated.
!
      DO I=1,3
        DO J=1,3
          ALCL(J,I)=0.0D0
        ENDDO
      ENDDO
!
      DO I = 1,3
        IA(ICMP(I)) = IAA(I)
      END DO
!
! Do the appropriate conversion to the section of the matrix we need.
!
      CNVRT(1)=1.0D0
      CNVRT(2)=1.0D0
      CNVRT(3)=-1.0D0/(240.0D0*RD2DG) !(1DEG/240SEC)/RD2DG
      DO I=1,3
        IF(IA(I).NE.0) THEN
          DO J=1,3
            IF(IA(J).NE.0) THEN
              IAIIAJ=INDX4(IA(I),IA(J))
              ALCL(J,I)=mat(ja+IAIIAJ)*CNVRT(J)*CNVRT(I)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
!@      CALL OLD_JACOBI( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!@      CALL OLD_EIGSRT( D, V, INT2(3), INT2(3) )
      CALL LAPACK_JACOBI ( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!
!  Calculate the axes of the error ellipse and write them out.
!
      DO I=1,NC
        IF(DSQRT(MAX(0.0D0,D(I)))*RD2DG*3600.0D3.GE..0005) THEN
          AZD=DATAN2Z(V(2,I),V(1,I))*RD2DG
          AZM=DSQRT(V(1,I)**2+V(2,I)**2)
          ELD=DATAN2Z(V(3,I),AZM)*RD2DG
          IF(KSPOOL) &
     &       WRITE(23,9910) OorR,AZD,ELD,DSQRT(D(I))*RD2DG*3600.D0*1000.D0
9910      FORMAT(30X,A6,' Error Ellipsoid Axis: ', &
     & 'Longitude = ',F6.1,' Latitude = ',F5.1,' Sigma    = ',F10.3, &
     & ' mas')
        ENDIF
      ENDDO
!
      RETURN
      END
