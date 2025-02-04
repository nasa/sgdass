      SUBROUTINE COV_REOP ( THISJD, FSTJD, INC, NUM, PI, CVINF, WHO, EOPRCONS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COV_EOP PROGRAM SPECIFICATION
!
! 1.1 Get the constraint matrix
!
! 1.2 REFERENCES:
!
! 2.  COV_EOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NUM, INC
      REAL*8 FSTJD,THISJD,PI
      REAL*8 EOPRCONS(3)
      CHARACTER*(*) WHO(3)
!
! EOPRCONS - Earth orientation rate parameter constraint
! FSTJD - First J.D.
! INC - Interval between epochs
! NUM - Number of epochs
! PI - Pi in 11-digit precision
! THISJD - Epoch for which shorter position and parameter names lists
!          should be generated
! WHO - List of earth orientation offset parameter names for this epoch
!
! 2.3 OUTPUT Variables:
!
      REAL*8 CVINF(6)
!
! CVINF - THe solve format matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: /adjst/eop_share
!       CALLED SUBROUTINES: dppfa,dppin
!
! 3.  LOCAL VARIABLES
!
      REAL*8 CVDUM(6), COVR(6), RSSX, RSSY, RSSU, SIGX, SIGY, SIGU
      REAL*8 RAD_PER_MASEC, SEC_PER_MASEC, COVR_MIN
      PARAMETER  ( COVR_MIN = 0.01 ) ! 0.01 mas/day -- minimal value
      INTEGER*2 I
      CHARACTER*150 ERRSTR
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!   pet   970610 Added support of new EOP parametrization
!   pet   2001.05.29  Improved comments. Added check whether diagnal elements
!                     of the apriori EOP rate constraint matrix are too small
!
! 5.  COV_EOP PROGRAM STRUCTURE
!
!   conversion constants
!
      RAD_PER_MASEC=PI/(180.D0*3600.D0*1000.D0)
      SEC_PER_MASEC=1.D0/15000.D0
!
! --- Get values to RSS to diagonal
!
      RSSX=EOPRCONS(1)*RAD_PER_MASEC
      RSSY=EOPRCONS(2)*RAD_PER_MASEC
      RSSU=EOPRCONS(3)/1.0D6
!
! --- Form the covariance matrix for the left endpoint in all of its glory
!
      COVR(1)=RSSX*RSSX
      COVR(2)=0.0
      COVR(3)=RSSY*RSSY
      COVR(4)=0.0
      COVR(5)=0.0
      COVR(6)=RSSU*RSSU
      IF ( COVR(1) .LT. (COVR_MIN*RAD_PER_MASEC)**2  .OR. &
     &     COVR(3) .LT. (COVR_MIN*RAD_PER_MASEC)**2  .OR. &
     &     COVR(6) .LT. (COVR_MIN*SEC_PER_MASEC)**2       ) THEN
           WRITE ( UNIT=ERRSTR, FMT='(A,3(D15.7,1X))' ) 'COV_REOP: Too small '// &
     &            'diagnal elements of a priori constraint EOP rate matrix: ', &
     &     COVR(1), COVR(3), COVR(6)
!
           CALL FERR ( INT2(161), ERRSTR, INT2(0), INT2(0) )
      END IF
!
! --- Now perform straight-line interpolation between the two days by
!
      CVDUM(1)=COVR(1)
      CVDUM(2)=COVR(2)
      CVDUM(3)=COVR(3)
      CVDUM(4)=COVR(4)
      CVDUM(5)=COVR(5)
      CVDUM(6)=COVR(6)
!
! --- Scale for inverting
!
      SIGX=DSQRT(CVDUM(1))
      SIGY=DSQRT(CVDUM(3))
      SIGU=DSQRT(CVDUM(6))
!
      CVDUM(1)=1.D0
      CVDUM(2)=CVDUM(2)/(SIGX*SIGY)
      CVDUM(3)=1.D0
      CVDUM(4)=CVDUM(4)/(SIGX*SIGU)
      CVDUM(5)=CVDUM(5)/(SIGY*SIGU)
      CVDUM(6)=1.D0
!
! --- Look at who is not present, zero out rows/cols accordingly
!
      DO I=1,6
         CVINF(I)=0.D0
      ENDDO
      IF ( INDEX ( WHO(1), 'X WOBBLE' ) .NE. 0  .OR. &
     &     INDEX ( WHO(1), 'X WGRate' ) .NE. 0       ) THEN
         CVINF(1)=CVDUM(1)
         IF ( INDEX ( WHO(2), 'Y WOBBLE' ) .NE.0  .OR. &
     &        INDEX ( WHO(2), 'Y WGRate' ) .NE.0        ) THEN
              CVINF(2)=CVDUM(2)
              CVINF(3)=CVDUM(3)
              IF ( INDEX ( WHO(3), 'UT1'      ) .NE. 0  .OR. &
     &             INDEX ( WHO(3), 'UT1GRate' ) .NE. 0       ) THEN
                   CVINF(4)=CVDUM(4)
                   CVINF(5)=CVDUM(5)
                   CVINF(6)=CVDUM(6)
              ENDIF
            ELSE IF ( INDEX ( WHO(2), 'UT1'      ) .NE. 0  .OR. &
     &                INDEX ( WHO(2), 'UT1GRate' ) .NE. 0   ) THEN ! Y missing
              CVINF(4)=CVDUM(4)
              CVINF(5)=CVDUM(5)
              CVINF(6)=CVDUM(6)
         ENDIF
        ELSE IF ( INDEX ( WHO(1), 'Y WOBBLE' ) .NE. 0  .OR. &
     &            INDEX ( WHO(1), 'Y WGRate' ) .NE. 0       ) THEN
         CVINF(3)=CVDUM(3)
         IF ( INDEX ( WHO(2), 'UT1'      ) .NE. 0  .OR. &
     &        INDEX ( WHO(2), 'UT1GRate' ) .NE. 0       ) THEN
!
              CVINF(5)=CVDUM(5)
              CVINF(6)=CVDUM(6)
         ENDIF
        ELSE IF ( INDEX ( WHO(1), 'UT1'      ) .NE. 0  .OR. &
     &            INDEX ( WHO(1), 'UT1GRate' ) .NE. 0       ) THEN
         CVINF(6)=CVDUM(6)
      ENDIF
!
! --- Cholesky decomposition routines:  see LINPAK for details
!
      CALL DPPFA( CVINF, INT2(3) )
      CALL DPPIN( CVINF, INT2(3) )
!
      CVINF(1)=CVINF(1)/(SIGX*SIGX) ! X cov
      CVINF(2)=CVINF(2)/(SIGX*SIGY) ! XY cov
      CVINF(3)=CVINF(3)/(SIGY*SIGY) ! Y cov
      CVINF(4)=CVINF(4)/(SIGX*SIGU) ! XU cov
      CVINF(5)=CVINF(5)/(SIGY*SIGU) ! YU cov
      CVINF(6)=CVINF(6)/(SIGU*SIGU) ! U cov
!
      RETURN
      END  !#!  COV_REOP  #!#
