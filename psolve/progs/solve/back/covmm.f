      SUBROUTINE COVMM ( A, A2, B, B2, SCAL, SCAL2 )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COVMM PROGRAM SPECIFICATION
!
! 1.1 Produce covariance matrix
!
! 1.2 REFERENCES:
!
! 2.  COVMM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 SCAL(*),B(*),A(*),B2(*),A2(*)
!
! A,B,SCAL - Sub-sets of ARC matrix
! A2,B2 - Sub-sets of CGM matrix
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SCAL2(*)
!
! SCAL2 -  Subset of CGM matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      INTEGER*2 IPARM1(10,M_GPA),IPARM2(10,M_GPA)
      INTEGER*2 IPARM3(10,M_GPA)
      COMMON / PARAM / IPARM1, IPARM2, IPARM3
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cvrnc
!       CALLED SUBROUTINES: out_cgm,out_lst,out_mtx,out_one,arc_arc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IWDS,ISTS
      PARAMETER ( IWDS=10, ISTS=1)
!
      INTEGER*2 I,J,K,DUMMY(3),IPRMS
      INTEGER*4 IXCTC(M_GPA), J1, J2, J3
      INTEGER*2 IOFST,JOFST,IERR,NUMDD,LCL
      INTEGER*2 CPAR1, CPAR2, INT2_ARG
      INTEGER*4 M4, N4, R, INDX4
      INTEGER*8 INDX8
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!
!  mwh  900418  Modify call to out_cgm to allow STA or SOU covariance option
!  pet  971105  Added error message to catch the sitution when no parameters
!               were found for calculation covariation
!  pet  2000.09.25  Added opening/closing COVFxx file with global-global
!                   covarianve matrix
!     :2002.12.19:jwr: TRUE__L2 and FALSE__L2 introduced for -i2 removal
!
! 5.  COVMM PROGRAM STRUCTURE
!
      CALL ACS_COVFIL   ( 'O'  )
      DO J1=1,M_GPA !set up dummy x-ref list for cgm
         IXCTC(J1) = J1
      ENDDO
      IF ( I_ARC.EQ.0 ) THEN !print cgm matrix on first arc alone
           IF ( IARCNM.EQ.1 ) THEN
                CALL USE_COVF_MAT ( SCAL2, NPARM2, 'R' )
                DO J2=1,NPARM2
                   TVECT(J2)=A2 ( INDX8(J2,J2) )
                ENDDO
                IF ( ICOV(1:3) .NE. 'ALL' ) THEN
                     CPAR1 = IND_ICOV(1)
                     DO J3=1,NPARM2
                        IF (IND_ICOV(J3).GT.0) CPAR2 = IND_ICOV(J3)
                     ENDDO
                  ELSE
                     CPAR1 = 1
                     CPAR2 = NPARM2
                ENDIF
!
                IF ( CPAR1 .LE. 0   .OR.  CPAR2 .LE. 0 ) THEN
                     WRITE ( 6, * ) ' cpar1 = ',cpar1,' cpar2 = ',cpar2
                     CALL ERR_LOG ( 4601, -1, 'COVMM', 'Failure to '// &
     &                   'calculate indeces of the parameters whose covariations '// &
     &                   'are to be calculated. Check you BATOPT configure file. '// &
     &                   'Draw special attention at keyword: COVARIANCES and '// &
     &                   'content the sections $FLAGS, $CARRY. Are you sure that '// &
     &                   'at least one parameters whose covariation you are eager '// &
     &                   'to see should (and is) really estimated?' )
                     STOP 'BACK. Abnormal termination'
                END IF
                CALL OUT_CGM ( INAMCG, A2, B2, TVECT, IPARM2, CPAR1, CPAR2, &
     &                         JMAX_TRI, M_GPA, IWDS )
         ENDIF
       ELSE IF(I_ARC.EQ.IARCNM) THEN
        IF ( JCOV(1:3) .EQ. 'ALL' ) THEN
           DO J3=1,NPARM3
              TVECT(J3) = A( INDX8( J3, J3) )
           ENDDO
           CALL OUT_MTX( IPARM1, J_ARCNAME, J_ARCNAME, A, B, TVECT, INT2(1), &
     &          IARCS, INT2(1), IARCS, TRUE__L2, TRUE__L2, JMAX_TRI, M_GPA, IWDS, &
     &          IARCNM, IARCNM )
!
           CALL OUT_MTX( IPARM1, INAMCG, J_ARCNAME, A, B, TVECT, IARCS+1, &
     &          NPARM3, INT2(1), IARCS, FALSE__L2, FALSE__L2, JMAX_TRI, M_GPA, &
     &          IWDS, INT2(0), IARCNM )
!
           IF(I_ARC.EQ.1) THEN
              CALL USE_COVF_MAT(SCAL2,NPARM2,'R' )
              DO J2=1,NPARM2
                 TVECT(J2) = A2 ( INDX8(J2,J2) )
              ENDDO
              CALL OUT_CGM( INAMCG, A2, B2, TVECT, IPARM2, INT2(1), nparm2, &
     &             JMAX_TRI, M_GPA, IWDS )
           ENDIF
         ELSE
!
!   the jcov keyword is something other than 'ALL:' like 'NUT,' 'STA,'
!   or some such.  use OUT_LST since we don't explicitely know, except
!   as indicated in IND_JCOV & IND_ICOV, which parts  of the matrix to dump.
!
        IF(PERMUT) THEN
!
!   extract the diagonal elements
!
         DO J3=1,NPARM3
            TVECT(I)=A(INDX8(J3,J3))
         ENDDO
         CALL OUT_LST(IND_JCOV,IPARM1,J_ARCNAME,A,B,TVECT, &
     &                END_JDX,JMAX_TRI,M_GPA,IWDS,IARCNM )
        ELSE
         CALL OUT_ONE( IND_JCOV, IND_JCOV, IPARM2, IPARM2, IARCS, IARCS, &
     &        I_ARCNAME, I_ARCNAME, A, INT2(0), INT2(0), TVECT, JMAX_TRI, M_GPA, &
     &        IWDS )
        ENDIF
       ENDIF
      ELSE
!
! ----- Form [V]ij from A & A2.  the # globals is = CPARAMS since
! ----- we have reordered the matricies to include all globals in the
! ----- covfile, in the covfile order.
!
        CALL ARC_ARC ( A, A2, I_ARCNAME, J_ARCNAME )
      ENDIF
      CALL ACS_COVFIL   ( 'C' )
!
      RETURN
      END  !#!  COVMM  #!#
