      SUBROUTINE MXCMB ( MODE, N_ARR1, L, G, ARR1, ARR2, B3DOBJ, B1B3DOBJ )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  MXCMB PROGRAM SPECIFICATION
!
! 1.1 Move elements of the CGM (ARR2) Matrix into corresponding ARC
!     (ARR1) matrix
!
! 1.2 REFERENCES:
!
! 2.  MXCMB INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
      REAL*8 ARR2(* )
!
! ARR2 - CGM matrix
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ARR1(*)
!
! ARR1 - ARC matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      COMMON / NAMARC   / SAVNAM
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: back
!       CALLED SUBROUTINES: amatx
!
! 3.  LOCAL VARIABLES
!
      INCLUDE 'fast.i'
      CHARACTER  SAVNAM*(NAME_SIZE)
      INTEGER*4  MODE, N_ARR1, L, G, JA, JB, JS
      INTEGER*4  POS1, POS2, J1, J2, J3, J4
      INTEGER*8  ISH_BG, ISH_AG
      LOGICAL*2  KBIT
      INTEGER*4  I4P1, I4P2
      DATA I4P1 /1/, I4P2 /2/
!
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  I, J
      INTEGER*8  LOCC
      LOCC(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   14-FEB-97 Added support of B1D case
!   pet   27-FEB-97 Added support of B1B3D case
!   pet   04-FEB-98 Added rewriting to glbc4 saved condition number of local-
!                   local block of the nirmal matrix ot the arc under
!                   consideration
!   pet   10-JUL-98 Changed the place where arc file is read in fast mode --
!                   moved reading from mxcmb to back
!   pet   04-MAR-99 Added an actual parameter MODE.  If MODE=1, then it means
!                   that CGM^(-1) should be read from disk. MODE=2,3 is
!                   considered as an indication of that CGM^(-1) SHOULD not be
!                   read from disk since ARR2 already contains it.
!
! 5.  MXCMB PROGRAM STRUCTURE
!
!CCCCC
!      some important parameters from ../include/baccm.i
!
! ---  NPARM1 ( INTEGER*2 ) -- total number of parameters in the session under
! ---                          analysis
! ---  NPARM2 ( INTEGER*2 ) -- total number of global parameters in the run
! ---  NPARM3 ( INTEGER*2 ) -- the same as NPARM1
!
! --- Reading global-global covariance matrix to array ARR2
!
      IF ( MODE .EQ. 1 ) THEN
           CALL ACS_COVFIL   ( 'O'  )
           CALL USE_COVF_MAT ( ARR2, NPARM2, 'R' )
           CALL ACS_COVFIL   ( 'C'  )
      END IF
!
      JS = 1
      JB = 1 + 2*M_GPA
      JA = 1 + 3*M_GPA
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &     FAST_MODE .EQ. F__B1D                                     )  THEN
           IF ( MODE .EQ. 1 .OR. MODE .EQ. 2 ) THEN
!
! ------------- Reading to ARR1 intermidiary matrices and vectors for this arc
! ------------- which we produced in the forward run
!
                CALL ACS_ARCFIL   ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'O' )
                CALL USE_ARCF_MAT ( ARR1,  NPARM1, 'R' )
                CALL ACS_ARCFIL   ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'C' )
              ELSE
                NPARM1 = INT2(N_ARR1)
           END IF
      END IF
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  ) THEN
!
! -------- Move the elements of the CGM (ARR2) into the corresponding
! -------- ARC (ARR1) slots via cross reference list IX2T3.
!
           CALL AMATX ( ARR1(JA), ARR2(JA), ARR1(JB), ARR2(JB), ARR1(JS), &
     &                  ARR2(JS), IX2T3, NPARM2 )
         ELSE IF ( FAST_MODE .EQ. F__B1D ) THEN
!
! -------- Moving global covariance matrix and vector of global adjustments to
! -------- appropriate place of ARC1.
!
           ISH_AG=JA-1 + (INT8(L)*INT8(L+1))/2  + INT8(L)*INT8(G)  ! Shift for cov. matrix
           ISH_BG=JB-1 +  L                                        ! Shift for global adjustment
           DO 410 J1=1,NPARM2
              POS1 = IX2T3(J1) - L
              IF ( POS1 .GT. 0 ) THEN
                   ARR1(ISH_BG + POS1) = ARR2(JB-1 +J1)
                   DO 420 J2=1,J1
                      POS2 = IX2T3(J2) - L
                      IF ( POS2 .GT. 0 ) THEN
                         ARR1(ISH_AG + LOCC(POS1,POS2) ) = ARR2(JA-1 + LOCC(J1,J2))
                      END IF
 420               CONTINUE
              END IF
 410       CONTINUE
!
! -------- Now ARC1 has the following strucuture:
! -------- ARR1(1)   -- scale vector for parameters of this arc
! -------- ARR1(JB)  -- vector of "proto-adjustments" for this arc
! -------- ARR1(JA)  -- matrix of local-local paramreters ^-1
! -------- ARR1(JA+L*(L+1))  -- some rectungular matrix size of L*G
! -------- ARR1(JA+L*(L+1)+L*G)  -- extraction of global-global covariance
! --------              matrix which contains only those parameters which
! --------              are in this arc
! -------- ARR1(JB+L)  -- extraction of the vector of global adjustments which
! --------                contains only those parameters which are in this arc
!
         ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Moving global covariance matrix and vector of global adjustments
! -------- from ARR2 to appropriate place in B1B3DOBJ
!
           DO 430 J3=1,NPARM2
              POS1 = IX2T3(J3) - L
              IF ( POS1 .GT. 0 ) THEN
!
! -------------- Moving vector of the estimates
!
                 CALL LIB$MOVC3 ( 8, ARR2(JB-1 +J3), &
     &                            %VAL(B1B3DOBJ%AD_E00 + 8*(POS1-1) ) )
                 DO 440 J4=1,J3
                    POS2 = IX2T3(J4) - L
                    IF ( POS2 .GT. 0 ) THEN
!
! -------------------- Moving elements of global-global covariance matrix
!
                       CALL LIB$MOVC3 ( 8, ARR2(JA-1 +LOCC(J3,J4)), &
     &                      %VAL(B1B3DOBJ%AD_W00 +8*( LOCC(POS1,POS2)-1 ) ) )
                    END IF
 440             CONTINUE
              END IF
 430       CONTINUE
      END IF
!
      RETURN
      END  !#!  MXCMB  #!#
