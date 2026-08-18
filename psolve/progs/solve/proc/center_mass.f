        SUBROUTINE CONSTRAIN_CM ( FAST_MODE, FAST_DBG, LIN_STA_SIGMA, NPARM, &
     &                            LPARM, LPARM_CON, WT, NUM_WTS, A, B3DOBJ, &
     &                            B1B3DOBJ, CNSTROBJ )
! ************************************************************************
! *                                                                      *
! *         Constrain some linear combination of station positions.      *
! *                                                                      *
! * Modifications:                                                       *
! *                                                                      *
! * jmg 951127 Fix pathological condition arising when site not used.    *
! * jmg 960610 Remove unused get_names subroutine.  (A cutil subroutine  *
! *            by that name has been created as part of the project      *
! *            to remove holleriths.)                                    *
! * pet 970131 Added support for B3D mode. Added IMPLICIT NONE. "Combed" *
! *            source code.                                              *
! * pet 970226 Added support for B1B3D mode.                             *
! * pet 980119 Added support of CNSTROBJ data structure.                 *
! * pet 980205 Declared the sigma of the constrain in solve.i instead of *
! *            hard-coded value. Write down information about the type   *
! *            of applied constraint.                                    *
! * pet 980722 Made LIN_STA_SIGMA formal parameter instead of named      *
! *            constatnt in solve.i block.                               *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  NPARM
      REAL*8     LIN_STA_SIGMA, A(*)
!
      CHARACTER*20 LPARM(*)
      INTEGER*4    ISTAT(MAX_STA,3)
      REAL*8       RSTAT(MAX_STA,3)
      INTEGER*4    IWT, I, IS, J, JS, NUM_STAT(3)
      INTEGER*4    IPARM
      REAL*8       RNORM(3)
!
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      CHARACTER*11 LCOMP(3)
      INTEGER*4    NUM_WTS
      CHARACTER*20 LPARM_CON(*)
      REAL*8       WT(*)
!C
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  FAST_MODE, FAST_DBG, IC, TYP_I, NBS_I, IR_I, IC_I 
      INTEGER*8  POS 
      ADDRESS__TYPE :: ADR
      REAL*8     A_DIAG, UPD, EPS
      PARAMETER  ( EPS = 1.D-25 )
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D, FULL_B1B3D
      INTEGER*8, EXTERNAL :: INDX8
!
      DATA LCOMP/"X COMPONENT","Y COMPONENT","Z COMPONENT"/
!
!     ! Transformation INT2 -> INT4
!
! ----- Initialization.
!
      DO IC=1,3
         NUM_STAT(IC) = 0.
         RNORM(IC)    = 0.
      END DO
!
! ----- Search for matches between parameters and constrained stations.
! ----- build three vectors (one each for X, Y,Z).  Each vector has two parts:
! ----- an index into the parameter space (ISTAT) and a weight.
!
      A_DIAG = -1.0D0
      DO IPARM=1,NPARM
           IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD .OR. &
     &          FAST_MODE .EQ. F__B1D  ) THEN
!
! ------------- FULL case
!
                POS = INDX8 ( IPARM, IPARM )
                A_DIAG = A(POS)
             ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------- B3D case
!
!                ADR = FULL_B3D ( B3DOBJ, IPARM, IPARM, &
!     &                           %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                ADR = FULL_B3D ( B3DOBJ, IPARM, IPARM, &
     &                           TYP_I, NBS_I, IR_I, IC_I )
                CALL LIB$MOVC3 ( 8, %VAL(ADR), A_DIAG )
             ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------- B1B3D case
!
                ADR = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, IPARM, IPARM, &
     &                             %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                CALL LIB$MOVC3 ( 8, %VAL(ADR), A_DIAG )
           END IF
!
           IF ( ABS( A_DIAG ) .GT. EPS ) THEN
              DO IWT=1,NUM_WTS
                 IF ( LPARM(IPARM) .EQ. LPARM_CON(IWT) ) THEN
                      DO IC=1,3
                         IF ( INDEX ( LPARM(IPARM), LCOMP(IC) ) .NE. 0 ) THEN
!
                              NUM_STAT(IC)=NUM_STAT(IC)+1
                              ISTAT(NUM_STAT(1),IC)=IPARM
                              RSTAT(NUM_STAT(1),IC)=WT(IWT)
                              RNORM(IC)=RNORM(IC)+WT(IWT)*WT(IWT)
                              GOTO 100
                       ENDIF
                    ENDDO
                 ENDIF
              END DO
100           CONTINUE
           ENDIF
        END DO
!
! ----- Normalize constrain vectors.
!
        DO IC=1,3
           RNORM(IC)=SQRT(RNORM(IC))
           DO IPARM=1,NUM_STAT(IC)
              RSTAT(IPARM,IC)=RSTAT(IPARM,IC)/RNORM(IC)
           END DO
        END DO
!
! ----- Now do constraint for each of the three directions.
!
        DO IC=1,3
           DO I=1,NUM_STAT(IC)
              IS=ISTAT(I,IC)
              DO J=I,NUM_STAT(IC)
                 JS=ISTAT ( J, IC )
!
! -------------- Add information about the type of the constraint applied
!
                 CALL ADD_TYCNS ( 'LIN_STA', 'Linear comb. of station '// &
     &               'posit.', 'meter', LIN_STA_SIGMA, FALSE_L4, CNSTROBJ )
!
! -------------- UPD -- Stuff which will be added TO NORMAL MATRIX
!
                 UPD = RSTAT(I,IC)*RSTAT(J,IC)/LIN_STA_SIGMA**2
!
                 CALL ADD_CNSTR ( IS, JS, UPD, CNSTROBJ )
              END DO
           END DO
        END DO
!
        RETURN
        END  !#!  CONSTRAIN_CM  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE GET_CM_WTS ( LPARM_WT, WT, NUM_WTS )
        IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
        INCLUDE    'solve.i'
        INCLUDE    'glbc4.i'
        INCLUDE    'precm.i'
!
        INTEGER*4    ISTAT, IND, I
        INTEGER*4    NUM_WTS, IOS
        CHARACTER*20 LPARM_WT(3,MAX_STA)
        REAL*8       WT(3,MAX_STA)
        CHARACTER    LDUM*80, FINAM*255
        CHARACTER    LCOMP(3)*11, LSTAT*8
        DATA LCOMP / "X COMPONENT", "Y COMPONENT", "Z COMPONENT" /
        INTEGER*4,  EXTERNAL :: I_LEN
!
! ----- Get weights to be used in cm constraints.
! ----- presumably will have a better way of getting this stuff in latter.
!
        CALL CLRCH ( FINAM )
        IF ( STA_WT_FIL(1:1) .EQ. '/' ) THEN
             FINAM = STA_WT_FIL
           ELSE
             FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//STA_WT_FIL
        END IF
        OPEN ( 66, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
        CALL FERR ( INT2(IOS), "Opening station weight file: "// &
     &              FINAM(1:I_LEN(FINAM)), INT2(0), INT2(0) )
!
! ----- This file has station names followed by wts for each component.
! ----- any line that starts with * is ignored.
!
        NUM_WTS=0
        ISTAT=0
!
! ----- Beginning of indirect cycle
!
100     CONTINUE
           READ ( 66, '(A80)', END=200, ERR=100 ) LDUM
           IF ( LDUM(1:1) .EQ. "*" ) GOTO 100
!
           ISTAT=ISTAT+1
           LSTAT=LDUM(1:8)
           READ ( LDUM(9:80), * ) WT(1,ISTAT), WT(2,ISTAT), WT(3,ISTAT)
!
! -------- REPLACE "_" WITH " "
!
           IND=INDEX(LSTAT,"_")
           DO WHILE(IND .NE. 0)
              LSTAT(IND:IND)=" "
              IND=INDEX(LSTAT,"_")
           END DO
           DO I=1,3
              LPARM_WT(I,ISTAT)=LSTAT//" "//LCOMP(I)
           END DO
        GOTO 100
!
! ----- End of indirect cycle
!
200     CONTINUE
!
! ----- End of work. Close file
!
        CLOSE ( 66 )
!
        RETURN
        END  !#!  GET_CM_WTS  #!#
