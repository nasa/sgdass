      SUBROUTINE DISCONT_INIT ( ESM, NESM, ESMSITES, ESMDATES, PWC )
      IMPLICIT NONE
!
! 1.  DISCONT_INIT PROGRAM SPECIFICATION
!
! 1.1 Set up the list of discontinuities in site motions (episodic motion)
!
! 1.2 REFERENCES:
!
! 2.  DISCONT_INIT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER ESM*(*), PWC*(*) 
!
! ESM - Name of file containing site motion disontinuity information
! PWC - Name of file containing station names and epochs for piecewise
!        continuous linear station positions
!
! 2.3 OUTPUT Variables:
      INTEGER*2 NESM
      CHARACTER ESMSITES(MAX_ESM)*8, RESULT*8
      REAL*8    ESMDATES(MAX_ESM)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'fbcom.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      CHARACTER FILE*(NAME_SIZE) 
      LOGICAL*2 KFBDSP
      INTEGER*2 I
      DATA       KFBDSP / .FALSE. /
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! FILE - File name for subroutine calls
! I,J - Loop indices
! KFBDSP - True if flyby information is to be displayed
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   mwh  910227     Created, based on flyby_init
!   pet  1999.05.01 Allosed to have full paths for filenames ESM, PWC
!   pet  2005.03.17 
!
! 5.  DISCONT_INIT PROGRAM STRUCTURE
!
!      Initialization
!
      CALL USE_GLBFIL_4 ( 'OR' )
      NESM = 0
      DO I=1,MAX_ESM
         ESMSITES(I) = ' '
         ESMDATES(I) = 0.D0
      END DO
      DO I=1,2
         PWCNUM(I) = 1
      ENDDO
      DO I=1,MAX_PWC_EPS
         PWCEP(I) = 0.D0
      ENDDO
!
! --- Read Episodic Site Motion file, if any
!
      IF ( ESM .NE. ' '  .AND.  ESM .NE. 'NONE' ) THEN
           IF ( ESM(1:1) .EQ. '/' ) THEN
                FILE = ESM
              ELSE
                FILE = PRE_SAV_DIR(:PRE_SV_LEN)//ESM
           END IF
           CALL GESM ( NESM, ESMSITES, ESMDATES, FILE )
           IF ( NESM  > 0  .AND. &
     &          L_SPE > 0  .AND.  &
     &          ADR_SPE .NE. 0 ) THEN
!
                CALL CHECK_SPE_SITE ( INT4(NESM), ESMSITES, L_SPE, &
     &                                %VAL(ADR_SPE), RESULT )
                IF ( ILEN(RESULT) > 0 ) THEN
                     CALL ERR_LOG ( 8671, -3, 'DISCONT_INIT', 'Trap of '// &
     &                   'internal control: an attempt to model '// &
     &                   'displacement of site '//RESULT//' with both '// &
     &                   'expansion with B-spline basis and as site with '// &
     &                   'position discontinuity. This is not allowed. '// &
     &                   'Tip: make a new knot for spline parameterization '// &
     &                   'for the epoch of discontinuity with multiplicity '// &
     &                   'equal to the degree of the B-spline basis' )
                     WRITE ( 6, '(A)' ) 'Abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
           END IF
      END IF
!
! --- Read Piecewise continuous station file if any
!
      IF ( PWC(1:1) .NE. ' '  .AND.  PWC(1:4) .NE. 'NONE' ) THEN
           CALL CLRCH ( FILE )
           IF ( PWC(1:1) .EQ. '/' ) THEN
                FILE = PWC
              ELSE
                FILE = PRE_SAV_DIR(:PRE_SV_LEN)//PWC
           END IF
           CALL GPWC ( PWCNUM, PWCEP, PWCSITES, FILE, PWC_INTRVL )
           IF ( PWCNUM(2)  > 0  .AND. &
     &          L_SPE      > 0  .AND. &
     &          ADR_SPE .NE. 0        ) THEN
!
                CALL CHECK_SPE_SITE ( INT4(PWCNUM(2)), PWCSITES, L_SPE, &
     &                                %VAL(ADR_SPE), RESULT )
                IF ( ILEN(RESULT) > 0 ) THEN
                     CALL ERR_LOG ( 8672, -3, 'DISCONT_INIT', 'Trap of '// &
     &                   'internal control: an attempt to model '// &
     &                   'displacement of site '//RESULT//' with both '// &
     &                   'expansion with B-spline basis and modeling as '// &
     &                   'a piice-wise linear model. This is not allowed. '// &
     &                   'Tip: piece-wise linear model is equivalent to '// &
     &                   'spile of zero-th order' )
                     WRITE ( 6, '(A)' ) 'Abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
           END IF
      END IF
      CALL USE_GLBFIL_4 ( 'WC' )
!
      RETURN
      END  !#!  DISCONT_INIT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_SPE_SITE ( L_STA, C_STA, L_SPE, SPE, RESULT )
! ************************************************************************
! *                                                                      *
! *   Aucilliary program CHECK_SPE_SITE
! *                                                                      *
! * ### 17-MAR-2005  CHECK_SPE_SITE  v1.0 (c) L. Petrov  17-MAR-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  L_STA, L_SPE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      TYPE ( SPE__TYPE ) :: SPE(L_SPE)
      CHARACTER  C_STA(L_STA)*(*), RESULT*(*)
      CHARACTER  STA1*8, STA2*8
      INTEGER*4  J1, J2
!
      CALL CLRCH ( RESULT )
      DO 410 J1=1,L_STA
         STA1 = C_STA(J1)
!@U         CALL UNDSCR ( STA1 )
         DO 420 J2=1,L_SPE
            STA2 = SPE(J2)%STATION 
!@U            CALL UNDSCR ( STA2 )
            IF ( STA1 == STA2 ) THEN
                 RESULT = C_STA(J1)
                 RETURN 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  CHECK_SPE_SITE
