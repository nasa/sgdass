      SUBROUTINE COVP_MAIN ( ARR, F_IO_NRM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COVP_MAIN PROGRAM SPECIFICATION
!
! 1.1 COVP IS THE ROUTINE WHICH WILL TAKE THE COVARIANCE MATRIX
!     (THE INVERSE OF THE NORMAL EQUATIONS MATRIX) AND CALCULATE
!     AND PRINT THE CORRELATION MATRIX.
!
! 1.2 REFERENCES:
!
! 2.  COVP_MAIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covp
!       CALLED SUBROUTINES: cosrt
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2   KBIT, F_IO_NRM
      INTEGER*2   IXC(10), IYC(10), IXYNAM(16,M_GPA), I, IDMP, J
      INTEGER*4   I4P0, I4P1, I4P51, I4P255, I4P256
      REAL*8      FCORR(10), DIV(M_GPA)
      CHARACTER   STR*80
      REAL*8      ARR(*), DSP(M_GPA)
      COMMON / LCLEMA / IXYNAM, DIV
      INTEGER*4   NXPNT, IFIR, JJ, JA, JB, JS, J1,J2, J3
!!      DATA IDMP  /2H--/
      DATA I4P0, I4P1,I4P51,I4P255,I4P256 /0,1,51,255,256/
      INTEGER*4  I4, J4
      INTEGER*8  LOCC
      LOCC(I4,J4)= INT8(MIN(I4,J4)) + (INT8(MAX(I4,J4))*INT8(MAX(I4,J4)-1))/2
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!   JLR  921215  Replaced nJ's with I4Pn's
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   pet  970613  Implement MATView interface for interactive mode. Updated
!                comments
!
! 5.  COVP_MAIN PROGRAM STRUCTURE
!
!
! --- Calculate the correlation matrix
!
      IF ( KBATCH .OR. ( KMINOUT .AND.  .NOT. KSCREEN) ) GOTO 810
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS =   M_GPA
!
      IF ( F_IO_NRM ) THEN
!
! --- Read normal equations file
!
           CALL USE_NRMFIL ( ARR, NPARAM, 'ORC' )
      END IF
!
!---- INITIALIZE FCORR
!
      DO 10 I = 1,10
         FCORR(I) = 0.0
   10 CONTINUE
!
! --- Open and read PARFIL
!
      CALL USE_PARFIL ( 'ORC' )
!
      IF ( KBIT( IPRES, INT2(3) ) .AND. KSCREEN ) THEN
           CALL END_MN()    ! Postpone curses
           CALL UN_CURSES() ! Elimination of the influence of curses
!
! -------- Request: how it would be better to print normal matrix?
!
           WRITE ( 6, FMT='(A)' ) ' '
           WRITE ( 6, FMT='(A)' ) 'COVP: Where to print correlation '// &
     &                'matrix: in spool_file or at the screen '
           WRITE ( 6, FMT='(A$)' ) '(MATView interface)   '// &
     &                                   ' File/(Screen) >> '
           READ ( UNIT=5, FMT='(A)' ) STR
           CALL CHASHL ( STR )
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:1) .EQ. 'F' .AND. KSPOOL ) THEN
              WRITE ( 23, 200 ) IRNCD
  200         FORMAT( " CORRELATION MATRIX FOR RUN ",I5,"-"I4 )
              NXPNT = I4P1
              ARR(JA+NXPNT) = 1.0D0
              IF ( ARR(JS+1) .GT. 1.0D-34 ) THEN
                 DIV(1)=1.0D0/ARR(JS+1)
               ELSE
                 DIV(1)=0.0D0
              ENDIF
!
! ----------- Fix sig as we go so that condition number are correct
!
              IF ( ARR(1) .GT. 1.0D-34 ) THEN
                   ARR(JS+1) = ARR(JS+1)/ARR(1)
                ELSE
                   ARR(JS+1)=0.0D0
              ENDIF
!
              WRITE ( 23, 275 ) NXPNT, ARR(JA+NXPNT)
              DO I=2,NPARAM
                 IF ( ARR(JS+I) .GT. 1.0D-34 ) THEN
                      DIV(I)=1.0D0/ARR(JS+i)
                   ELSE
                      DIV(I)=0.0D0
                 ENDIF
                 IF ( ARR(I) .GT. 1.0D-34 ) THEN
                      ARR(JS+I) = ARR(JS+I)/ARR(I)
                   ELSE
                      ARR(JS+I)=0.0D0
                 ENDIF
                 IFIR  = NXPNT + I4P1
                 DO J=1,I-1
                    NXPNT = NXPNT+I4P1
                    ARR(JA+NXPNT) = ARR(JA+NXPNT)*DIV(I)*DIV(J)
!
!------------------ Find the ten largest elements of the correlation matrix
!
                    CALL COSRT ( ARR(JA+NXPNT), I, J, FCORR, IXC, IYC )
                 ENDDO
                 NXPNT = NXPNT+I4P1
                 ARR(JA+NXPNT)=1.0D0
                 WRITE(23,275) I, ( ARR(JA+JJ), JJ=IFIR, NXPNT )
  275            FORMAT ( 1X, I5, '. ',200(15(1X,F7.4)/8X) )
              ENDDO
!
! ----------- Close spool file
!
              CALL USE_SPOOL('C' )
             ELSE
!
! ----------- Interctive printout at the screen
!
              DO 410 J1=1,NPARAM
                 DSP(J1) = SQRT ( ARR(JA +LOCC(J1,J1) ) )
 410          CONTINUE
!
! ----------- Transformation covariance matrix to correlation matrix
!
              DO 420 J2=1,NPARAM
                 DO 430 J3=1,J2
                    IF ( ABS(DSP(J3)*DSP(J2)) .GT. 1.D-30 ) THEN
                         ARR(JA +LOCC(J3,J2) ) = ARR(JA +LOCC(J3,J2) ) &
     &                                       /(DSP(J3)*DSP(J2))
                      ELSE
!
! ---------------------- Special trick to show that something was wrong
!
                         ARR(JA +LOCC(J3,J2) ) = -2.D0
                     END IF
 430             CONTINUE
 420          CONTINUE
!
! ----------- And at last -- calling MATView interface
!
              CALL MATVIEW ( 3, NPARAM, NPARAM, ARR(JA+1), &
     &            'Correlation matrix', '(F5.3,1X)', 1, 1, -3 )
           END IF
      END IF
      CALL USE_COMMON ( 'OWC' )
 810  CONTINUE
!
      END  !#!  COVP_MAIN  #!#
