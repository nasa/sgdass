      SUBROUTINE DO_BRK_CNST ( L_STA, C_STA, L_PAR, C_PAR, &
     &                         FAST_MODE, BRK_CNST, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DO_BRK_CNST
! *                                                                      *
! *  ### 30-APR-2009   DO_BRK_CNST  v1.0 (c)  L. Petrov 30-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'fast.i'
      INTEGER*4  FAST_MODE, L_STA, L_PAR, IUER
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  C_STA(L_STA)*8, C_PAR(L_PAR)*20
      REAL*8     BRK_CNST
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP, IE, &
     &           L_BRK, K_BRK, IND_BRK, IND_CLO_1ST, IND_CLO_2ND, IER
      LOGICAL*4  FL_DEBUG
      INTEGER*4, EXTERNAL :: IXMN8 
!
      FL_DEBUG = .TRUE.
      IF ( FL_DEBUG  ) THEN
           WRITE ( 6, * )  'DO_BRK_CNST: NUM_CLO = ', NUM_CLO  
           WRITE ( 6, * )  'DO_BRK_CNST: JDATE_CLO = ', JDATE_CLO(1:NUM_CLO)  
      END IF 
      K_BRK = 0 
      DO 410 J1=1,L_STA
         IF ( NUM_BRK(J1) > 0 ) THEN
              DO 420 J2=1,NUM_BRK(J1)
                 IP = IXMN8 ( NUM_CLO, JDATE_CLO, JDATE_BRK(J2,J1) )
                 IF ( J1 == L_STA ) THEN
                      IE = L_PAR
                    ELSE
                      IE = NSPARM(J1+1)
                 END IF
!
                 L_BRK = 0
                 IND_BRK = 0
                 IND_CLO_1ST = 0
                 IND_CLO_2ND = 0
                 DO 430 J3=NSPARM(J1)+1,IE
                    IF ( C_PAR(J3)(1:10) == C_STA(J1)//'B0' ) THEN
                         L_BRK = L_BRK + 1
                         IF ( L_BRK == J2 ) THEN
                              IND_BRK = J3
                         END IF
                    END IF
                    IF ( C_PAR(J3)(1:10) == C_STA(J1)//'C0' ) THEN
                         IF ( IND_CLO_1ST == 0 ) IND_CLO_1ST = J3
                    END IF
                    IF ( C_PAR(J3)(1:9) == C_STA(J1)//'c' ) THEN
                         IF ( IND_CLO_2ND == 0 ) IND_CLO_2ND = J3
                    END IF
 430             CONTINUE 
!
                 IF ( IND_CLO_1ST == 0 ) THEN
                      CALL ERR_LOG ( 4211, IUER, 'DO_BRK_CNST', 'Trap of '// &
     &                    'internal control: no B-spline clock were found '// &
     &                    'for station '//C_STA(J1) )
                      RETURN 
                 END IF
!
                 IF ( IND_CLO_2ND == 0 ) THEN
                      CALL ERR_LOG ( 4212, IUER, 'DO_BRK_CNST', 'Trap of '// &
     &                    'internal control: no B-spline clock were found '// &
     &                    'for station '//C_STA(J1) )
                      RETURN 
                 END IF
!
                 IF ( IND_BRK == 0 ) THEN
                      CALL ERR_LOG ( 4213, IUER, 'DO_BRK_CNST', 'Trap of '// &
     &                    'internal control: no clock breake clock were '// &
     &                    'found for station '//C_STA(J1) )
                      RETURN 
                 END IF
                 IF ( IP > 1 ) THEN
                      IND_CLO_1ST = IND_CLO_2ND + IP-2
                      IND_CLO_2ND = IND_CLO_1ST + 1
                 END IF
                 IF ( FL_DEBUG ) THEN
                      WRITE ( 6, 110 ) C_STA(J1), J2, IP, &
     &                                 IND_CLO_1ST, C_PAR(IND_CLO_1ST), &
     &                                 IND_CLO_2ND, C_PAR(IND_CLO_2ND), &
     &                                 IND_BRK, C_PAR(IND_BRK) 
 110                  FORMAT ( 'DO_BRK_CNS  ',A, ' brk: ', I1, &
     &                         ' Ind_seg: ', I3, &
     &                         ' Ind_clo_1st: ', I5, 1X, A, &
     &                         ' Ind_clo_2nd: ', I5, 1X, A, &
     &                         ' Ind_brk: ', I5, 1X, A )
                 END IF
!
                 K_BRK = K_BRK + 1
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_NAM ( 'CLO_BRK', K_BRK, 'Clock_breaks', 'sec', &
     &                              0.0D0, BRK_CNST, .FALSE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4214, IUER, 'DO_BRK_CNST', 'Error '// &
     &                    'in an attempt to put information about clock '// &
     &                    'break constraint' )
                      RETURN
                 END IF
!
! -------------- Add constraint equation
!
!!  write ( 6, * ) 
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( 'CLO_BRK', K_BRK, IND_CLO_1ST, &
     &                             (JDATE_BRK(J2,J1) - JDATE_CLO(IP+1))/ &
     &                             (JDATE_CLO(IP+1) - JDATE_CLO(IP)), &
     &                             .FALSE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4215, IUER, 'DO_BRK_CNST', 'Error '// &
     &                    'in an attempt to put information about '// &
     &                    'the first term of the clock break constraint '// &
     &                    'equation' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( 'CLO_BRK', K_BRK, IND_CLO_2ND, &
     &                             (JDATE_BRK(J2,J1) - JDATE_CLO(IP))/ &
     &                             (JDATE_CLO(IP+1) - JDATE_CLO(IP)), &
     &                             .FALSE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4216, IUER, 'DO_BRK_CNST', 'Error '// &
     &                    'in an attempt to put information about '// &
     &                    'the second term of the clock break constraint '// &
     &                    'equation' )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( 'CLO_BRK', K_BRK, IND_BRK, 1.0D0, &
     &                             .FALSE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4217, IUER, 'DO_BRK_CNST', 'Error '// &
     &                    'in an attempt to put information about '// &
     &                    'the thhird term of the clock break constraint '// &
     &                    'equation' )
                      RETURN
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
      IF ( FL_DEBUG  ) THEN
           WRITE ( 6, * )  'DO_BRK_CNST: K_BRK = ' , K_BRK 
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   !#!  DO_BRK_CNST #!#
