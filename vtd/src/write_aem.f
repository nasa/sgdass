      SUBROUTINE WRITE_AEM ( AEM, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_AEM writes into file FILOUT fields of the AEM object *
! *   that keeps parameters of the a~priori Earth rotation model.        *
! *                                                                      *
! *  ###  31-OCT-2006  WRITE_AEM  v1.2 (c)  L. Petrov  06-DEC-2006  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'aem.i'
      TYPE       ( AEM__TYPE ) :: AEM
      CHARACTER  FILOUT*128
      INTEGER*4  IUER
      CHARACTER  STR*128
      REAL*8       AMP_EPS, RATE_EPS
      PARAMETER  ( AMP_EPS  = 1.D-13 ) ! min nutation amplitude in rad
      PARAMETER  ( RATE_EPS = 1.D-30 ) ! min nutation rate in rad/s
      INTEGER*4  IOS, J1, J2, J3, J4, J5, J6, J7, J8, J9, LUN, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 2431, IUER, 'WRITE_AEM', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output file '// &
     &          FILOUT )
           RETURN 
      END IF
!
      WRITE ( LUN, '(A)' ) AEM__LABEL
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Apriori parameters for the Empirical Earth rotation model'
      WRITE ( LUN, '(A)' ) '#  Format: '
      WRITE ( LUN, '(A)' ) '#           {keyword}:: {value}'
      WRITE ( LUN, '(A)' ) '#           {keyword}: ( ind ) {value}'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'MODEL_NAME:: '//AEM%MODEL_NAME(1:I_LEN(AEM%MODEL_NAME))
      WRITE ( LUN, '(A)' ) 'MODEL_DATE:: '//AEM%MODEL_DATE(1:I_LEN(AEM%MODEL_DATE))
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( UNIT=STR, FMT='(A,I8)' ) 'N_COM:: ',AEM%N_COM
      CALL CHASHL ( STR(9:) )
      WRITE ( LUN, '(A)' ) STR(1:I_LEN(STR))
      WRITE ( LUN, '(A)' ) '#'
      DO 410 J1=1,AEM%N_COM
         WRITE ( LUN, '(A,I4,A)' ) 'MODEL_COMMENT: ( ', J1, ' ) '// &
     &                  AEM%MODEL_COMMENT(J1)(1:I_LEN(AEM%MODEL_COMMENT(J1)))
 410  CONTINUE 
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Precession parameters:'
      WRITE ( LUN, '(A)' ) '#  ======================'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A,I1)' ) 'N_PRC:: ',AEM%N_PRC
      WRITE ( LUN, '(A)' ) '#'
!
      DO 420 J2=0,AEM%N_PRC
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'DZETA:      ( ',J2,' ) ', &
     &                AEM%DZETA(J2)
 420  CONTINUE 
      WRITE ( LUN, '(A)' ) '#'
!
      DO 430 J3=0,AEM%N_PRC
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'TETA:       ( ',J3,' ) ', &
     &                AEM%TETA(J3)
 430  CONTINUE 
      WRITE ( LUN, '(A)' ) '#'
!
      DO 440 J4=0,AEM%N_PRC
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'Z:          ( ',J4,' ) ', &
     &                AEM%Z(J4)
 440  CONTINUE 
      WRITE ( LUN, '(A)' ) '#'
!
      DO 450 J5=0,AEM%N_PRC
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'EPS0:       ( ',J5,' ) ', &
     &                AEM%EPS0(J5)
 450  CONTINUE 
      WRITE ( LUN, '(A)' ) '#'
!
      DO 460 J6=0,AEM%N_PRC
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'CHI:        ( ',J6,' ) ', &
     &                AEM%CHI(J6)
 460  CONTINUE 
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Nutation parameters:'
      WRITE ( LUN, '(A)' ) '#  ===================='
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( UNIT=STR, FMT='(A,I8)' ) 'N_NUT:: ', AEM%N_NUT
      CALL CHASHL ( STR(9:) )
      WRITE ( LUN, '(A)' ) STR(1:I_LEN(STR))
      WRITE ( LUN, '(A)' ) '#'
!
      DO 470 J7=1,AEM%N_NUT
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_PHS:    ( ',J7,' ) ', &
     &                 AEM%NUT_PHS(J7)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_FRQ:    ( ',J7,' ) ', &
     &                 AEM%NUT_FRQ(J7)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_ACC:    ( ',J7,' ) ', &
     &                 AEM%NUT_ACC(J7)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_PSI_IN: ( ',J7,' ) ', &
     &                 AEM%NUT_PSI_IN(J7)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_EPS_IN: ( ',J7,' ) ', &
     &                 AEM%NUT_EPS_IN(J7)
!
         IF ( DABS(AEM%NUT_EPS_OUT(J7)) > AMP_EPS .OR. &
     &        DABS(AEM%NUT_PSI_OUT(J7)) > AMP_EPS      ) THEN
!
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_PSI_OUT: ( ',J7,' ) ', &
     &                       AEM%NUT_PSI_OUT(J7)
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_EPS_OUT: ( ',J7,' ) ', &
     &                      AEM%NUT_EPS_OUT(J7)
              
         END IF
!
         IF ( DABS(AEM%NUT_EPS_IN_RATE(J7)) > RATE_EPS .OR. &
     &        DABS(AEM%NUT_PSI_IN_RATE(J7)) > RATE_EPS      ) THEN
!
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_PSI_IN_RATE: ( ',J7,' ) ', &
     &                       AEM%NUT_PSI_IN_RATE(J7)
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_EPS_IN_RATE: ( ',J7,' ) ', &
     &                      AEM%NUT_EPS_IN_RATE(J7)
              
         END IF
!
         IF ( DABS(AEM%NUT_EPS_OUT_RATE(J7)) > RATE_EPS .OR. &
     &        DABS(AEM%NUT_PSI_OUT_RATE(J7)) > RATE_EPS      ) THEN
!
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_PSI_OUT_RATE: ( ',J7,' ) ', &
     &                       AEM%NUT_PSI_OUT_RATE(J7)
              WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'NUT_EPS_OUT_RATE: ( ',J7,' ) ', &
     &                      AEM%NUT_EPS_OUT_RATE(J7)
              
         END IF
!
         WRITE ( LUN, '(A)' ) '#'
 470  CONTINUE 
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Nominal Earth orientation and rotation rate:'
      WRITE ( LUN, '(A)' ) '#  ============================================'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A,1PD22.15)' ) 'S0::                 ', AEM%S0
      WRITE ( LUN, '(A,1PD22.15)' ) 'OMEGA_N::            ', AEM%OMEGA_N
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Polynomials for E3:'
      WRITE ( LUN, '(A)' ) '#  ==================='
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A,I1)' ) 'N_E3P:: ', AEM%N_E3P
      WRITE ( LUN, '(A)' ) '#'
      DO 480 J8=0,AEM%N_E3P
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'E3_POL:     ( ',J8,' ) ', &
     &                 AEM%E3_POL(J8)
 480  CONTINUE 
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) '#  Harmonic model for E3:'
      WRITE ( LUN, '(A)' ) '#  ======================'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A,I1)' ) 'N_E3H:: ', AEM%N_E3H
      WRITE ( LUN, '(A)' ) '#'
!
      DO 490 J9=1,AEM%N_E3H
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'E3_FRQ:     ( ',J9,' ) ', &
     &                 AEM%E3_FRQ(J9)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'E3_COS:     ( ',J9,' ) ', &
     &                 AEM%E3_COS(J9)
         WRITE ( LUN, '(A,I4,A,1PD22.15)' ) 'E3_SIN:     ( ',J9,' ) ', &
     &                 AEM%E3_SIN(J9)
         WRITE ( LUN, '(A)' ) '#'
 490  CONTINUE 
!
      WRITE ( LUN, '(A)' ) AEM__LABEL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRITE_AEM  !#!#
