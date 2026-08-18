      SUBROUTINE GET_WAHR1980 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_WAHR1980 gets nutation expansion WAHR1980.             *
! *                                                                      *
! *  ### 14-MAR-2004  GET_WAHR1980  v1.0 (c) L. Petrov  14-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'wahr1980_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      INTEGER*4  J1
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3351, IUER, 'GET_WAHR1980', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
      L_HEO = N_NUT
      DO 410 J1=1,N_NUT
         PHS_HEO(J1) = PHAS_NUT(J1)
         FRQ_HEO(J1) = FREQ_NUT(J1)
         ACC_HEO(J1) = ACCL_NUT(J1)
         PMC_HEO(J1) = PMC_NUT(J1)*1.D-12
         PMS_HEO(J1) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(J1) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(J1) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_WAHR1980  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_IERS1996 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO,  &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_IERS1996 gets nutation expansion IERS1996. It includes *
! *   luni-solar nutations, Celestial Ephemeris pole offset and          *
! *   correction to precession. It does not include FCN terms. It does   *
! *   not include geodesic nutation.                                     *
! *                                                                      *
! *  ### 14-MAR-2004  GET_IERS1996  v2.1 (c) L. Petrov  09-APR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut_const.i'
      INCLUDE   'iers1996_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      LOGICAL*4  FL_PREC_ADDED
      INTEGER*4  IND, J1
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3361, IUER, 'GET_IERS1996', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
!
      L_HEO = N_NUT+1
      FL_PREC_ADDED = .FALSE.
      DO 410 J1=1,N_NUT
         IND = J1
         IF ( FREQ_NUT(J1) .GT. -(OM__EAR + OM_PRC)  .AND.  &
     &        .NOT.  FL_PREC_ADDED                        ) THEN
!
              PHS_HEO(IND) = 0.0D0
              FRQ_HEO(IND) = -(OM__EAR + OM_PRC)
              ACC_HEO(IND) = 0.0D0
              PMC_HEO(IND) = EPS_OFFS_IERS96*MAS__TO__RAD
              PMS_HEO(IND) = PSI_OFFS_IERS96*MAS__TO__RAD*DSIN(EPSILON_0)
              PMC_RATE_HEO(IND) = EPS_RATE_IERS96*MAS__TO__RAD/YEAR__TO__SEC
              PMS_RATE_HEO(IND) = PSI_RATE_IERS96*MAS__TO__RAD*DSIN(EPSILON_0)/YEAR__TO__SEC
              FL_PREC_ADDED = .TRUE.
         END IF
!
         IF ( FL_PREC_ADDED ) THEN
              IND = J1 + 1
         END IF
!
         PHS_HEO(IND) = PHAS_NUT(J1)
         FRQ_HEO(IND) = FREQ_NUT(J1)
         ACC_HEO(IND) = ACCL_NUT(J1)
         PMC_HEO(IND) = PMC_NUT(J1)*1.D-12
         PMS_HEO(IND) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(IND) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(IND) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
      IF ( FL_PREC_ADDED ) L_HEO = N_NUT+1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_IERS1996  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                         PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_REN2000 gets nutation expnasion REN2000.               *
! *                                                                      *
! *  ### 17-OCT-2003  GET_REN2000  v1.0 (c)  L. Petrov  17-OCT-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ren2000_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      INTEGER*4  J1
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3352, IUER, 'GET_REN2000', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
      L_HEO = N_NUT
      DO 410 J1=1,N_NUT
         PHS_HEO(J1) = PHAS_NUT(J1)
         FRQ_HEO(J1) = FREQ_NUT(J1)
         ACC_HEO(J1) = ACCL_NUT(J1)
         PMC_HEO(J1) = PMC_NUT(J1)*1.D-12
         PMS_HEO(J1) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(J1) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(J1) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_REN2000  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_MHB2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                         PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MHB2000 gets nutation expansion MHB2000.               *
! *                                                                      *
! *  ### 17-OCT-2003  GET_MHB2000  v1.1 (c)  L. Petrov  09-APR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut_const.i'
      INCLUDE   'mhb2000_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      LOGICAL*4  FL_PREC_ADDED
      INTEGER*4  J1, IND
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3353, IUER, 'GET_MHB2000', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
!
      FL_PREC_ADDED = .FALSE.
      L_HEO = N_NUT
      DO 410 J1=1,N_NUT
         IND = J1
         IF ( FREQ_NUT(J1) .GT. -(OM__EAR + OM_PRC)  .AND.  &
     &        .NOT.  FL_PREC_ADDED                        ) THEN
!
              PHS_HEO(IND) = 0.0D0
              FRQ_HEO(IND) = -(OM__EAR + OM_PRC)
              ACC_HEO(IND) = 0.0D0
              PMC_HEO(IND) = EPS_OFFS_MHB2000*MAS__TO__RAD
              PMS_HEO(IND) = PSI_OFFS_MHB2000*MAS__TO__RAD*DSIN(EPSILON_0)
              PMC_RATE_HEO(IND) = EPS_RATE_MHB2000*MAS__TO__RAD/YEAR__TO__SEC
              PMS_RATE_HEO(IND) = PSI_RATE_MHB2000*MAS__TO__RAD*DSIN(EPSILON_0)/YEAR__TO__SEC
              FL_PREC_ADDED = .TRUE.
         END IF
!
         IF ( FL_PREC_ADDED ) THEN
              IND = J1 + 1
         END IF
!
         PHS_HEO(IND) = PHAS_NUT(J1)
         FRQ_HEO(IND) = FREQ_NUT(J1)
         ACC_HEO(IND) = ACCL_NUT(J1)
         PMC_HEO(IND) = PMC_NUT(J1)*1.D-12
         PMS_HEO(IND) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(IND) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(IND) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
      IF ( FL_PREC_ADDED ) L_HEO = N_NUT+1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_MHB2000  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_MHB2000_TRANSF ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                         PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MHB2000_TRANSF gets nutation expnasion MHB2000_TRANSF. *
! *                                                                      *
! * ## 29-DEC-2003 GET_MHB2000_TRANSF  v1.0 (c) L. Petrov 29-DEC-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'mhb2000_transf_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      INTEGER*4  J1
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3354, IUER, 'GET_MHB2000', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
      L_HEO = N_NUT
      DO 410 J1=1,N_NUT
         PHS_HEO(J1) = PHAS_NUT(J1)
         FRQ_HEO(J1) = FREQ_NUT(J1)
         ACC_HEO(J1) = ACCL_NUT(J1)
         PMC_HEO(J1) = PMC_NUT(J1)*1.D-12
         PMS_HEO(J1) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(J1) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(J1) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_MHB2000_TRANSF  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_MHB2000_ADDON ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                         PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MHB2000_ADDON gets nutation expansion MHB2000_ADDON.   *
! *                                                                      *
! * ## 29-DEC-2003  GET_MHB2000_ADDON  v1.1 (c) L. Petrov 09-APR-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut_const.i'
      INCLUDE   'mhb2000_addon_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      LOGICAL*4  FL_PREC_ADDED
      INTEGER*4  J1, IND
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3355, IUER, 'GET_MHB2000', 'Parameter M_HEO is '// &
     &         'too small' )
           RETURN 
      END IF
!
      FL_PREC_ADDED = .FALSE.
      L_HEO = N_NUT
      DO 410 J1=1,N_NUT
         IND = J1
         IF ( FREQ_NUT(J1) .GT. -(OM__EAR + OM_PRC)  .AND.  &
     &        .NOT.  FL_PREC_ADDED                        ) THEN
!
              PHS_HEO(IND) = 0.0D0
              FRQ_HEO(IND) = -(OM__EAR + OM_PRC)
              ACC_HEO(IND) = 0.0D0
              PMC_HEO(IND) = EPS_OFFS_MHB2000*MAS__TO__RAD
              PMS_HEO(IND) = PSI_OFFS_MHB2000*MAS__TO__RAD*DSIN(EPSILON_0)
              PMC_RATE_HEO(IND) = EPS_RATE_MHB2000*MAS__TO__RAD/YEAR__TO__SEC
              PMS_RATE_HEO(IND) = PSI_RATE_MHB2000*MAS__TO__RAD*DSIN(EPSILON_0)/YEAR__TO__SEC
              FL_PREC_ADDED = .TRUE.
         END IF
!
         IF ( FL_PREC_ADDED ) THEN
              IND = J1 + 1
         END IF
!
         PHS_HEO(IND) = PHAS_NUT(J1)
         FRQ_HEO(IND) = FREQ_NUT(J1)
         ACC_HEO(IND) = ACCL_NUT(J1)
         PMC_HEO(IND) = PMC_NUT(J1)*1.D-12
         PMS_HEO(IND) = PMS_NUT(J1)*1.D-12
         PMC_RATE_HEO(IND) = PMC_RATE_NUT(J1)*1.D-24
         PMS_RATE_HEO(IND) = PMS_RATE_NUT(J1)*1.D-24
 410  CONTINUE 
      IF ( FL_PREC_ADDED ) L_HEO = N_NUT+1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_MHB2000_ADDON  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_MHB2000_M_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &           ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO,  &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MHB2000 gets the difference of nutation expnasions     *
! *   MHB2000 minus REN2000.                                             *
! *                                                                      *
! * # 03-NOV-2003 GET_MHB2000_M_REN2000 v1.0 (c) L. Petrov 03-NOV-2003 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'mhb2000_heo.i'
      INTEGER*4  M_HEO, L_HEO, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      INTEGER*4  J1, IER
!
      IF ( M_HEO .LT. N_NUT ) THEN
           WRITE( 6, * ) 'M_HEO=',M_HEO, ' N_NUT=',N_NUT
           CALL ERR_LOG ( 3356, IUER, 'GET_MHB2000_M_REN2000', 'Parameter '// &
     &         'M_HEO is too small' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GET_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                   PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3357, IUER, 'GET_MHB2000_M_REN2000', 'Error in '// &
     &         'getting expansion REN2000' )
           RETURN 
      END IF
!
      IF ( L_HEO .NE.  N_NUT ) THEN
           WRITE( 6, * ) ' L_HEO=',L_HEO,' N_NUT=',N_NUT
           CALL ERR_LOG ( 3358, IUER, 'GET_MHB2000_M_REN2000', &
     &            'Inconsistent expansion' )
           RETURN 
      END IF
      DO 410 J1=1,N_NUT
         IF ( DABS ( PHS_HEO(J1) - PHAS_NUT(J1) ) .GT. 1.0D-8 ) THEN
              WRITE ( 6, * ) ' J1=',J1, ' PHS_HEO(J1)=', PHS_HEO(J1), &
     &                       ' PHAS_NUT(J1)=',PHAS_NUT(J1)
              CALL ERR_LOG ( 3359, IUER, 'GET_MHB2000_M_REN2000', &
     &                      'Inconsistent phase' )
              RETURN 
         END IF
         IF ( DABS ( FRQ_HEO(J1) - FREQ_NUT(J1) ) .GT. 1.0D-15 ) THEN
              WRITE ( 6, * ) ' J1=',J1,' FRQ_HEO(J1)=', FRQ_HEO(J1), &
     &                       ' FREQ_NUT(J1)=',FREQ_NUT(J1)
              CALL ERR_LOG ( 3360, IUER, 'GET_MHB2000_M_REN2000', &
     &            'Inconsistent phase' )
              RETURN 
         END IF
!
         PMC_HEO(J1) = PMC_NUT(J1)*1.D-12 - PMC_HEO(J1)
         PMS_HEO(J1) = PMS_NUT(J1)*1.D-12 - PMS_HEO(J1)
         PMC_RATE_HEO(J1) = PMC_RATE_NUT(J1)*1.D-24 - PMC_RATE_HEO(J1)
         PMS_RATE_HEO(J1) = PMS_RATE_NUT(J1)*1.D-24 - PMS_RATE_HEO(J1)
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_MHB2000_M_REN2000 #!#
