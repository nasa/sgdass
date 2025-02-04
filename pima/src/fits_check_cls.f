       PROGRAM    FITS_CHECK_CLS_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL FITS_CHECK_CLS()
       END  PROGRAM  FITS_CHECK_CLS_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE FITS_CHECK_CLS()
! ************************************************************************
! *                                                                      *
! *   Program FITS_CHECK_CLS
! *                                                                      *
! * ### 06-OCT-2013  FITS_CHECK_CLS  v1.0 (c)  L. Petrov 06-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FILFITS*256
      INTEGER*4  MARG
      PARAMETER  ( MARG = 32 )
      CHARACTER  STR*128
      REAL*8     GAP_SCAN, GAP_SCAN_DEF
      PARAMETER  ( GAP_SCAN_DEF = 300.0D0 )
      CHARACTER  FIL*128
      LOGICAL*4  FL_WEI_USE
      INTEGER*4  J1, J2, IDEV, ICLR, ISIZE, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      GAP_SCAN = GAP_SCAN_DEF
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fits_to_check_cls [gap_scan]'
           CALL EXIT ( 0 )
         ELSE
           CALL GETARG ( 1, FILFITS )
           IF ( IARGC() .GE. 2 ) THEN 
                CALL GETARG ( 2, STR )
                READ ( UNIT=STR, FMT='(F10.2)' ) GAP_SCAN
           END IF
      END IF     
!
      VIS%SKY_FRQ => NULL()
      VIS%MJD     => NULL()
      VIS%TAI     => NULL()
      VIS%VIS     => NULL()
      VIS%UV      => NULL()
      VIS%WEI     => NULL()
      VIS%IND_BAS => NULL()
      VIS%INT_TIM => NULL()
!
! --- Get visibility data
!
      IUER = -1
      CALL GET_FITS_VIS ( FILFITS, VIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1508, -2, 'FITS_TO_RADPLOT', 'Failure in an '// &
     &         'attempt to read the fits file with visibility data '// &
     &          FILFITS )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL CLOSURE_STAT ( VIS, GAP_SCAN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1508, -2, 'FITS_TO_RADPLOT', 'Failure in an '// &
     &         'attempt to read the fits file with visibility data '// &
     &          FILFITS )
           CALL EXIT ( 1 )
      END IF
!
      END  SUBROUTINE  FITS_CHECK_CLS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLOSURE_STAT ( VIS, GAP_SCAN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CLOSURE_STAT computes statistcs of scan average phase      *
! *   closures.                                                          *
! *                                                                      *
! *  ### 06-OCT-2013   CLOSURE_STAT  v1.0 (c) L. Petrov  06-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      REAL*8     GAP_SCAN
      INTEGER*4  IUER
      INTEGER*4  M_TIM, M_SCA, M_BAS, M_TRI
      REAL*8     WEI_MIN
      PARAMETER  ( M_TIM = 256*1024 )
      PARAMETER  ( M_SCA =     8192 )
      PARAMETER  ( M_BAS =     8192 )
      PARAMETER  ( M_TRI =  64*1024 )
      PARAMETER  ( WEI_MIN = 1.D-15 )
      LOGICAL*1, ALLOCATABLE :: FLAG(:)
      REAL*4,    ALLOCATABLE :: WEI(:,:)
      REAL*4,    ALLOCATABLE :: PHS_CLS(:,:), PHS_CLS_TIM(:,:,:)
      REAL*4,    ALLOCATABLE :: WEI_CLS(:),   WEI_CLS_TIM(:,:)
      REAL*8     TAI_SCA(M_SCA)
      CHARACTER  STR*128, CT_STA(3)*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           KP, IND_SCA(2,M_SCA), LT_STA, &
     &           MJD_SCA(M_SCA), L_SCA, I_BAS, IND_BAS, &
     &           IND_STA_SUB(2), IND_STA(2), IVRB, LUN, IP, IND_SUB, &
     &           L_STA, LIS_STA(MSTA), L_BAS, LIS_BAS(M_BAS), IS, &
     &           L_TRI, LIS_TRI(3,M_TRI), ISG(3), &
     &           IND_TIM, I_TIM, L_TIM, LIS_TIM(M_TIM), IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ADD_LIS, ADD_CLIST, GET_UNIT, IFIND_PL, ILEN, I_LEN, NSTBA
!
! --- Get the number of scans and their dates.
! --- Create array of scan indexes IND_SCA and dates of the beginning of the
! --- scan
!
      ALLOCATE ( FLAG(M_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_TIM, STR )
           CALL ERR_LOG ( 1611, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           RETURN
      END IF
!
      ALLOCATE ( WEI(VIS%NFRQ,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VIS%NP*VIS%NFRQ, STR )
           CALL ERR_LOG ( 1612, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           RETURN
      END IF
!
      L_SCA = 0
      CALL NOUT_I4 ( 2*L_SCA, IND_SCA )
      IF ( VIS%NP .LE. 0 ) THEN
         ELSE IF ( VIS%NP == 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           IND_SCA(2,L_SCA) = 1
         ELSE IF ( VIS%NP > 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           MJD_SCA(L_SCA) = VIS%MJD(1)
           TAI_SCA(L_SCA) = VIS%TAI(1)
!
! -------- Check all points
!
           DO 410 J1=2,VIS%NP
              IF ( ( VIS%MJD(J1) - VIS%MJD(J1-1) )*86400.0D0 + &
     &             ( VIS%TAI(J1) - VIS%TAI(J1-1) ) > GAP_SCAN  ) THEN
!
! ---------------- The gap between points is greater than GAP_SCAN
!
                   IND_SCA(2,L_SCA) = J1-1
                   L_SCA = L_SCA + 1        ! increment the scan counter
                   IF ( L_SCA > M_SCA ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( M_SCA, STR )
                         CALL ERR_LOG ( 1611, IUER, 'CLOSURE_STAT', &
     &                       'Trap of internal control: too many scans. '// &
     &                       'Increase parameters M_SCA: '//STR )
                         DEALLOCATE ( FLAG )
                         DEALLOCATE ( WEI  )
                         RETURN
                   END IF
                   IND_SCA(1,L_SCA) = J1
                   MJD_SCA(L_SCA) = VIS%MJD(J1)
                   TAI_SCA(L_SCA) = VIS%TAI(J1)
              END IF
 410       CONTINUE
           IND_SCA(2,L_SCA) = VIS%NP
      END IF
        write ( 6, * ) 'l_sca= ', l_sca ! %%%
      DO 420 J2=1,L_SCA  ! Cycle over scans
!
! ------ Initialization of accumulators
!
         L_BAS = 0
         L_STA = 0
         L_TRI = 0
         L_TIM = 0
!
! ------ Cycle over epochs of the scan
!
         DO 430 J3=IND_SCA(1,J2),IND_SCA(2,J2)
            IND_SUB    = NINT(100.0*(VIS%IND_BAS(J3) - INT(VIS%IND_BAS(J3)))) + 1
            IND_STA_SUB(1) = VIS%IND_BAS(J3)/256
            IND_STA_SUB(2) = VIS%IND_BAS(J3) - IND_STA_SUB(1)*256
            IND_STA(1) = VIS%LIS_STA(IND_STA_SUB(1),IND_SUB)
            IND_STA(2) = VIS%LIS_STA(IND_STA_SUB(2),IND_SUB)
            I_BAS = NSTBA ( IND_STA(1), IND_STA(2) )
            IS = ADD_LIS ( MSTA,  L_STA, LIS_STA, IND_STA(1), -2 ) 
            IS = ADD_LIS ( MSTA,  L_STA, LIS_STA, IND_STA(2), -2 ) 
            IS = ADD_LIS ( M_BAS, L_BAS, LIS_BAS, I_BAS, -2      ) 
            I_TIM = 1000*(       (VIS%MJD(J3) - VIS%MJD_REF)*86400 + &
     &                      IDINT(VIS%TAI(J3) - VIS%TAI_REF)         )
            IS = ADD_LIS ( M_TIM, L_TIM, LIS_TIM, I_TIM, -2      ) 
 430     CONTINUE 
         CALL SORT_I ( L_STA, LIS_STA )
         CALL SORT_I ( L_BAS, LIS_BAS )
         write ( 6, * ) 'lis_sta= ', lis_sta(1:l_sta)  ! %%%
         write ( 6, * ) 'l_epc= ', ind_sca(2,j2) - ind_sca(1,j2) + 1, ' l_tim= ', l_tim ! %%%%%
!
         CALL ERR_PASS ( IUER, IER )
         CALL TRI_GRP  ( L_STA, LIS_STA, L_BAS, LIS_BAS, &
     &                   M_TRI, L_TRI, LIS_TRI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1613, IUER, 'CLOSURE_STAT', 'Error in '// &
     &            'routine TRI_GRP' )
              RETURN
         END IF
  l_tri = 1 ! %%%%
!
         ALLOCATE ( PHS_CLS_TIM(VIS%NFRQ,L_TIM,L_TRI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*L_TRI*VIS%NFRQ, STR )
              CALL ERR_LOG ( 1614, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &            'PHS_CLS_TIM' )
              RETURN
         END IF
!
         ALLOCATE ( WEI_CLS_TIM(L_TIM,L_TRI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*L_TRI*VIS%NFRQ, STR )
              CALL ERR_LOG ( 1614, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &            'WEI_CLS_TIM' )
              RETURN
         END IF
!
         ALLOCATE ( PHS_CLS(VIS%NFRQ,L_TRI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*L_TRI*VIS%NFRQ, STR )
              CALL ERR_LOG ( 1614, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for array PHS_CLS' )
              RETURN
         END IF
!
         ALLOCATE ( WEI_CLS(L_TRI), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*L_TRI*VIS%NFRQ, STR )
              CALL ERR_LOG ( 1614, IUER, 'CLOSURE_STAT', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for array WEI_CLS' )
              RETURN
         END IF
         PHS_CLS = 0.0
         WEI_CLS = 0.0
         PHS_CLS_TIM = 0.0
         WEI_CLS_TIM = 0.0
!
         DO 440 J4=IND_SCA(1,J2),IND_SCA(2,J2)
            IND_SUB    = NINT(100.0*(VIS%IND_BAS(J4) - INT(VIS%IND_BAS(J4)))) + 1
            IND_STA_SUB(1) = VIS%IND_BAS(J4)/256
            IND_STA_SUB(2) = VIS%IND_BAS(J4) - IND_STA_SUB(1)*256
            IND_STA(1) = VIS%LIS_STA(IND_STA_SUB(1),IND_SUB)
            IND_STA(2) = VIS%LIS_STA(IND_STA_SUB(2),IND_SUB)
            I_BAS = NSTBA ( IND_STA(1), IND_STA(2) )
            IND_BAS = IFIND_PL ( L_BAS, LIS_BAS, I_BAS )
            I_TIM = 1000*(       (VIS%MJD(J4) - VIS%MJD_REF)*86400 + &
     &                      IDINT(VIS%TAI(J4) - VIS%TAI_REF)         )
            IND_TIM = IFIND_PL ( L_TIM, LIS_TIM, I_TIM )
            DO 450 J5=1,L_TRI
               CALL ERR_PASS ( IUER, IER )
               CALL SIGN_TRI ( LIS_TRI(1,J5), L_BAS, LIS_BAS, ISG, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1615, IUER, 'CLOSURE_STAT', 'Trap of '// &
     &                  'inteternal control: a wrong triangle' )
                    RETURN
               END IF
!
               DO 460 J6=1,3
                  IF ( IND_BAS == LIS_TRI(J6,J5) ) THEN
!  if ( ind_tim ==  7 ) write ( 6, * ) 'p07 = ', int2(j6), ' sgn= ', int2(isg(j6)), ' phs= ', phas_cmpl_r4(vis%vis(1,j4)) ! %%%
!  if ( ind_tim == 22 ) write ( 6, * ) 'p22 = ', int2(j6), ' sgn= ', int2(isg(j6)), ' phs= ', phas_cmpl_r4(vis%vis(1,j4)) ! %%%
!  if ( ind_tim == 27 ) write ( 6, * ) 'p27 = ', int2(j6), ' sgn= ', int2(isg(j6)), ' phs= ', phas_cmpl_r4(vis%vis(1,j4)) ! %%%
                       DO 470 J7=1,VIS%NFRQ
                          PHS_CLS_TIM(J7,IND_TIM,J5) = PHS_CLS_TIM(J7,IND_TIM,J5) + ISG(J6)*PHAS_CMPL_R4(VIS%VIS(J7,J4))
 470                   CONTINUE 
                       WEI_CLS_TIM(IND_TIM,J5) = WEI_CLS_TIM(IND_TIM,J5) + 1.0
                  END IF
 460           CONTINUE 
 450        CONTINUE 
 440     CONTINUE 
!
!!         write ( 6, * ) 'l_sta= ', l_sta, ' l_bas= ', l_bas, ' l_tri= ', l_tri ! %%%%%%%%%%%%%%%%%%%%
         DO 480 J8=1,L_TRI
            LT_STA = 0
            DO 490 J9=1,L_TIM
               IF ( ABS(WEI_CLS_TIM(J9,J8) - 3.0)  < 0.1 ) THEN
!                    write ( 6, * ) 'j9= ', int2(j9), ' j8= ', int2(j8), ' phs_cls= ', phs_cls_tim(1,j9,j8)
               END IF
 490        CONTINUE 
            DO 4100 J10=1,3
               CALL NBAST ( LIS_BAS(LIS_TRI(J10,J8)), IND_STA(1), IND_STA(2) )
               IER = -1
               IS = ADD_CLIST ( 3, LT_STA, CT_STA, VIS%C_STA(IND_STA(1)), IER )
               IS = ADD_CLIST ( 3, LT_STA, CT_STA, VIS%C_STA(IND_STA(2)), IER )
 4100       CONTINUE 
            CALL SORT_CH ( LT_STA, CT_STA )
            DO 4110 J11=1,VIS%NFRQ
               WRITE ( 6, 210 ) CT_STA(1), CT_STA(2), CT_STA(3), J11, PHS_CLS(J11,J8)
 210           FORMAT ( 'Tri ', A, 1X, A, 1X, A, '  I_frq: ', I4, ' Phs_cls: ', F6.3 )
 4110       CONTINUE 
 480     CONTINUE 
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE CLOSURE_STAT  !#!#
