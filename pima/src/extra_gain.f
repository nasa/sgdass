      PROGRAM    EXTRA_GAIN
! ************************************************************************
! *                                                                      *
! *   Program EXTRA_GAIN
! *                                                                      *
! *  ### 02-OCT-2013   EXTRA_GAIN  v1.0 (c)  L. Petrov  03-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILUVA*128, FILUVS*128
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: UVA, UVS
      REAL*8,    ALLOCATABLE :: GAIN_SUB(:,:,:), GAIN(:,:)
      REAL*8     MRE(MSUB)
      CHARACTER  STR*128
      INTEGER*4  IND_REF, J1, J2, J3, J4, J5, KSTA(MSTA), IUER
      INTEGER*4  LTM_DIF, ILEN, I_LEN
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: extra_gain uva-file uvs-file'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILUVA )
           CALL GETARG ( 2, FILUVS )
      END IF 
!
      IUER = -1
      CALL GET_FITS_VIS ( FILUVA, UVA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5001, IUER, 'EXTRA_GAIN', 'Cannot read original '// &
     &         'file with visibilities '//FILUVA )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GET_FITS_VIS ( FILUVS, UVS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5002, IUER, 'EXTRA_GAIN', 'Cannot read processed '// &
     &         'file with visibilities '//FILUVS )
           CALL EXIT ( 1 )
      END IF
!
      IF ( UVA%NP .NE. UVS%NP ) THEN
           WRITE ( 6, * ) ' UVA%NP= ', UVA%NP, ' UVS%NP= ', UVS%NP
           IUER = -2
           CALL ERR_LOG ( 5003, IUER, 'EXTRA_GAIN', 'The number of '// &
     &         'visilibily points is different in original and processed '// &
     &         'data files' )
           CALL EXIT ( 1 )
      END IF
!!      WRITE ( 6, * ) 'np,nsub,nfrq,nsta= ', UVA%NP, UVA%NSUB, UVA%NFRQ, UVA%NSTA
!
      ALLOCATE ( GAIN_SUB(UVA%NFRQ,UVA%NSTA,UVA%NSUB), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*UVA%NSTA*UVA%NFRQ*UVA%NSUB, STR )
           IUER = -2
           CALL ERR_LOG ( 5004, IUER, 'EXTRA_GAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array GAIN_SUB' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( GAIN(UVA%NFRQ,UVA%NSTA), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*UVA%NSTA*UVA%NFRQ, STR )
           IUER = -2
           CALL ERR_LOG ( 5005, IUER, 'EXTRA_GAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array GAIN' )
           CALL EXIT ( 1 )
      END IF
      GAIN = 1.0D0
!
      WRITE ( 6, '(A)' ) '# GAIN Correction table.  Format version of 2013.10.03'
      WRITE ( 6, '(A)' ) '#  '
      WRITE ( 6, '(A)' ) '# Experiment Name: '//UVA%EXP_NAME
      WRITE ( 6, '(A)' ) '# Experiment Date: '//UVA%DATE_OBS
      WRITE ( 6, '(A)' ) '# Source Name:     '//UVA%SOU_NAME
      WRITE ( 6, '(A)' ) '#  '
! 
      KSTA = 0
      DO 410 J1=1,UVA%NSUB
         IUER = -1
         CALL TAKE_GAIN_SUB ( UVA, UVS, J1, GAIN_SUB, MRE(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 5005, IUER, 'EXTRA_GAIN', 'Failure to compute '// &
     &            'gain for subarray '//STR(1:I_LEN(STR)) )
              CALL EXIT ( 1 )
         END IF
         DO 420 J2=1,UVA%L_STA(J1)
            KSTA(J2) = KSTA(J2) + 1
            DO 430 J3=1,UVA%NFRQ
               GAIN(J3,J2) = GAIN(J3,J2)*GAIN_SUB(J3,UVA%LIS_STA(J2,J1),J1)
!               WRITE ( 6, 210 ) UVA%C_STA(UVA%LIS_STA(J2,J1)), &
!     &                          J3, 1.D-6*UVA%SKY_FRQ(J3), GAIN(J3,UVA%LIS_STA(J2,J1),J1)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      DO 440 J4=1,UVA%NSTA
         DO 450 J5=1,UVA%NFRQ
            IF ( KSTA(J4) > 0 ) THEN
                 GAIN(J5,J4) = GAIN(J5,J4)**(1.D0/KSTA(J4))
                 WRITE ( 6, 210 ) UVA%C_STA(J4), &
     &                            J5, 1.D-6*UVA%SKY_FRQ(J5), GAIN(J5,J4)
 210             FORMAT ( 'Sta: ', A, ' Ifrq: ', I3, ' Sky_frq: ', F9.2, ' Gain: ', F7.3 )
            END IF          
 450     CONTINUE 
 440  CONTINUE 
!
      END  PROGRAM  EXTRA_GAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TAKE_GAIN_SUB ( UVA, UVS, ISUB, GAIN, MRE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TAKE_GAIN_SUB 
! *                                                                      *
! * ### 02-OCT-2013  TAKE_GAIN_SUB  v1.0 (c) L. Petrov  02-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: UVA, UVS
      REAL*8     GAIN(UVA%NFRQ,UVA%NSTA,UVA%NSUB), MRE
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EQU_OBS(:), EST_VEC(:)
      REAL*8     RC, RH, WEI, SIG_CNS
      PARAMETER  ( SIG_CNS = 1.D3 )
      INTEGER*4  ISUB, IUER 
      INTEGER*4  J1, J2, J3, J4, J5, J6, LPAR, LPA2, IND_SUB, &
     &           IND_STA, IND_STA_SUB(2), IP, IER
      INTEGER*4, EXTERNAL :: IFIND_PL, ILEN, I_LEN
!
      GAIN(1:UVA%NFRQ,1:UVA%NSTA,ISUB) = 1.0D0
!
      LPAR = UVA%NFRQ*UVA%L_STA(ISUB)
      LPA2 = (LPAR*(LPAR+1))/2
      ALLOCATE ( NOR_MAT(LPA2) )
      ALLOCATE ( NOR_VEC(LPAR) )
      ALLOCATE ( EQU_OBS(LPAR) )
      ALLOCATE ( EST_VEC(LPAR) )
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
      DO 410 J1=1,UVA%NP
         IND_SUB    = NINT(100.0*(UVA%IND_BAS(J1) - INT(UVA%IND_BAS(J1)))) + 1
         IF ( IND_SUB .NE. ISUB ) GOTO 410
         IND_STA_SUB(1) = UVA%IND_BAS(J1)/256
         IND_STA_SUB(2) = UVA%IND_BAS(J1) - IND_STA_SUB(1)*256
         DO 420 J2=1,UVA%NFRQ
            EQU_OBS = 0.0D0
            EQU_OBS((J2-1)*UVA%L_STA(ISUB) + IND_STA_SUB(1)) = 1.0D0
            EQU_OBS((J2-1)*UVA%L_STA(ISUB) + IND_STA_SUB(2)) = 1.0D0
!
            IF ( ABS(UVS%VIS(J2,J1)) > PIMA__AMP_MIN .AND. &
     &           ABS(UVA%VIS(J2,J1)) > PIMA__AMP_MIN       ) THEN
                 RH  = LOG ( ABS(UVS%VIS(J2,J1))/ABS(UVA%VIS(J2,J1)) )
                 WEI = DSQRT(1.0D0*UVA%WEI(J2,J1))
                 WEI = 1.0D0
                 CALL DIAD_CVT_S ( WEI**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( LPAR, WEI, RH, EQU_OBS, NOR_VEC )
            END IF
 420     CONTINUE 
 410  CONTINUE
!
      IP = 1
      DO 430 J3=1,LPAR
         NOR_MAT(IP) = NOR_MAT(IP) + 1.D0/SIG_CNS**2
         IP = IP + J3 + 1
 430  CONTINUE 
!
! --- Invert normal matrix
!
!!   call matview_2 ( lpar, nor_mat )  ! %%%%
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
!!  write ( 6, * ) ' rc= ' ,rc ! %%%
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5111, IUER, 'TAKE_GAIN_SUB', 'Failure to '// &
     &         'invert the normal matrix during an attempt to solve '// &
     &         'for gain corrections' )
           RETURN 
      END IF
!
      IER = -1
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, EST_VEC, IER )
!
      IP = 1
      MRE = 0.0D0
      DO 440 J4=1,LPAR
         NOR_VEC(J4) = DEXP ( DSQRT(NOR_MAT(IP)) )
         IP = IP + J4 + 1
         MRE = MRE + NOR_VEC(J4) 
 440  CONTINUE 
      MRE = MRE/LPAR
!!   write ( 6, * ) 'est_vec= ', est_vec ! %%%
!
      IP = 0
      DO 450 J5=1,UVA%NFRQ
         DO 460 J6=1,UVA%L_STA(ISUB) 
            IND_STA = UVA%LIS_STA(J6,ISUB) ! station index in UVA%NSTA/UVA%C_STA
            IP = IP + 1
            GAIN(J5,IND_STA,ISUB) = DEXP ( EST_VEC(IP) )
 460     CONTINUE 
 450  CONTINUE 
!
      DEALLOCATE ( EST_VEC )
      DEALLOCATE ( EQU_OBS )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TAKE_GAIN_SUB  !#!#
