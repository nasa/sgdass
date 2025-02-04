      SUBROUTINE PIMA_GACO_COMP ( PIM, SOU_STR, DIR_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GACO_COMP
! *                                                                      *
! * ### 23-FEB-2016  PIMA_GACO_COMP  v1.1 (c) L. Petrov 30-JUL-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'sou_map.i'
      CHARACTER  DIR_NAME*(*), SOU_STR*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      TYPE     ( VIS__TYPE ) :: UVA_VIS, UVS_VIS
      INTEGER*4  MIND, MBUF
      PARAMETER  ( MIND = 128 )
      PARAMETER  ( MBUF = 2*PIM__MSOU )
      INTEGER*4  IND_SOU(PIM__MSOU)
      CHARACTER  STR*128, BUF(MBUF)*80, FIL_UVA*128, FIL_UVS*128, &
     &           C_SOU(PIM__MSOU)*10
      INTEGER*8  SIZE_UVA, SIZE_UVS
      LOGICAL*1  LEX
      REAL*8     GAP_SCAN, SIG__MIN, SIG__MAX, SIG__CNS, EPS_TIM
      PARAMETER  ( GAP_SCAN = 300.0D0 )
      PARAMETER  ( SIG__MIN = 1.0D-8  )
      PARAMETER  ( SIG__MAX = 8.0     )
      PARAMETER  ( SIG__CNS = 100.0D0 )
      PARAMETER  ( EPS_TIM  = 1.0D0   )
      REAL*8     EQU_VEC(2), RH, SIG, RC, APR_GACO(2), UVA_FRQ(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EST_VEC(:)
      INTEGER*8  FRQ_I8(PIM__MFRQ)
      INTEGER*4, ALLOCATABLE :: IND_UVA_XREF(:)
      INTEGER*4  LIND, IND(2,MIND), J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           J11, J12, J13, J14, J15, ID, IP, IS, IND_FRQ, L_PAR, IND_PAR(2), &
     &           NSOU, NBUF, UNIX_DATE, L_SCA, IND_SCA(2,PIM__MSCA), &
     &           IND_SUB, IND_STA_SUB(2), IND_STA(2), ISTA(2), &
     &           ISTA_VIS(PIM__MSTA), L_VIS, &
     &           IND_UVA_SUB, IND_UVA_STA(2), IND_UVS_SUB, IND_UVS_STA(2), &
     &           KVIS, LFRQ, NUVA_FRQ, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LTM_DIF
      INTEGER*8, EXTERNAL :: IFIND_PL8
!
      INQUIRE ( FILE=PIM%CONF%SPLT_GAIN_CORR_FILE, EXIST=LEX )
      IF ( LEX ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_READ_GACO ( PIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5811, IUER, 'PIMA_GACO_COMP', 'Failure '// &
     &              'in attempt to read gain correction from input file '// &
     &               PIM%CONF%SPLT_GAIN_CORR_FILE )
                RETURN 
           END IF 
         ELSE
           CALL ERR_PASS ( IUER, IER  )
           CALL PIMA_GACO_INIT ( PIM, 1.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5812, IUER, 'PIMA_GACO_COMP', 'Failure '// &
     &              'in attempt to inilize gain correction' )
                RETURN 
           END IF 
     END IF 
!
      IND_SOU = 0
      CALL EXWORD ( SOU_STR, MIND, NSOU, IND, ',', IER )
      DO 410 J1=1,NSOU
         DO 420 J2=1,PIM%NSOU
            IF ( IND_SOU(J1) == 0 ) THEN
                 IF ( SOU_STR(IND(1,J1):IND(2,J1)) == TRIM(PIM%SOU(J2)%DB_NAME)    ) THEN
                      IND_SOU(J1) = J2
                   ELSE IF ( SOU_STR(IND(1,J1):IND(2,J1)) == TRIM(PIM%SOU(J2)%J2000_NAME) ) THEN
                      IND_SOU(J1) = J2
                   ELSE IF ( SOU_STR(IND(1,J1):IND(2,J1)) == TRIM(PIM%SOU(J2)%IVS_NAME) ) THEN
                      IND_SOU(J1) = J2
                   ELSE IF ( SOU_STR(IND(1,J1):IND(2,J1)) == TRIM(PIM%SOU(J2)%B1950_NAME) ) THEN
                      IND_SOU(J1) = J2
                 END IF
            END IF
 420     CONTINUE 
         IF ( IND_SOU(J1) == 0 .AND. NSOU > 1 ) THEN
              CALL ERR_LOG ( 5813, IUER, 'PIMA_GACO_COMP', 'Unknown '// &
     &            'source '//SOU_STR(IND(1,J1):IND(2,J1))// &
     &            ' specified in the second qualifier' )
              RETURN 
         END IF
         IF ( IND_SOU(J1) > 0 ) C_SOU(J1) = PIM%SOU((IND_SOU(J1)))%J2000_NAME
 410  CONTINUE 
!
      IF ( NSOU == 1 .AND. IND_SOU(1) == 0 ) THEN
           INQUIRE ( FILE=SOU_STR, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5814, IUER, 'PIMA_GACO_COMP', 'Unknown source '// &
     &               SOU_STR(1:I_LEN(SOU_STR))//' specified in the first qualifier' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( SOU_STR, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5815, IUER, 'PIMA_GACO_COMP', 'Error in reading '// &
     &              'source file '//SOU_STR(1:I_LEN(SOU_STR))// &
     &              ' specified in the first qualifier' )
                RETURN 
           END IF
!
           NSOU = 0
           DO 430 J3=1,NBUF
              IF ( BUF(J3)(1:1) == '#' ) GOTO 430
              NSOU = NSOU + 1
              DO 440 J4=1,PIM%NSOU
                 IF ( BUF(J3) == PIM%SOU(J4)%IVS_NAME   .OR. &
     &                BUF(J3) == PIM%SOU(J4)%J2000_NAME .OR. &
     &                BUF(J3) == PIM%SOU(J4)%B1950_NAME .OR. &
     &                BUF(J3) == PIM%SOU(J4)%DB_NAME         ) THEN
                      IND_SOU(NSOU) = J4
                 END IF
 440          CONTINUE 
              IF ( IND_SOU(NSOU) == 0 ) THEN
                   CALL ERR_LOG ( 5816, IUER, 'PIMA_GACO_COMP', 'Unknown '// &
     &                 'source '//BUF(J3)(1:I_LEN(BUF(J3)))//' found in '// &
     &                 'the input source file '//SOU_STR )
                   RETURN 
              END IF
              C_SOU(NSOU) = PIM%SOU((IND_SOU(NSOU)))%J2000_NAME
 430       CONTINUE 
      END IF
!
      FRQ_I8(1:PIM%NFRQ) = PIM%FRQ(1:PIM%NFRQ,PIM%CONF%FRQ_GRP)%FREQ_I8
      LFRQ = 0
      DO 450 J5=1,NSOU
         FIL_UVA = DIR_NAME(1:I_LEN(DIR_NAME))//'/'// &
     &             PIM%SOU(IND_SOU(J5))%J2000_NAME//'_'// &
     &             PIM%CONF%BAND//'_uva.fits'
!
         FIL_UVS = DIR_NAME(1:I_LEN(DIR_NAME))//'/'// &
     &             PIM%SOU(IND_SOU(J5))%J2000_NAME//'_'// &
     &             PIM%CONF%BAND//'_uvs.fits'
         IS = FILE_INFO ( FIL_UVS(1:I_LEN(FIL_UVS))//CHAR(0), UNIX_DATE, &
                          SIZE_UVS )
!
         CALL ERR_PASS ( IUER, IER )
         CALL GET_FITS_VIS ( FIL_UVA, UVA_VIS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5820, IUER, 'PIMA_GACO_COMP', 'Error in reading '// &
     &            'initial visibility file '//TRIM(FIL_UVA)//' for source '// &
     &             PIM%SOU(IND_SOU(J5))%IVS_NAME )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GET_FITS_VIS ( FIL_UVS, UVS_VIS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5821, IUER, 'PIMA_GACO_COMP', 'Error in reading '// &
     &            'self-calibrated visibility file '//FIL_UVS )
              RETURN 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 206 ) PIM%SOU(IND_SOU(J5))%J2000_NAME, &
     &                         TRIM(FIL_UVA), TRIM(FIL_UVS)
 206          FORMAT ( 'Processing source ', A, ' Uva: ', A, ' Uvs: ', A )
         END IF
!
         IF ( J5 == 1 ) THEN
!
! ------------- Determine the number of parameters
!
!@              L_PAR = PIM%NSTA*(PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1)
              NUVA_FRQ = UVA_VIS%NFRQ
              L_PAR = PIM%NSTA*NUVA_FRQ
!
! ----------- Allocate memory for NOR_MAT, NOR_VEC, and EST_VEC
!
              ALLOCATE ( NOR_MAT((L_PAR*(L_PAR+1))/2), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5817, IUER, 'PIMA_GACO_COMP', 'Error in '// &
     &                 'an attempt to allocate memory for normal matrix' )
                   RETURN 
              END IF
!
              ALLOCATE ( NOR_VEC(L_PAR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5818, IUER, 'PIMA_GACO_COMP', 'Error in '// &
     &                 'an attempt to allocate memory for normal vector' )
                   RETURN 
              END IF
!
              ALLOCATE ( EST_VEC(L_PAR), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5819, IUER, 'PIMA_GACO_COMP', 'Error in '// &
     &                 'an attempt to allocate memory for estimate vector' )
                   RETURN 
              END IF
              NOR_MAT = 0.0D0
              NOR_VEC = 0.0D0
              EST_VEC = 0.0D0
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                   WRITE ( 6, 210 ) PIM%NSTA, (PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1), L_PAR
 210               FORMAT ( 'PIMA_GACO_COMP NSTA: ', I2, ' NFRQ= ', I2, ' L_PAR = ', I4 )
              END IF
         END IF
!
         IF ( ALLOCATED ( IND_UVA_XREF ) ) DEALLOCATE ( IND_UVA_XREF )
         ALLOCATE ( IND_UVA_XREF(UVA_VIS%NP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5822, IUER, 'PIMA_GACO_COMP', 'Error in an attempt '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &            'for array IND_UVA_XREF' )
              RETURN 
         END IF
         IF ( LFRQ == 0 ) THEN
              LFRQ = UVA_VIS%NFRQ
            ELSE IF ( UVA_VIS%NFRQ .NE. LFRQ ) THEN
              CALL ERR_LOG ( 5823, IUER, 'PIMA_GACO_COMP', 'Error: UV-file for source '// &
     &             PIM%SOU(IND_SOU(J5))%J2000_NAME//' has different number of '// &
     &             'frequencies that for other source. Such a case is not supported' )
              RETURN 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, 220 ) LFRQ
 220          FORMAT ( 'PIMA_GACO_COMP: UVA_VIS%NFRQ= ', I3 ) 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                 WRITE ( 6, * ) 'PIMA_GACO_COMP: UVA station list ', UVA_VIS%C_STA(1:UVA_VIS%NSTA)//' '
                 WRITE ( 6, * ) 'PIMA_GACO_COMP: UVS station list ', UVS_VIS%C_STA(1:UVS_VIS%NSTA)//' '
                 WRITE ( 6, * ) 'PIMA_GACO_COMP: UVA time range ', UVA_VIS%MJD(1), UVA_VIS%TAI(1), ' to ', UVA_VIS%MJD(UVA_VIS%NP), UVA_VIS%TAI(UVA_VIS%NP)
                 WRITE ( 6, * ) 'PIMA_GACO_COMP: UVS time range ', UVS_VIS%MJD(1), UVS_VIS%TAI(1), ' to ', UVS_VIS%MJD(UVS_VIS%NP), UVS_VIS%TAI(UVS_VIS%NP)
         END IF
!
! ------ Generate cross-reference table
!
         KVIS = 0
         DO 460 J6=1,UVA_VIS%NP
            IND_UVA_XREF(J6) = 0
            IND_UVA_SUB    = NINT(100.0*(UVA_VIS%IND_BAS(J6) - INT(UVA_VIS%IND_BAS(J6)))) + 1
            IND_STA_SUB(1) = UVA_VIS%IND_BAS(J6)/256
            IND_STA_SUB(2) = UVA_VIS%IND_BAS(J6) - IND_STA_SUB(1)*256
            IND_UVA_STA(1) = UVA_VIS%LIS_STA(IND_STA_SUB(1),IND_UVA_SUB)
            IND_UVA_STA(2) = UVA_VIS%LIS_STA(IND_STA_SUB(2),IND_UVA_SUB)
            DO 470 J7=1,UVS_VIS%NP
               IND_UVS_SUB    = NINT(100.0*(UVS_VIS%IND_BAS(J7) - INT(UVS_VIS%IND_BAS(J7)))) + 1
               IND_STA_SUB(1) = UVS_VIS%IND_BAS(J7)/256
               IND_STA_SUB(2) = UVS_VIS%IND_BAS(J7) - IND_STA_SUB(1)*256
               IND_UVS_STA(1) = UVS_VIS%LIS_STA(IND_STA_SUB(1),IND_UVS_SUB)
               IND_UVS_STA(2) = UVS_VIS%LIS_STA(IND_STA_SUB(2),IND_UVS_SUB)
               IF ( UVS_VIS%C_STA(IND_UVS_STA(1)) .NE. UVA_VIS%C_STA(IND_UVA_STA(1)) ) GOTO 470
               IF ( UVS_VIS%C_STA(IND_UVS_STA(2)) .NE. UVA_VIS%C_STA(IND_UVA_STA(2)) ) GOTO 470
               IF ( DABS( UVS_VIS%MJD(J7)*86400.0D0 + UVS_VIS%TAI(J7) - &
     &                    UVA_VIS%MJD(J6)*86400.0D0 - UVA_VIS%TAI(J6)   ) >  EPS_TIM ) GOTO 470
               IND_UVA_XREF(J6) = J7
 470        CONTINUE 
            IF ( IND_UVA_XREF(J6) > 0 ) KVIS = KVIS + 1
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 WRITE ( 6, * ) 'PIMA_GACO_COMP: J6 = ', INT2(J6), ' KVIS= ', INT2(KVIS), &
     &                          ' Bas: ', UVS_VIS%C_STA(IND_UVA_STA(1)),  UVS_VIS%C_STA(IND_UVA_STA(2)),  &
     &                          ' IND_UVA_XREF(J6)= ', IND_UVA_XREF(J6) 
                 write ( 6, * ) 'IND_UVA_SUB= ', INT2(IND_UVA_SUB), '  IND_STA_SUB= ', INT2(IND_STA_SUB), ' IND_UVA_STA= ', INT2(IND_UVA_STA)
            END IF
 460     CONTINUE 
!
         IF ( UVA_VIS%NFRQ .NE. UVS_VIS%NFRQ ) THEN
              CALL ERR_LOG ( 5824, IUER, 'PIMA_GACO_COMP', 'Files with initial '// &
     &            'and self-calibrated visibilites '//FIL_UVA(1:I_LEN(FIL_UVA))// &
     &            ' and '//FIL_UVS(1:I_LEN(FIL_UVS))//' have different number '// &
     &            'of frequencies. Such a case is not supported' )
              RETURN 
         END IF
!
         IF ( UVA_VIS%NSTA .NE. UVS_VIS%NSTA ) THEN
              CALL ERR_LOG ( 5825, IUER, 'PIMA_GACO_COMP', 'Files with initial '// &
     &            'and self-calibrated visibilites '//FIL_UVA(1:I_LEN(FIL_UVA))// &
     &            ' and '//FIL_UVS(1:I_LEN(FIL_UVS))//' have different number '// &
     &            'of stations. Such a case is not supported' )
              RETURN 
         END IF
!
         IF ( UVA_VIS%NSUB .NE. UVS_VIS%NSUB ) THEN
              WRITE ( 6, * ) 'UVA_VIS%NSUB= ', UVA_VIS%NSUB 
              WRITE ( 6, * ) 'UVS_VIS%NSUB= ', UVS_VIS%NSUB 
              CALL ERR_LOG ( 5826, IUER, 'PIMA_GACO_COMP', 'Files with initial '// &
     &            'and self-calibrated visibilites '//FIL_UVA(1:I_LEN(FIL_UVA))// &
     &            ' and '//FIL_UVS(1:I_LEN(FIL_UVS))//' have different number '// &
     &            'of subarrays. Such a case is not supported' )
              RETURN 
         END IF
!
         IF ( UVA_VIS%NP < 1 ) THEN
              GOTO 450
           ELSE IF ( UVA_VIS%NP == 1 ) THEN
              L_SCA = 1
           ELSE IF ( UVA_VIS%NP  > 1 ) THEN
              L_SCA = 1
              IND_SCA(1,L_SCA) = 1
!
! ----------- Check all points
!
              DO 480 J8=2,UVA_VIS%NP
                 IF ( ( UVA_VIS%MJD(J8) - UVA_VIS%MJD(J8-1) )*86400.0D0 + &
     &                ( UVA_VIS%TAI(J8) - UVA_VIS%TAI(J8-1) ) > GAP_SCAN  ) THEN
!
! ------------------- The gap between points is greater than GAP_SCAN
!
                      IND_SCA(2,L_SCA) = J8-1
                      L_SCA = L_SCA + 1        ! increment the scan counter
                      IF ( L_SCA > PIM__MSCA ) THEN
                            CALL CLRCH ( STR )
                            CALL INCH  ( PIM__MSCA, STR )
                            CALL ERR_LOG ( 5827, IUER, 'PIMA_GACO_COMP', &
     &                          'Trap of internal control: too many scans. '// &
     &                          'Increase parameters M_SCA: '//STR )
                            RETURN
                      END IF
                      IND_SCA(1,L_SCA) = J8
                 END IF
 480          CONTINUE
              IND_SCA(2,L_SCA) = UVA_VIS%NP
              DO 490 J9=1,L_SCA  ! Cycle over scans
!
! -------------- Cycle over epochs of the scan
!
                 DO 4100 J10=IND_SCA(1,J9),IND_SCA(2,J9)
                    IF ( IND_UVA_XREF(J10) == 0 ) GOTO 4100
                    IND_SUB    = NINT(100.0*(UVA_VIS%IND_BAS(J10) - INT(UVA_VIS%IND_BAS(J10)))) + 1
                    IND_STA_SUB(1) = UVA_VIS%IND_BAS(J10)/256
                    IND_STA_SUB(2) = UVA_VIS%IND_BAS(J10) - IND_STA_SUB(1)*256
                    IND_STA(1) = UVA_VIS%LIS_STA(IND_STA_SUB(1),IND_SUB)
                    IND_STA(2) = UVA_VIS%LIS_STA(IND_STA_SUB(2),IND_SUB)
                    DO 4110 J11=1,PIM%NSTA
                       IF ( UVA_VIS%C_STA(IND_STA(1)) == PIM%STA(J11)%NAME      ) ISTA(1) = J11
                       IF ( UVA_VIS%C_STA(IND_STA(1)) == PIM%STA(J11)%IVS_NAME  ) ISTA(1) = J11
                       IF ( UVA_VIS%C_STA(IND_STA(1)) == PIM%STA(J11)%ORIG_NAME ) ISTA(1) = J11
!
                       IF ( UVA_VIS%C_STA(IND_STA(2)) == PIM%STA(J11)%NAME      ) ISTA(2) = J11
                       IF ( UVA_VIS%C_STA(IND_STA(2)) == PIM%STA(J11)%IVS_NAME  ) ISTA(2) = J11
                       IF ( UVA_VIS%C_STA(IND_STA(2)) == PIM%STA(J11)%ORIG_NAME ) ISTA(2) = J11
!
                       IP = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J11)%NAME ) 
                       IF ( IP > 0 ) ISTA_VIS(J11) = IP
                       IP = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J11)%IVS_NAME ) 
                       IF ( IP > 0 ) ISTA_VIS(J11) = IP
                       IP = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J11)%ORIG_NAME ) 
                       IF ( IP > 0 ) ISTA_VIS(J11) = IP
 4110               CONTINUE 
                    IF ( ISTA(1) < 1 ) THEN
                         CALL ERR_LOG ( 5828, IUER, 'PIMA_GACO_COMP', 'Trap of '// &
     &                       'internal control: station '//UVA_VIS%C_STA(IND_STA(1))// &
     &                       ' did not observe in experiment '//PIM%CONF%SESS_CODE )
                         RETURN
                    END IF
                    IF ( ISTA(2) < 1 ) THEN
                         CALL ERR_LOG ( 5829, IUER, 'PIMA_GACO_COMP', 'Trap of '// &
     &                       'internal control: station '//UVA_VIS%C_STA(IND_STA(1))// &
     &                       ' did not observe in experiment '//PIM%CONF%SESS_CODE )
                         RETURN
                    END IF
!
                    DO 4120 J12=1,UVA_VIS%NFRQ
                       IND_FRQ = IFIND_PL8 ( INT8(PIM%NFRQ), FRQ_I8, &
     &                                       NINT ( UVA_VIS%SKY_FRQ(J12), KIND=8 ) )
                       IF ( IND_FRQ < 1 ) THEN
!@                            CALL CLRCH ( STR )
!@                            WRITE ( UNIT=STR(1:15), FMT='(F15.1)' ) UVA_VIS%SKY_FRQ(J12)
!@                            CALL ERR_PASS ( IUER, IER )
!@                            CALL ERR_LOG ( 5830, IER, 'PIMA_GACO_COMP', 'Trap of '// &
!@     &                         'internal control: frequency '//STR(1:I_LEN(STR))// &
!@     &                         ' was not used in experiment '// &
!@     &                          PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//' '//&
!@     &                         'band '//PIM%CONF%BAND//', nevertheless, continue' )
                       END IF
                       IND_PAR(1) = (ISTA(1) - 1)*UVA_VIS%NFRQ + J12
                       IND_PAR(2) = (ISTA(2) - 1)*UVA_VIS%NFRQ + J12
                       EQU_VEC(1) = 1.0D0
                       EQU_VEC(2) = 1.0D0
                       IF ( ABS(UVA_VIS%VIS(J12,J10)) > PIMA__AMP_MIN .AND. &
     &                      ABS(UVA_VIS%VIS(J12,J10)) < PIMA__AMP_MAX       ) THEN
                            IF ( UVA_VIS%STATUS_GACO == SMP__ALLC ) THEN
!
! ------------------------------ Take into account gain correction that has already
! ------------------------------ been applied
!
                                 IF ( ISTA_VIS(ISTA(1)) > 0 ) THEN
                                      APR_GACO(1) = UVA_VIS%GACO(ISTA_VIS(ISTA(1)))%GAIN_CORR(J12)
                                    ELSE 
                                      APR_GACO(1) = 1.0D0
                                 END IF
                                 IF ( ISTA_VIS(ISTA(2)) > 0 ) THEN
                                      APR_GACO(2) = UVA_VIS%GACO(ISTA_VIS(ISTA(2)))%GAIN_CORR(J12)
                                    ELSE 
                                      APR_GACO(2) = 1.0D0
                                 END IF
                                 RH = LOG(APR_GACO(1)*APR_GACO(2)* &
     &                                    ABS(UVS_VIS%VIS(J12,IND_UVA_XREF(J10)))/ &
     &                                    ABS(UVA_VIS%VIS(J12,J10)) &
     &                                   )
                               ELSE
                                 RH = LOG(ABS(UVS_VIS%VIS(J12,IND_UVA_XREF(J10)))/ABS(UVA_VIS%VIS(J12,J10)))
                            END IF
                          ELSE 
                            RH = 0.0D0
                       END IF
!
                       IF ( UVA_VIS%WEI(J12,J10)               > SIG__MIN .AND. &
     &                      UVS_VIS%WEI(J12,IND_UVA_XREF(J10)) > SIG__MIN       ) THEN
                            SIG = 1.0/SQRT(UVA_VIS%WEI(J12,J10))
                          ELSE
                            SIG = 1.0D12
                       END IF
                       IF ( ABS(UVA_VIS%VIS(J12,J10)) > PIMA__AMP_MIN     .AND. &
     &                      ABS(UVA_VIS%VIS(J12,J10)) < PIMA__AMP_MAX     .AND. &
     &                      UVA_VIS%WEI(J12,J10) > SIG__MIN               .AND. &
     &                      UVS_VIS%WEI(J12,IND_UVA_XREF(J10)) > SIG__MIN       ) THEN
                            PIM%GACO(ISTA(1))%NVIS(J12) = PIM%GACO(ISTA(1))%NVIS(J12) + 1
                            PIM%GACO(ISTA(2))%NVIS(J12) = PIM%GACO(ISTA(2))%NVIS(J12) + 1
                       END IF
                       CALL ADD_TRG ( RH, SIG, &
     &                                2,  IND_PAR,     EQU_VEC, &
     &                                L_PAR,  NOR_VEC, NOR_MAT )
 4120               CONTINUE 
 4100            CONTINUE 
 490          CONTINUE
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, 230 ) PIM%SOU(IND_SOU(J5))%J2000_NAME, UVA_VIS%NP, UVS_VIS%NP, &
     &                         KVIS, UVA_VIS%NSTA
 230          FORMAT ( 'PIMA_GACO_COMP Source: ',A, ' Nvis(UVA): ', I6, ' Nvis(IVS): ', I6, &
     &                 ' Nvis(xrf): ', I6, ' UVA_VIS%NSTA= ', I2 )
         END IF
 450  CONTINUE 
!
!!    call matview_2 ( l_par, nor_mat )
      ID = 0
      DO 4130 J13=1,L_PAR
         ID = ID + J13
         NOR_MAT(ID) = NOR_MAT(ID) + 1.0D0/SIG__CNS**2
 4130 CONTINUE 
!
! --- Solve normal system using least squares
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5831, IUER, 'PIMA_GACO_COMP', 'Failure in '// &
     &         'inverting normal matrix' )
           RETURN
      END IF
      CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, EST_VEC, IER )
      IP = 0
      ID = 0
      DO 4140 J14=1,PIM%NSTA
         PIM%GACO(J14)%NFRQ = NUVA_FRQ
         DO 4150 J15=1,PIM%GACO(J14)%NFRQ
!
! --------- Here we again assume that all sources have the same frequencies
!
            PIM%GACO(J14)%GACO_FRQ(J15) = UVA_VIS%SKY_FRQ(J15)
            IP = IP + 1
            ID = ID + IP
            IF ( PIM%GACO(J14)%GAIN_CORR(J15) == 0.0 ) GOTO 4150
            PIM%GACO(J14)%GAIN_CORR(J15) = EST_VEC(IP)
            PIM%GACO(J14)%GACO_ERR(J15)  = DSQRT(NOR_MAT(ID))
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 IS = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J14)%NAME ) 
                 IF ( IS < 1 ) THEN
                      IS = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J14)%IVS_NAME ) 
                      IF ( IS < 1 ) THEN
                           IS = LTM_DIF ( 1, UVA_VIS%NSTA, UVA_VIS%C_STA, PIM%STA(J14)%ORIG_NAME ) 
                      END IF
                 END IF
                 IF ( IS > 0 ) THEN
                      WRITE ( 6, * ) 'PIMA_GACO_COMP Sta: ', UVS_VIS%C_STA(IS), &
     &                               ' IFRQ = ', INT2(J15), ' IP = ',INT2(IP), &
     &                               ' GE_LOG = ', SNGL(PIM%GACO(J14)%GAIN_CORR(J15))
                END IF 
            END IF 
            IF ( PIM%GACO(J14)%GACO_ERR(J15)  < SIG__MAX ) THEN
                 PIM%GACO(J14)%GAIN_CORR(J15) = EXP ( PIM%GACO(J14)%GAIN_CORR(J15) )
               ELSE
                 PIM%GACO(J14)%GAIN_CORR(J15) =  1.0D0
                 PIM%GACO(J14)%GACO_ERR(J15)  = -1.0D0
            END IF
 4150    CONTINUE 
 4140 CONTINUE 
      PIM%GACO_STATUS = PIMA__LOADED
!
! --- Sort the source names that were used for gain computation
!
      CALL SORT_CH  ( NSOU, C_SOU )
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_WRITE_GACO ( PIM, NSOU, C_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5832, IUER, 'PIMA_GACO_COMP', 'Failure in attempt '// &
     &         'to write gain corrections into the output file' )
           RETURN
      END IF
!
      DEALLOCATE ( NOR_VEC ) 
      DEALLOCATE ( NOR_MAT ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_GACO_COMP  !#!#
