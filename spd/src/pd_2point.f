      PROGRAM    PD_2POINT
! ************************************************************************
! *                                                                      *
! *   Program PD_2POINT
! *                                                                      *
! *  ### 05-NOV-2014   PD_2POINT   v1.2 (c)  L. Petrov  2020.12.29  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'spd.i'
      TYPE     ( HEB__TYPE    ) :: HEB_3D
      TYPE     ( SPD_2P__TYPE ), POINTER :: SPD_2P(:)
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      INTEGER*4    M_FIL, M_BUF
      PARAMETER  ( M_FIL  = 128*1024 )
      PARAMETER  ( M_BUF  = 1024*1024 )
      REAL*8     EPS
      PARAMETER  ( EPS = 100.0 )
      CHARACTER  FILIN*128, RESP_DIR*128, C_FIL(M_FIL)*128, &
     &           BUF(M_BUF)*256, OUT(M_BUF)*256, DATE_FIL*21, FILNAM*128, &
     &           STR*128, TEST_STR*16, EXT*4
      CHARACTER    PD_2POINT__LABEL*32
      PARAMETER  ( PD_2POINT__LABEL = 'PD_2POINT  version of 2014.11.21' )
      REAL*4     EPS_LEV1, EPS_LEV2, EPS_LAT, EPS_LON, EPS_TIM
      PARAMETER  ( EPS_LEV1 = 0.001 )
      PARAMETER  ( EPS_LEV2 = 1.0 )
      PARAMETER  ( EPS_LAT  = 1.E-5 )
      PARAMETER  ( EPS_LON  = 3.E-5 )
      PARAMETER  ( EPS_TIM  = 1.0   )
      INTEGER*8  DIR_DESC(16), MEL
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           J12, J13, J14, J15, J16, J17, J18, &
     &           NB, LEV, L_FIL, ID, IL, IS, NMOD, N_DEL, NOUT, &
     &           IVRB, MJD_BEG, MJD_END, MJD_FIL, IFMT, ISTL, &
     &           IND_BEG, IND_END, IND_TIM, IUER
      REAL*8     TAI_BEG, TAI_END, TAI_FIL, RLEV, DIST
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, GET_FILE_FROM_DIR
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR
!
      IVRB = 2
      EXT = '.heb'
      TEST_STR = 'timer'
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'pd_2point dat_fil resp_dir'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN    )
           CALL GETARG ( 2, RESP_DIR )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILIN, M_BUF, BUF, NB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4401, IUER, 'PD_2POINT', 'Error in an attempt '// &
     &         'to read input file '//FILIN )
           CALL EXIT ( 0 )
      END IF
!
      IF ( BUF(1)(1:60) == '# Two-point data for delay computation. Format of 2014.11.13' ) THEN
           IFMT = 1
         ELSE IF ( BUF(1)(1:60) == '# Two-point data for delay computation. Format of 2015.01.24' ) THEN
           IFMT = 2
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 4402, IUER, 'PD_2POINT', 'Unsupported type of '// &
     &         'the input file '//FILIN(1:I_LEN(FILIN))//' . Expected '// &
     &         'format label has not been found in the first line' )
           CALL EXIT ( 1 )
      END IF
!
      MJD_BEG = 0
      N_DEL = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( MJD_BEG == 0 ) THEN
              READ ( UNIT=BUF(J1)(11:15), FMT='(I5)'   ) MJD_BEG
              READ ( UNIT=BUF(J1)(17:23), FMT='(F7.1)' ) TAI_BEG
         END IF 
         READ ( UNIT=BUF(J1)(11:15), FMT='(I5)'   ) MJD_END
         READ ( UNIT=BUF(J1)(17:23), FMT='(F7.1)' ) TAI_END
         N_DEL = N_DEL + 1
 410  CONTINUE 
!
      ALLOCATE ( SPD_2P(N_DEL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           MEL = N_DEL*SIZEOF(SPD_2P(1))
           CALL IINCH8 ( MEL, STR )
           IUER = -2
           CALL ERR_LOG ( 4403, IUER, 'PD_2POINT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array SPD_2P' )
           CALL EXIT ( 1 )
      END IF
!
      N_DEL = 0
      DO 420 J2=1,NB
         IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
         IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
         N_DEL = N_DEL + 1
         READ ( UNIT=BUF(J2)(11:15),   FMT='(I5)'    ) SPD_2P(N_DEL)%MJD
         READ ( UNIT=BUF(J2)(17:23),   FMT='(F7.1)'  ) SPD_2P(N_DEL)%TAI
         READ ( UNIT=BUF(J2)(26:38),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(1)
         READ ( UNIT=BUF(J2)(40:52),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(2)
         READ ( UNIT=BUF(J2)(54:66),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_EMI(3)
         READ ( UNIT=BUF(J2)(69:81),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(1)
         READ ( UNIT=BUF(J2)(83:95),   FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(2)
         READ ( UNIT=BUF(J2)(97:109),  FMT='(F13.4)' ) SPD_2P(N_DEL)%COO_REC(3)
         READ ( UNIT=BUF(J2)(112:117), FMT='(F6.4)'  ) SPD_2P(N_DEL)%DEL(1)
         READ ( UNIT=BUF(J2)(120:125), FMT='(F6.4)'  ) SPD_2P(N_DEL)%DEL(2)
         READ ( UNIT=BUF(J2)(128:138), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_RDER(1)
         READ ( UNIT=BUF(J2)(140:150), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_RDER(2)
         READ ( UNIT=BUF(J2)(152:162), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_EDER(1)
         READ ( UNIT=BUF(J2)(164:174), FMT='(F11.4)' ) SPD_2P(N_DEL)%DEL_EDER(2)
!
         DIST= DSQRT ( (SPD_2P(N_DEL)%COO_EMI(1) - SPD_2P(N_DEL)%COO_REC(1))**2 + &
     &                 (SPD_2P(N_DEL)%COO_EMI(2) - SPD_2P(N_DEL)%COO_REC(2))**2 + &
     &                 (SPD_2P(N_DEL)%COO_EMI(3) - SPD_2P(N_DEL)%COO_REC(3))**2   )
         IF ( DIST > 1.5*SPD__U_MAX ) THEN
              ISTL = SPD__SAT
            ELSE 
              ISTL = SPD__2P
         END IF
 420  CONTINUE 
!
      LEV = 0
      L_FIL = 0
      DO 430 J2=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, RESP_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4403, IUER, 'PD_2POINT', 'Error in '// &
     &            'reading input directory '//RESP_DIR(1:I_LEN(RESP_DIR))// &
     &            '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 830 ! End of work
         IF ( INDEX ( FILNAM, EXT )     < 1    ) GOTO 430
         IF ( INDEX ( FILNAM, '#' )     .GE. 1 ) GOTO 430
         IF ( INDEX ( FILNAM, '/refr/' ) < 1   ) GOTO 430
!
         IL = ILEN(FILNAM)
         IF ( IL < 22 ) GOTO 430
         DATE_FIL = FILNAM(IL-16:IL-13)//'.'//FILNAM(IL-12:IL-11)//'.'// &
     &              FILNAM(IL-10:IL-6)//':'//FILNAM(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL  ERR_LOG ( 4404, IUER, 'PD_2POINT', &
     &             'Unexpected format of file name '//FILNAM )
              CALL EXIT ( 1 )
         END IF
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > M_FIL )  THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_FIL, STR )
              IUER = -1
              CALL ERR_LOG ( 4405, IUER, 'PD_2POINT', &
     &            'Too many files in directory '// &
     &            RESP_DIR(1:I_LEN(RESP_DIR))// &
     &            ' -- more than '//STR )
              CALL EXIT ( 1 )
         END IF
         C_FIL(L_FIL) = FILNAM 
 430  CONTINUE 
 830  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4406, IUER, 'PD_2POINT', 'No files with '// &
     &         'extension '//EXT(1:I_LEN(EXT))//' were found in the '// &
     &         'input directory '//RESP_DIR )
           CALL EXIT ( 1 )
         ELSE
           CALL SORT_CH ( L_FIL, C_FIL )
      END IF
!
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4407, IUER, 'PD_2POINT', &
     &         'No appropriate data files were found in the input '// &
     &         'directory '//RESP_DIR )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) L_FIL
 110       FORMAT ( I6, ' input atmospheric files have been found' )
      END IF 
!
      IND_BEG = 0
      IND_END = 0
      DO 440 J4=1,L_FIL
         IL = ILEN(C_FIL(J4))
         DATE_FIL = C_FIL(J4)(IL-16:IL-13)//'.'//C_FIL(J4)(IL-12:IL-11)//'.'// &
     &              C_FIL(J4)(IL-10:IL-6)//':'//C_FIL(J4)(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL  ERR_LOG ( 4408, IUER, 'PD_2POINT', &
     &             'Unexpected format of file name '//FILNAM )
              CALL EXIT ( 1 )
         END IF
!
         IF ( (MJD_FIL*86400.0D0 + TAI_FIL) < (MJD_BEG*86400.0D0 + TAI_BEG) + EPS ) THEN
              IND_BEG = J4
         END IF 
         IF ( (MJD_FIL*86400.0D0 + TAI_FIL) > (MJD_END*86400.0D0 + TAI_END) - EPS ) THEN
              IF ( IND_END == 0 ) IND_END = J4
         END IF 
 440  CONTINUE 
!
      IF ( IND_BEG < 1 ) THEN
           IL = ILEN(C_FIL(1))
           DATE_FIL = C_FIL(1)(IL-16:IL-13)//'.'//C_FIL(1)(IL-12:IL-11)//'.'// &
     &                C_FIL(1)(IL-10:IL-6)//':'//C_FIL(1)(IL-5:IL-4)//':00.0'
           STR = MJDSEC_TO_DATE ( SPD_2P(1)%MJD, SPD_2P(1)%TAI, IUER )
           IUER = -2
           CALL ERR_LOG ( 4409, IUER, 'PD_2POINT', 'Begin date in the data '// &
     &          STR(1:21)//' is before the first epoch of atmospheric data '// &
     &          DATE_FIL )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IND_END < 1 ) THEN
           IL = ILEN(C_FIL(L_FIL))
           DATE_FIL = C_FIL(L_FIL)(IL-16:IL-13)//'.'// &
     &                C_FIL(L_FIL)(IL-12:IL-11)//'.'// &
     &                C_FIL(L_FIL)(IL-10:IL-6)//':'// &
     &                C_FIL(L_FIL)(IL-5:IL-4)//':00.0'
           STR = MJDSEC_TO_DATE ( SPD_2P(N_DEL)%MJD, SPD_2P(N_DEL)%TAI, IUER )
           IUER = -2
           CALL ERR_LOG ( 4410, IUER, 'PD_2POINT', 'End date in the data '// &
     &          STR(1:21)//'is after the last epoch of atmospheric data '// &
     &          DATE_FIL )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IND_BEG - 2 .GE. 1 ) THEN
           IND_BEG = IND_BEG - 2
         ELSE 
           IND_BEG = 1
      END IF
      IF ( IND_BEG + 2 .LE. L_FIL ) THEN
           IND_END = IND_END + 2
         ELSE 
           IND_END = L_FIL
      END IF    
!
      IND_TIM = 0
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      DO 450 J5=IND_BEG,IND_END
         IND_TIM = IND_TIM + 1
         IUER = -1
         CALL READ_HEB ( C_FIL(J5), HEB_3D, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4411, IUER, 'PD_2POINT', 'End date in the data '// &
     &            'is after the last epoch of atmospheric data '//DATE_FIL )
              CALL EXIT ( 1 )
         END IF
         HEB_3D%TAI = HEB_3D%UTC
!
         IF ( IND_TIM == 1 ) THEN
              IUER = -1
              CALL SPD_4D_INIT ( SPD_4D, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -2
                   CALL ERR_LOG ( 4412, IUER, 'PD_2POINT', 'Failure in an '// &
     &                 'attempt to initialize object SPD' )
                   CALL EXIT ( 1 )
              END IF
              SPD_4D%DIMS(1) = HEB_3D%DIMS(1) - SPD__MDEG
              SPD_4D%DIMS(2) = HEB_3D%DIMS(2) - SPD__MDEG
              SPD_4D%DIMS(3) = HEB_3D%DIMS(3) - SPD__MDEG
              SPD_4D%DIMS(4) = IND_END - IND_BEG + 1
              IF ( IFMT == 1 ) THEN
                   SPD_4D%DIMS(5) = 2
                 ELSE 
                   SPD_4D%DIMS(5) = HEB_3D%DIMS(4)
              END IF
!
              IF ( ASSOCIATED ( SPD_4D%LEV ) ) DEALLOCATE ( SPD_4D%LEV )
              ALLOCATE ( SPD_4D%LEV(1-SPD__MDEG:SPD_4D%DIMS(1)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD__MLEV+SPD_4D%DIMS(1)), STR )
                   IUER = -2
                   CALL ERR_LOG ( 4413, IUER, 'PD_2POINT', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LEV' )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( SPD_4D%LON ) ) DEALLOCATE ( SPD_4D%LON )
              ALLOCATE ( SPD_4D%LON(1-SPD__MDEG:SPD_4D%DIMS(2)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(2)+SPD__MDEG), STR )
                   IUER = -2
                   CALL ERR_LOG ( 4417, IUER, 'PD_2POINT', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LON' )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( SPD_4D%LAT ) ) DEALLOCATE ( SPD_4D%LAT )
              ALLOCATE ( SPD_4D%LAT(1-SPD__MDEG:SPD_4D%DIMS(3)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(3)+SPD__MDEG), STR )
                   IUER = -2
                   CALL ERR_LOG ( 4416, IUER, 'PD_2POINT', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%LAT' )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ASSOCIATED ( SPD_4D%TIM ) ) DEALLOCATE ( SPD_4D%TIM )
              ALLOCATE ( SPD_4D%TIM(1-SPD__MDEG:SPD_4D%DIMS(4)+SPD__MDEG), &
     &                   STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( 4*(SPD_4D%DIMS(4)+SPD__MDEG), STR )
                   IUER = -2
                   CALL ERR_LOG ( 4418, IUER, 'PD_2POINT', 'Failure in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array SPD_4D%TIM' )
                   CALL EXIT ( 1 )
              END IF
!
              ALLOCATE ( SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1-SPD__MDEG:SPD_4D%DIMS(4),SPD_4D%DIMS(5)), STAT=IUER )
              IF ( IUER .NE. 0 ) THEN
                   MEL = INT8(SPD_4D%DIMS(1)+SPD__MDEG)*INT8(SPD_4D%DIMS(2)+SPD__MDEG)* &
     &                   INT8(SPD_4D%DIMS(3)+SPD__MDEG)*INT8(SPD_4D%DIMS(4)+SPD__MDEG)* &
     &                   INT8(SPD_4D%DIMS(5))
                   CALL IINCH8 ( MEL, STR )
                   IUER = -2
                   CALL ERR_LOG ( 4419, IUER, 'PD_2POINT', 'Error in an attempt '// &
     &                 'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory for array REFR' )
                   CALL EXIT ( 1 )
              END IF
              SPD_4D%LEV  = 0.0
              SPD_4D%LAT  = 0.0
              SPD_4D%LON  = 0.0
              SPD_4D%TIM  = 0.0
              SPD_4D%REFR = 0.0
              SPD_4D%STATUS = SPD__ALLO
              SPD_4D%MJD_0 = HEB_3D%MJD
              SPD_4D%TAI_0 = HEB_3D%TAI
         END IF
!
         SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),IND_TIM,1:SPD_4D%DIMS(5)) = &
     &        HEB_3D%VAL(1:SPD_4D%DIMS(1)+SPD__MDEG,1:SPD_4D%DIMS(2)+SPD__MDEG,1:SPD_4D%DIMS(3)+SPD__MDEG,1:SPD_4D%DIMS(5))
         SPD_4D%TIM(IND_TIM) = (HEB_3D%MJD*86400.0D0 + HEB_3D%TAI) - &
     &                         (SPD_4D%MJD_0*86400.0D0 + SPD_4D%TAI_0)
 450  CONTINUE 
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point reading data files: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Build arrays of levels
!
      DO 460 J6=1,SPD_4D%DIMS(1)
         RLEV = J6-1 - (SPD__MLEV-1)/2
         SPD_4D%LEV(J6) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
 460  CONTINUE 
!
! --- Add arguments of extended knots for the height array
!
      DO 470 J7=0,1-SPD__MDEG,1
         SPD_4D%LEV(J7) = SPD_4D%LEV(J7+1) - EPS_LEV1
 470  CONTINUE 
      DO 480 J8=SPD_4D%DIMS(1)+1,SPD_4D%DIMS(1)+SPD__MDEG
         SPD_4D%LEV(J8) = SPD_4D%LEV(J8-1) + EPS_LEV2
 480  CONTINUE 
!
! --- Build arrays of longitude
!
      DO 490 J9=1,SPD_4D%DIMS(2)
!
! ------ NB: the longitudinal grid has two more nodes: at 360deg and 360deg + 1step
!
         SPD_4D%LON(J9) = (J9-1)*PI2/(SPD_4D%DIMS(2) - 2)
 490  CONTINUE 
!
! --- Add arguments of extended knots for the longitude array
!
      DO 4100 J10=0,1-SPD__MDEG,-1
         SPD_4D%LON(J10) = SPD_4D%LON(J10+1) - EPS_LON
 4100 CONTINUE 
      DO 4110 J11=SPD_4D%DIMS(2)+1,SPD_4D%DIMS(2)+SPD__MDEG
         SPD_4D%LON(J11) = SPD_4D%LON(J11-1) + EPS_LON
 4110 CONTINUE 
!
! --- Biuld the latitude array
!
      DO 4120 J12=1,SPD_4D%DIMS(3)
         SPD_4D%LAT(J12) = -P2I + (J12-1)*PI__NUM/(SPD_4D%DIMS(3)-1) 
 4120 CONTINUE 
!
! --- Add arguments of extended knots for the lattidude array
!
      DO 4130 J13=0,1-SPD__MDEG,-1
         SPD_4D%LAT(J13) = SPD_4D%LAT(J13+1) - EPS_LAT
 4130 CONTINUE 
      DO 4140 J14=SPD_4D%DIMS(3)+1,SPD_4D%DIMS(3)+SPD__MDEG
         SPD_4D%LAT(J14) = SPD_4D%LAT(J14-1)  + EPS_LAT
 4140 CONTINUE 
!
! --- Add arguments of extended knots for the time array
!
      DO 4150 J15=0,1-SPD__MDEG,-1
         SPD_4D%TIM(J15) = SPD_4D%TIM(J15+1) - EPS_TIM
 4150 CONTINUE 
      DO 4160 J16=SPD_4D%DIMS(4)+1,SPD_4D%DIMS(4)+SPD__MDEG
         SPD_4D%TIM(J16) = SPD_4D%TIM(J16-1) + EPS_TIM
 4160 CONTINUE 
!
      DO 4170 J17=1,SPD_4D%DIMS(5)
         IUER = -1
         CALL BSPL4_4D_CMP ( SPD__MDEG, 0, SPD_4D%DIMS,    &
     &                       SPD_4D%LEV(1:SPD_4D%DIMS(1)), &
     &                       SPD_4D%LON(1:SPD_4D%DIMS(2)), &
     &                       SPD_4D%LAT(1:SPD_4D%DIMS(3)), &
     &                       SPD_4D%TIM(1:SPD_4D%DIMS(4)), &
     &                       SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1-SPD__MDEG:SPD_4D%DIMS(4),J17), &
     &                       IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 4420, IUER, 'PD_2POINT', 'Failure in an '// &
     &            'attempt to expand refractivity field into the '// &
     &            '4D B-spline basis' )
              CALL EXIT ( 1 )
         END IF
 4170 CONTINUE 
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point B-spline expansion: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      NMOD = SPD_4D%DIMS(5) 
      IUER = -1
      CALL COMP_PD_2POINT ( ISTL, NMOD, N_DEL, SPD_4D, SPD_2P, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4421, IUER, 'PD_2POINT', 'Failure in '// &
     &         'an attempt to compute path delays' )
           RETURN 
      END IF
      IF ( TEST_STR == 'timer'  ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'pd_2point path delay computation WALL time: '//STR(1:27)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      N_DEL = 0
      NOUT  = 0
      DO 4180 J18=1,NB
         IF ( J18 == 2 ) THEN
              NOUT = NOUT + 1
              OUT(NOUT) = '#'
              NOUT = NOUT + 1
              OUT(NOUT) = '# Processed by '//PD_2POINT__LABEL//' on '//GET_CDATE()
              NOUT = NOUT + 1
              OUT(NOUT) = '# Used refractivity data from '//RESP_DIR(1:I_LEN(RESP_DIR))
              NOUT = NOUT + 1
              OUT(NOUT) = '#'
              GOTO 4180
         END IF
!
         IF ( BUF(J18)(1:14) == '# Processed by'      ) THEN
              NOUT = NOUT - 1
              GOTO 4180
         END IF
         IF ( BUF(J18)(1:19) == '# Used refractivity' ) GOTO 4180
         IF ( J18 > 1 ) THEN
              IF ( BUF(J18-1)(1:19) == '# Used refractivity' ) GOTO 4180
         END IF
!
         NOUT = NOUT + 1
         OUT(NOUT) = BUF(J18)
         IF ( OUT(NOUT)(1:1)  == '#' ) GOTO 4180
         IF ( ILEN(OUT(NOUT)) ==  0  ) GOTO 4180
         N_DEL = N_DEL + 1
         WRITE ( UNIT=OUT(NOUT)(112:117), FMT='(F6.4)'    ) SPD_2P(N_DEL)%DEL(1)
         WRITE ( UNIT=OUT(NOUT)(120:125), FMT='(F6.4)'    ) SPD_2P(N_DEL)%DEL(2)
         WRITE ( UNIT=OUT(NOUT)(128:138), FMT='(1PD11.4)' ) SPD_2P(N_DEL)%DEL_RDER(1)
         WRITE ( UNIT=OUT(NOUT)(140:150), FMT='(1PD11.4)' ) SPD_2P(N_DEL)%DEL_RDER(2)
 4180 CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NOUT, OUT, FILIN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4422, IUER, 'PD_2POINT', 'Failure in '// &
     &         'writing into the output file '//FILIN )
           RETURN 
      END IF
!
      END  PROGRAM  PD_2POINT  !#!  
