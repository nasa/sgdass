      SUBROUTINE VTD_LOAD_SPD ( L_STA, C_STA, VTD, MJD_BEG, &
     &                          TAI_BEG, MJD_END, TAI_END, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_SPD 
! *                                                                      *
! *  ### 21-FEB-2009  VTD_LOAD_SPD   v1.6 (c) L. Petrov 25-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  L_STA, MJD_BEG, MJD_END, IUER
      CHARACTER  C_STA(L_STA)*(*)
      REAL*8     TAI_BEG, TAI_END
      CHARACTER  FIL_SPD(VTD__M_STA,VTD__M_EPD)*128, FILNAM*128, STA_FIL*8
      CHARACTER  OUT*1024, STR*32, STR1*32, STA_NAM_LOWER*8, LOCK_FILE*128
      ADDRESS__TYPE :: DIR_DESC(16)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, LEV, &
     &           M_FIL, L_FIL, IS, ID, IL, ILO, NM_STA, IND_BEG, IND_END, &
     &           DIMS(3), IND_STA, IER
      LOGICAL*1  FL_LET, FL_LAST_DIR
      INTEGER*4  K1, K2, K3
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, LINDEX, LTM_DIF
!
      DO 410 J1=1,VTD__M_STA
         VTD%STATUS_SPD(J1) = VTD__NONE
         DO 420 J2=1,VTD__M_EPD
            CALL CLRCH ( FIL_SPD(J1,J2) )
 420    CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,VTD__M_EPD
         IF ( ILEN(VTD%CONF%DIR_EPD(J3)) > 0 ) THEN
              LEV = 0
              DO 440 J4=1,1024*1024*1024
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, VTD%CONF%DIR_EPD(J3), &
     &                                    FILNAM )
                 IF ( LEV == 0 ) GOTO 840
                 IF ( IS .NE. 0 ) THEN
                      CALL ERR_LOG ( 3811, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &                    'reading directory '// &
     &                    VTD%CONF%DIR_EPD(J3)(1:I_LEN(VTD%CONF%DIR_EPD(J3)))// &
     &                    ' -- '//FILNAM(1:I_LEN(FILNAM))//' where files '// &
     &                    'with slant path delay are supposed to reside' )
                      RETURN 
                 END IF
!
                 IL = ILEN(FILNAM) 
                 IF ( IL < 13 ) GOTO 440
                 IF ( FILNAM(IL-4:IL) .NE. '.bspd' ) GOTO 440
!
! -------------- Build the station name from the file name
!
                 FL_LET = .FALSE.
                 DO 450 J5=8,1,-1
                    STA_FIL(J5:J5) = FILNAM(IL-13+J5:IL-13+J5) 
!
! ----------------- Replace trailing underscores with blanks
!
                    IF ( .NOT. FL_LET .AND. STA_FIL(J5:J5) == '_' ) THEN
                          STA_FIL(J5:J5) = ' ' 
                    END IF
                    IF ( STA_FIL(J5:J5) .NE. '_' .AND. &
     &                   STA_FIL(J5:J5) .NE. ' '       ) THEN
                         FL_LET = .TRUE.
                    END IF
!
! ----------------- Replace letters of small register to letters of big register
!
                    IF ( ICHAR(STA_FIL(J5:J5)) .GE. ICHAR('a') .AND. &
     &                   ICHAR(STA_FIL(J5:J5)) .LE. ICHAR('z')       ) THEN
                         STA_FIL(J5:J5) = CHAR(ICHAR(STA_FIL(J5:J5))-32)
                    END IF
 450             CONTINUE 
!
! -------------- NB: IVS_NAME is in the upper register, but station name in
! -------------- the external file ids in the lower register
!
                 DO 460 J6=1,L_STA
                    IF ( C_STA(J6) == STA_FIL ) THEN
                         FIL_SPD(J6,J3) = FILNAM
                    END IF
 460             CONTINUE 
 440         CONTINUE 
 840         CONTINUE 
         END IF
 430  CONTINUE 
!
      CALL CLRCH  ( OUT )
      ILO = 1
      NM_STA = 0
      DO 470 J7=1,L_STA
         DO 480 J8=1,VTD__M_EPD
            IF ( ILEN(FIL_SPD(J7,J8)) > 1 ) GOTO 470
 480     CONTINUE 
!
         IF ( VTD%STA(J7)%STA_TYP .NE. VTD__GC ) THEN
              OUT(ILO:ILO+7) = C_STA(J7)
              OUT(ILO+8:ILO+8) =  ' ' 
              ILO = ILO + 9
              NM_STA = NM_STA + 1
         END IF
 470  CONTINUE 
!
      IF ( VTD%CONF%FL_WARN .AND. NM_STA > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NM_STA, STR )
           WRITE ( 6, '(A)' ) 'VTD_LOAD WARNING: Cannot find files with '// &
     &                        'slant path delays for '//STR(1:I_LEN(STR))// &
     &                        ' stations: '//OUT(1:I_LEN(OUT))
      END IF
!
      DO 490 J9=1,L_STA
         DO 4100 J10=1,VTD__M_EPD
            IF ( VTD%STATUS_SPD(J9) == SPD__INTR ) GOTO 490
            IF ( ILEN(FIL_SPD(J9,J10)) == 0 ) GOTO 4100
            IF ( J10 == VTD__M_EPD ) THEN
                 FL_LAST_DIR = .TRUE.
               ELSE 
                 IF ( ILEN(FIL_SPD(J9,J10+1)) == 0 ) THEN
                     FL_LAST_DIR = .TRUE.
                   ELSE
                     FL_LAST_DIR = .FALSE.
                 END IF
            END IF
            ID = LINDEX ( FIL_SPD(J9,J10), '/' )
            IF ( ID > 1 ) THEN
!
! -------------- Check for the read lock file 
!
                 LOCK_FILE = FIL_SPD(J9,J10)(1:ID)//'spd_read_lock'
                 CALL CHECK_SPD_LOCK_AND_WAIT ( LOCK_FILE )
            END IF
!
! --------- Read the header of the input file with sland path delay 
! --------- on binary format
!
            CALL ERR_PASS ( IUER, IER )
            CALL SPD_3D_BIN_READ_HEAD ( FIL_SPD(J9,J10), VTD%SPD_3D(J9), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3812, IUER, 'VTD_LOAD_SPD', 'Error in an '// &
     &               'attempt to parse the header of the slant path delay '// &
     &               'file '//FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))// &
     &               ' for station '//C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Determine indexes of slant path delay record for the beginning and 
! --------- the end of observations interval
!
            IND_BEG = ( (MJD_BEG*86400.0 + TAI_BEG) - &
     &          (VTD%SPD_3D(J9)%TIM%MJD_BEG*86400.0 + VTD%SPD_3D(J9)%TIM%TAI_BEG - &
     &           VTD%CONF%ATHD_LAG) )/VTD%SPD_3D(J9)%TIM%TIM_STEP
!
            IND_END = ( (MJD_END*86400.0 + TAI_END) - &
     &          (VTD%SPD_3D(J9)%TIM%MJD_BEG*86400.0 + VTD%SPD_3D(J9)%TIM%TAI_BEG) )/ &
     &           VTD%SPD_3D(J9)%TIM%TIM_STEP
!
            IF ( (MJD_END*86400.0 + TAI_END) >  &
     &           (VTD%SPD_3D(J9)%TIM%MJD_BEG*86400.0 + VTD%SPD_3D(J9)%TIM%TAI_BEG) + &
     &           IND_END*VTD%SPD_3D(J9)%TIM%TIM_STEP ) THEN
                 IND_END = IND_END + 1
            END IF
!
            IF ( IND_BEG < 1 ) THEN
                 IF ( FL_LAST_DIR ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( VTD%SPD_3D(J9)%TIM%MJD_BEG, &
     &                                          VTD%SPD_3D(J9)%TIM%TAI_BEG, -2 )
                      STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -2 )
                      CALL ERR_LOG ( 3813, IUER, 'VTD_LOAD_SPD', 'The file with '// &
     &                    'slant path delay for '// &
     &                     FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))//' has the first '// &
     &                    'epoch '//STR(1:21)//', which later than the first '// &
     &                    'epoch of observations '//STR1 )
                      RETURN 
                    ELSE
                      GOTO 4100
                 END IF
            END IF
!
            IF ( IND_END > VTD%SPD_3D(J9)%LAB%TOT_NUM_DEL ) THEN
                 IF ( FL_LAST_DIR ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( VTD%SPD_3D(J9)%TIM%MJD_END, &
     &                                        VTD%SPD_3D(J9)%TIM%TAI_END, -2 )
                      STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, -2 )
                      WRITE ( 6, * ) 'VTD%SPD_3D(J9)%LAB%TOT_NUM_DEL= ', VTD%SPD_3D(J9)%LAB%TOT_NUM_DEL
                      CALL ERR_LOG ( 3814, IUER, 'VTD_LOAD_SPD', 'The file with '// &
     &                    'slant path delay for '// &
     &                     FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))//' has the last '// &
     &                    'epoch '//STR(1:21)//', which earlier than the first '// &
     &                    'epoch of observations '//STR1 )
                      RETURN 
                   ELSE 
                      GOTO 4100
                 END IF
            END IF
!
            IF ( IND_BEG - M_SPD__OVR > 1 ) THEN
                 IND_BEG = IND_BEG - M_SPD__OVR
                ELSE 
                 IND_BEG = 1
            END IF
!
            IF ( IND_END + M_SPD__OVR < VTD%SPD_3D(J9)%LAB%TOT_NUM_DEL ) THEN
                 IND_END = IND_END + M_SPD__OVR  
               ELSE 
                 IND_END = VTD%SPD_3D(J9)%LAB%TOT_NUM_DEL
            END IF
!
            VTD%SPD_3D(J9)%TIM_BEG = VTD%SPD_3D(J9)%TIM%MJD_BEG*86400.0D0 + &
     &                               VTD%SPD_3D(J9)%TIM%TAI_BEG + &
     &                               (IND_BEG-1)*VTD%SPD_3D(J9)%TIM%TIM_STEP
!
! --------- Get the two arrays of slant path delay at the 2D mesh
!
            CALL ERR_PASS ( IUER, IER )
            CALL SPD_3D_BIN_READ_DEL ( FIL_SPD(J9,J10), VTD%SPD_3D(J9), &
     &                                 IND_BEG, IND_END, 1-SPD__MDEG, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3815, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &               'an attempt to read delay records from the slant '// &
     &               'path delay file '//FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))// &
     &               ' for station '//C_STA(J9) )
                 RETURN 
            END IF
!
            DIMS(1) = VTD%SPD_3D(J9)%ELV%N_EL
            DIMS(2) = VTD%SPD_3D(J9)%AZM%N_AZ
            DIMS(3) = VTD%SPD_3D(J9)%N_TIM
!
            DO 4110 J11=1,VTD%SPD_3D(J9)%MOD%N_RFR
!
! ------------ Compute the B-spline expansion for slant path delay
!
               CALL ERR_PASS ( IUER, IER )
               CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMS, &
     &                             VTD%SPD_3D(J9)%MAP_ARR, &
     &                             VTD%SPD_3D(J9)%AZM%AZIM, &
     &                             VTD%SPD_3D(J9)%TIM_ARR, &
     &              VTD%SPD_3D(J9)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J11), &
     &              IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 3816, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &                  'an attempt to compute B-spline expansion for the '// &
     &                  'first component of the slant path delay for '// &
     &                  'station '//C_STA(J9) )
                    RETURN 
               END IF
!
! ------------ Compute the B-spline expansion for the path delay in zenith direction
!
               CALL ERR_PASS ( IUER, IER )
               CALL BSPL4_1D_CMP ( SPD__MDEG, 0, VTD%SPD_3D(J9)%N_TIM, &
     &                             VTD%SPD_3D(J9)%TIM_ARR, &
     &                             VTD%SPD_3D(J9)%ZEN_DEL(1-SPD__MDEG,J11), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 3817, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &                  'an attempt to compute B-spline expansion for the '// &
     &                  'path delay in zenith direction at station '// &
     &                   C_STA(J9) )
                    RETURN 
               END IF
 4110       CONTINUE 
!
! --------- Compute the B-spline expansion for surface pressure
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, VTD%SPD_3D(J9)%N_TIM, &
     &                          VTD%SPD_3D(J9)%TIM_ARR, &
     &                          VTD%SPD_3D(J9)%SUR_PRS, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3818, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface atmosperic pressire at station '// &
     &                C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Compute the B-spline expansion for surface pressure
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, VTD%SPD_3D(J9)%N_TIM, &
     &                          VTD%SPD_3D(J9)%TIM_ARR, &
     &                          VTD%SPD_3D(J9)%SUR_PWP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3819, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface atmosperic pressire at station '// &
     &                C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Compute the B-spline expansion for surface temperature
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, VTD%SPD_3D(J9)%N_TIM, &
     &                          VTD%SPD_3D(J9)%TIM_ARR, &
     &                          VTD%SPD_3D(J9)%SUR_TEM, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3820, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface air temperature at station '//C_STA(J9) )
                 RETURN 
            END IF
!
            VTD%SPD_3D(J9)%ZEN_BIAS = 0.0D0
            VTD%STATUS_SPD(J9) = SPD__INTR
 4100    CONTINUE 
 490  CONTINUE 
!
      IF ( ILEN(VTD%CONF%SPD_BIAS_FILE) > 0 ) THEN
!
! -------- Now read the the file with SPD biases
!
           M_FIL = 16384
!
! -------- Allocate memory for a temporary buffer
!
           ALLOCATE ( BUF(M_FIL), STAT=IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( M_FIL, STR )
                CALL ERR_LOG ( 3821, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for SPD path delay biases' )
                RETURN 
           END IF
!
! -------- Read the file with SPD biases
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( VTD%CONF%SPD_BIAS_FILE, M_FIL, BUF, L_FIL, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3822, IUER, 'VTD_LOAD_SPD', 'Error in '// &
     &            'an attempt to read the buffer with the SPD path delay '// &
     &            'biases' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Check the header and trailing labels
!
           IF ( BUF(1) .NE. SPD_3D_BIAS__LABEL ) THEN
                CALL CLRCH ( STR )
                CALL TRAN  ( 13, BUF(1), STR )
                CALL ERR_LOG ( 3823, IUER, 'VTD_LOAD_SPD', 'Unrecogized '// &
     &              'format of the the SPD path delay bias file '// &
     &               VTD%CONF%SPD_BIAS_FILE(1:I_LEN(VTD%CONF%SPD_BIAS_FILE))// &
     &              ' : the first line is '//STR(1:I_LEN(STR))//' while '// &
     &              'the label '//SPD_3D_BIAS__LABEL//' was expected' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
           IF ( BUF(L_FIL) .NE. SPD_3D_BIAS__LABEL ) THEN
                CALL ERR_LOG ( 3824, IUER, 'VTD_LOAD_SPD', 'The SPD path '// &
     &              'delay bias file '// &
     &               VTD%CONF%SPD_BIAS_FILE(1:I_LEN(VTD%CONF%SPD_BIAS_FILE))// &
     &              ' : was not read up to the end: the last line is not '// &
     &              'the label '//SPD_3D_BIAS__LABEL//' as was expected' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Parse the file with SPD biases
!
           DO 4120 J12=1,L_FIL
              IF ( BUF(J12)(1:1) == 'B' ) THEN
!
! ---------------- Search whether this station will be used
!
                   IND_STA = LTM_DIF ( 1, L_STA, C_STA, BUF(J12)(12:19) )
                   IF ( IND_STA > 0 ) THEN
!
! --------------------- Decode the value of the bias
!
                        READ ( UNIT=BUF(J12)(23:34), FMT='(1PD12.5)', IOSTAT=IER ) &
     &                         VTD%SPD_3D(IND_STA)%ZEN_BIAS 
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J12, STR )
                             CALL ERR_LOG ( 3825, IUER, 'VTD_LOAD_SPD', &
     &                           'Error in decoding SPD path delay bias '// &
     &                           'at line '//STR(1:I_LEN(STR))// &
     &                           ' of the file '// &
     &                           VTD%CONF%SPD_BIAS_FILE(1:I_LEN(VTD%CONF%SPD_BIAS_FILE))// &
     &                           ' : '//BUF(J12)(23:34) )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
!
                        READ ( UNIT=BUF(J12)(38:44), FMT='(F7.4)', IOSTAT=IER ) &
     &                         VTD%SPD_3D(IND_STA)%ZEN_SCALE
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J12, STR )
                             CALL ERR_LOG ( 3826, IUER, 'VTD_LOAD_SPD', &
     &                           'Error in decoding SPD path delay scale '// &
     &                           'at line '//STR(1:I_LEN(STR))// &
     &                           ' of the file '// &
     &                           VTD%CONF%SPD_BIAS_FILE(1:I_LEN(VTD%CONF%SPD_BIAS_FILE))// &
     &                           ' : '//BUF(J12)(23:34) )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
                   END IF
              END IF
 4120      CONTINUE 
!
! -------- Deallocate temporary buffer
!
           DEALLOCATE ( BUF )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_LOAD_SPD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_SPD_LOCK_AND_WAIT ( LOCK_FILE )
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_SPD_LOCK_AND_WAIT checks the lock file. If the       *
! *   lock file exists, it reads, extracts the expire time, and if the   *
! *   current time did not reach expire, waits for experiation and then  *
! *   returns.                                                           *
! *   Example of the lock file:                                          *
! *                                                                      *
! *   Lock set on 2024.11.25-14:36:22 Expires on 2024.11.25-14:36:30     *
! *                                                                      *
! *   The expuire date is in the 7th word.                               *
! *   Caveats:                                                           *
! *     1) all errors in searching, opening, reading, and parsing of the *
! *        lock file are interpreted as "no lock is set".                *
! *     2) accuracy of the timer: 10 msec.                               *
! *                                                                      *
! * # 25-NOV-2024 CHECK_SPD_LOCK_AND_WAIT v1.0 (c) L. Petrov 25-NOV-2024 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  LOCK_FILE*(*)
      CHARACTER  BUF(1)*128, TIM_NOW_STR*23
      LOGICAL*1  LEX
      INTEGER*4  MIND
      PARAMETER  ( MIND = 128 ) 
      REAL*8     TIM_NOW, TIM_EXPIRY, TIM_ELAPSED, TIME_REMAINED, TIM_EPS
      PARAMETER  ( TIM_EPS = 0.001D0 )
      INTEGER*4  NB, IND(2,MIND), LIND, MJD_NOW, MJD_EXPIRY, IER
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
!
      INQUIRE ( FILE=LOCK_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           RETURN
      END IF
      CALL RD_TEXT ( LOCK_FILE, 1, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           RETURN
      END IF
      CALL EXWORD ( BUF(1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
      IF ( LIND < 7 ) THEN
           CALL UNLINK ( TRIM(LOCK_FILE)//CHAR(0) )
           RETURN
      END IF
      CALL DATE_TO_TIME ( BUF(1)(IND(1,7):IND(2,7)), MJD_EXPIRY, TIM_EXPIRY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL UNLINK ( TRIM(LOCK_FILE)//CHAR(0) )
           RETURN
      END IF
      TIM_NOW_STR = GET_CDATE_MS()
      CALL DATE_TO_TIME ( TIM_NOW_STR, MJD_NOW, TIM_NOW, IER )
      TIM_ELAPSED = ( (MJD_EXPIRY - MJD_NOW)*86400.0D0 + (TIM_EXPIRY -TIM_NOW) )
      IF ( TIM_ELAPSED > TIM_EPS ) THEN
           CALL NSLEEP ( TIM_ELAPSED, TIME_REMAINED )
      END IF
      CALL UNLINK ( TRIM(LOCK_FILE)//CHAR(0) )
!
      RETURN
      END  SUBROUTINE CHECK_SPD_LOCK_AND_WAIT  !#!#
