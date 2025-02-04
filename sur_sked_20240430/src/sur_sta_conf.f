      SUBROUTINE SUR_STA_CONF ( SUR, STATION_LINE, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_STA_CONF 
! *                                                                      *
! * ### 10-OCT-2005   SUR_STA_CONF   v2.2 (c) L. Petrov 03-APR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      CHARACTER  STATION_LINE*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 512 )
      PARAMETER  ( MIND  = 512 )
      CHARACTER  BUF(M_BUF)*512, STR*512, STR1*128, STR2*128, STA_FIL*128, &
     &           MODE*4, WORD*32
      REAL*8     CF, SF, CL, SL, MU, PP, SEC
      INTEGER*4  N_BUF, J1, J2, J3, J4, J5, J6, NN, LIND, IND(2,MIND), &
     &           LIND2, IND2(2,MIND), N_SUR_STP, N_HAZ, N_HEL, MJD, IER
      LOGICAL*1  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: ATAN_CS
!
      CALL ERR_PASS ( IUER, IER )
      CALL EXWORD ( STATION_LINE, MIND, LIND, IND, ', ', IER )
      IF ( IER .NE. 0 .OR. LIND == 1 ) THEN
           CALL ERR_LOG ( 1681, IUER, 'SUR_STA_CONF', 'Failure in parsing '// &
     &         'station list '//TRIM(STATION_LINE)//' only one station '// &
     &         'is defined, while at least two stations are required for '// &
     &         'a VLBI experiment' )
           RETURN 
      END IF
!
      SUR%REF_STA = 0
      SUR%L_STA = 0
      DO 410 J1=1,LIND
         STR = STATION_LINE(IND(1,J1):IND(2,J1))
         CALL EXWORD ( STR, MIND, LIND2, IND2, ': ', IER )
!
         SUR%L_STA = SUR%L_STA + 1
         CALL NOUT ( SIZEOF(SUR%STA(SUR%L_STA)), SUR%STA(SUR%L_STA) )
         SUR%STA(SUR%L_STA)%SATELLITE = .FALSE.
         SUR%STA(SUR%L_STA)%NAME = STR(IND2(1,1):IND2(2,1))
         SUR%STA(SUR%L_STA)%TAGALONE = .FALSE.
         SUR%STA(SUR%L_STA)%STICKY   = .FALSE.
         SUR%STA(SUR%L_STA)%REF      = .FALSE.
!
         CALL TRAN ( 11, SUR%STA(SUR%L_STA)%NAME, SUR%STA(SUR%L_STA)%NAME )
         IF ( LIND2 > 1 ) THEN
              DO 420 J2=2,LIND2
                 CALL TRAN ( 12, STR(IND2(1,J2):IND2(2,J2)), STR(IND2(1,J2):IND2(2,J2)) )
                 IF ( STR(IND2(1,J2):IND2(2,J2)) == 's' ) THEN
                      SUR%STA(SUR%L_STA)%STICKY   = .TRUE.
                    ELSE IF ( STR(IND2(1,J2):IND2(2,J2)) == 'r' ) THEN
                      SUR%STA(SUR%L_STA)%REF      = .TRUE.
                      IF ( SUR%REF_STA == 0 ) THEN
                           SUR%REF_STA = SUR%L_STA
                         ELSE
                           CALL ERR_LOG ( 1682, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                        'station list '//TRIM(STATION_LINE)//' more than one '// &
     &                        'reference stations is defined' )
                           RETURN 
                      END IF
                    ELSE IF ( STR(IND2(1,J2):IND2(2,J2)) == 't' ) THEN
                      SUR%STA(SUR%L_STA)%TAGALONE = .TRUE.
                    ELSE
                      CALL ERR_LOG ( 1683, IUER, 'SUR_STA_CONF', 'Unsupported '// &
     &                    'qualifier '//STR(IND2(1,J2):IND2(2,J2))//' was found '// &
     &                    'after station '//SUR%STA(SUR%L_STA)%NAME// &
     &                    ' while parsing sattion line. Supported '// &
     &                    'qualifiers are: s, r, t' )
                      RETURN 
                 END IF
 420          CONTINUE 
         END IF
!
         CALL CLRCH ( STA_FIL )
         CALL TRAN ( 12, SUR%STA(SUR%L_STA)%NAME, STA_FIL )
         STA_FIL = TRIM(SUR%STATION_SLEW_DIR)//'/'//TRIM(STA_FIL)//'.stp' 
         INQUIRE ( FILE=STA_FIL, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL CLRCH ( STA_FIL )
              CALL TRAN ( 12, SUR%STA(SUR%L_STA)%NAME, STA_FIL )
              STA_FIL = TRIM(SUR%STATION_SLEW_DIR)//'/'//TRIM(STA_FIL)//'.slew'
         END IF
         INQUIRE ( FILE=STA_FIL, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 1684, IUER, 'SUR_STA_CONF', 'No slewing information '// &
     &            'for station '//SUR%STA(SUR%L_STA)%NAME//' was found: '// &
     &            'file '//TRIM(STA_FIL)//' does not exist. Please create it' )
              RETURN 
         END IF 
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( STA_FIL, M_BUF, BUF, N_BUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1685, IUER, 'SUR_STA_CONF', 'Error in reading '// &
     &            'station configuration file '//STA_FIL )
              RETURN 
         END IF
!
         IF ( BUF(1)(1:LEN(SUR__STP_FMT)) == SUR__STP_FMT ) THEN
              MODE = 'stp'
            ELSE IF ( BUF(1)(1:LEN(SUR__SLEW_FMT)) == SUR__SLEW_FMT ) THEN
              MODE = 'slew'
            ELSE 
              CALL ERR_LOG ( 1686, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &            'station configuration file '//TRIM(STA_FIL)// &
     &            ' -- the first line is '//TRIM(BUF(1))//' while format label '// &
     &            SUR__SLEW_FMT//' or '//SUR__STP_FMT//' was expected' )
              RETURN 
         END IF
!
         SUR%STA(SUR%L_STA)%SETMODE = 0.0D0
         N_HAZ = 0
         N_HEL = 0
         N_SUR_STP = 0
         DO 430 J3=1,N_BUF
            CALL EXWORD ( BUF(J3), MIND, LIND2, IND2, CHAR(0)//CHAR(32)//CHAR(9), IER )
            IF ( BUF(J3)(1:1)  == '#' ) GOTO 430
            IF ( ILEN(BUF(J3)) ==  0  ) GOTO 430
            IF ( LIND2         <   2  ) GOTO 430
            IF ( INDEX ( BUF(J3), 'SHORT_NAME:'  ) .NE. 1 .AND. &
     &           INDEX ( BUF(J3), 'LAST_UPDATE:' ) .NE. 1 .AND. LIND2 < 4 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 1687, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                TRIM(STR)//' line of file '//TRIM(STA_FIL)//' too short '// &
     &                'line. At least 4 words is expected' )
                 RETURN 
            END IF
            IF ( INDEX ( BUF(J3), 'COORD:' ) .EQ. 1 .AND. LIND2 < 6 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 1688, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                TRIM(STR)//' line of file '//TRIM(STA_FIL)//' too short '// &
     &                'line. At least 6 words is expected' )
                 RETURN 
            END IF
            IF ( INDEX ( BUF(J3), 'AZ_RANGE:' ) .EQ. 1 .AND. LIND2 < 7 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 CALL ERR_LOG ( 1689, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                TRIM(STR)//' line of file '//TRIM(STA_FIL)//' too short '// &
     &                'line. At least 7 words is expected' )
                 RETURN 
            END IF
            IF ( INDEX ( BUF(J3), 'SHORT_NAME:' ) == 1 ) THEN
                 IF ( MODE == 'slew' ) THEN
                      SUR%STA(SUR%L_STA)%SHORT_NAME = BUF(J3)(IND2(1,3):IND2(2,3))
                   ELSE IF ( MODE == 'stp' ) THEN
                      SUR%STA(SUR%L_STA)%SHORT_NAME = BUF(J3)(IND2(1,4):IND2(2,4))
                 END IF
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'LAST_UPDATE:' ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%LAST_UPDATE = BUF(J3)(IND2(1,3):IND2(2,3))
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'COORD:' ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%NAME = BUF(J3)(IND2(1,2):IND2(2,2))
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)') SUR%STA(SUR%L_STA)%COO_TRS(1)
                 READ ( UNIT=BUF(J3)(IND2(1,5):IND2(2,5)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%COO_TRS(2)
                 READ ( UNIT=BUF(J3)(IND2(1,6):IND2(2,6)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%COO_TRS(3)
!
! -------------- Calculation longitude LONGITUDE and geocentric lattitude PHI_GCN of
! -------------- the station
!
                 SUR%STA(SUR%L_STA)%LONG = ATAN_CS ( SUR%STA(SUR%L_STA)%COO_TRS(1), &
     &                                        SUR%STA(SUR%L_STA)%COO_TRS(2)  )
                 IF ( SUR%STA(SUR%L_STA)%LONG       .LT. 0.0D0 ) THEN
                      SUR%STA(SUR%L_STA)%LONG = PI2 + SUR%STA(SUR%L_STA)%LONG
                 END IF
!
                 PP  = DSQRT ( SUR%STA(SUR%L_STA)%COO_TRS(1)**2 + &
     &                         SUR%STA(SUR%L_STA)%COO_TRS(2)**2 )
                 IF ( DABS(PP) .LT. 1.D-8 ) PP=1.D-8
                 SUR%STA(SUR%L_STA)%RAD = DSQRT ( SUR%STA(SUR%L_STA)%COO_TRS(3)**2 + PP**2    )
                 IF ( SUR%STA(SUR%L_STA)%RAD < VTD__HEIGHT_MIN .OR. &
     &                SUR%STA(SUR%L_STA)%RAD > VTD__HEIGHT_MAX      ) THEN
!
                      CALL CLRCH ( STR )
                      WRITE ( UNIT=STR, FMT='(I12)' ) J2
                      CALL CHASHL ( STR  )
                      CALL CLRCH  ( STR2 )
                      WRITE ( UNIT=STR2, FMT='(3(F15.3,2X))' ) &
     &                        ( SUR%STA(SUR%L_STA)%COO_TRS(NN), NN=1,3 )
                      CALL ERR_LOG ( 1690, IUER, 'SUR_STA_CONF', 'Wrong '// &
     &                    'positions of station '//SUR%STA(SUR%L_STA)%NAME// &
     &                    ' -- '//STR2(1:ILEN(STR2))// &
     &                    ' -- they are not on the surface of our planet' )
                      RETURN
                 END IF
!
                 SUR%STA(SUR%L_STA)%LAT_GCN = DATAN( SUR%STA(SUR%L_STA)%COO_TRS(3)/PP )
!
! -------------- Computation of geodetic latitude
!
                 MU = DATAN ( SUR%STA(SUR%L_STA)%COO_TRS(3)/PP * &
     &                        ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/ &
     &                          SUR%STA(SUR%L_STA)%RAD  ) )
!
                 SUR%STA(SUR%L_STA)%LAT_GDT = &
     &               DATAN( ( (1.D0 - VTD__FE)*SUR%STA(SUR%L_STA)%COO_TRS(3) + &
     &                         VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                        ( (1.D0 - VTD__FE)* &
     &                        ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
! -------------- Computation of height above the ellipsoid
!
                 SUR%STA(SUR%L_STA)%HEI_ELL = PP*DCOS(SUR%STA(SUR%L_STA)%LAT_GDT) + &
     &                       SUR%STA(SUR%L_STA)%COO_TRS(3)*DSIN(SUR%STA(SUR%L_STA)%LAT_GDT) - &
     &                       VTD__REA* &
     &                       DSQRT( 1.D0 - VTD__EXC_SQ*DSIN(SUR%STA(SUR%L_STA)%LAT_GDT)**2 )
!
! -------------- Computation of the local gravity accelration on the ellipsoid
!
                 SUR%STA(SUR%L_STA)%GAC_ELL = VTD__ACC_EQU* &
     &                       (1.D0 + VTD__GRV_LAT* DSIN(SUR%STA(SUR%L_STA)%LAT_GCN)**2 + &
     &                       VTD__GRV_H*VTD__GRV_H )/ &
     &                       DSQRT (1.D0 - VTD__EXC_SQ*DSIN(SUR%STA(SUR%L_STA)%LAT_GCN)**2 )
!
! -------------- Calculation matrix of transformation from REN (local topocentric,
! -------------- (Radial,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
                 CF = DCOS(SUR%STA(SUR%L_STA)%LAT_GCN)
                 SF = DSIN(SUR%STA(SUR%L_STA)%LAT_GCN)
                 CL = DCOS(SUR%STA(SUR%L_STA)%LONG)
                 SL = DSIN(SUR%STA(SUR%L_STA)%LONG)
!
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(1,1) = CF*CL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(2,1) = CF*SL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(3,1) = SF
!
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(1,2) = -SL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(2,2) =  CL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(3,2) =  0.D0
!
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(1,3) = -SF*CL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(2,3) = -SF*SL
                 SUR%STA(SUR%L_STA)%REN_TO_TRS(3,3) =  CF
!
! -------------- Calculation matrix of transformation from UEN (local topocentric,
! -------------- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
                 CF = DCOS(SUR%STA(SUR%L_STA)%LAT_GDT)
                 SF = DSIN(SUR%STA(SUR%L_STA)%LAT_GDT)
!
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(1,1) = CF*CL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(2,1) = CF*SL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(3,1) = SF
!
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(1,2) = -SL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(2,2) =  CL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(3,2) =  0.D0
!
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(1,3) = -SF*CL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(2,3) = -SF*SL
                 SUR%STA(SUR%L_STA)%UEN_TO_TRS(3,3) =  CF
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'MOUNT:' ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%MOUNT_TYPE = BUF(J3)(IND2(1,4):IND2(2,4))
                 IF ( SUR%STA(SUR%L_STA)%MOUNT_TYPE == MT__ALTAZ ) THEN
                      CONTINUE 
                   ELSE IF ( SUR%STA(SUR%L_STA)%MOUNT_TYPE == MT__EQUAT ) THEN
                      CONTINUE 
                   ELSE IF ( SUR%STA(SUR%L_STA)%MOUNT_TYPE == MT__XY_N  ) THEN
                      CONTINUE 
                   ELSE IF ( SUR%STA(SUR%L_STA)%MOUNT_TYPE == MT__XY_E  ) THEN
                      CONTINUE 
                   ELSE 
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 1691, IUER, 'SUR_STA_CONF', 'Unsupported '// &
     &                    'mounting type '//BUF(J3)(IND2(1,4):IND2(2,4))// &
     &                    ' at line '//STR(1:I_LEN(STR))//' was found in the '// &
     &                    'station file '//STATION_LINE(1:I_LEN(STATION_LINE)) )
                      RETURN 
                 END IF
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'SLEW_EL:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%SLEW_RATE_EL
                 SUR%STA(SUR%L_STA)%SLEW_RATE_EL = &
     &                   SUR%STA(SUR%L_STA)%SLEW_RATE_EL*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'SLEW_AZ:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%SLEW_RATE_AZ
                 SUR%STA(SUR%L_STA)%SLEW_RATE_AZ = &
     &                   SUR%STA(SUR%L_STA)%SLEW_RATE_AZ*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'ACCL_EL:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%SLEW_ACCL_EL
                 SUR%STA(SUR%L_STA)%SLEW_ACCL_EL = &
     &                   SUR%STA(SUR%L_STA)%SLEW_ACCL_EL*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'ACCL_AZ:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%SLEW_ACCL_AZ
                 SUR%STA(SUR%L_STA)%SLEW_ACCL_AZ = &
     &                   SUR%STA(SUR%L_STA)%SLEW_ACCL_AZ*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'TSETTLE_AZ:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%TIME_SETTLE_AZ
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'TSETTLE_EL:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%TIME_SETTLE_EL
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'EL_MIN:'  ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%EL_MIN
                 SUR%STA(SUR%L_STA)%EL_MIN = SUR%STA(SUR%L_STA)%EL_MIN*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'EL_MAX:'  ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%EL_MAX
                 SUR%STA(SUR%L_STA)%EL_MAX = SUR%STA(SUR%L_STA)%EL_MAX*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'AZ_RANGE:'  ) == 1 ) THEN
                 DO 440 J4=1,4
                    READ ( UNIT=BUF(J3)(IND2(1,3+J4):IND2(2,3+J4)), FMT='(F13.4)' ) SUR%STA(SUR%L_STA)%AZ_RANGE(J4)
                    SUR%STA(SUR%L_STA)%AZ_RANGE(J4) = SUR%STA(SUR%L_STA)%AZ_RANGE(J4)*DEG__TO__RAD
 440             CONTINUE 
                 SUR%STA(SUR%L_STA)%AZ_ACC_MIN = SUR%STA(SUR%L_STA)%AZ_RANGE(1)
                 SUR%STA(SUR%L_STA)%AZ_ACC_MAX = SUR%STA(SUR%L_STA)%AZ_RANGE(4)
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'HOR_AZIM:'  ) == 1 ) THEN
                 N_HAZ = LIND2 - 3
                 DO 450 J5=1,N_HAZ
                    WORD = BUF(J3)(IND2(1,J5+3):IND2(2,J5+3))
                    IF ( INDEX ( WORD, '.' ) < 1 ) THEN
                         WORD = TRIM(WORD)//'.0'
                    END IF 
                    READ ( UNIT=WORD, FMT='(F10.5)' ) SUR%STA(SUR%L_STA)%AZ_HM(J5)
                    SUR%STA(SUR%L_STA)%AZ_HM(J5) = SUR%STA(SUR%L_STA)%AZ_HM(J5)*DEG__TO__RAD
 450             CONTINUE 
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'HOR_ELEV:'  ) == 1 ) THEN
                 N_HEL = LIND2 - 3
                 DO 460 J6=1,N_HEL
                    WORD = BUF(J3)(IND2(1,J6+3):IND2(2,J6+3))
                    IF ( INDEX ( WORD, '.' ) < 1 ) THEN
                         WORD = TRIM(WORD)//'.0'
                    END IF 
                    READ ( UNIT=WORD, FMT='(F10.5)' ) SUR%STA(SUR%L_STA)%EL_HM(J6)
                    SUR%STA(SUR%L_STA)%EL_HM(J6) = SUR%STA(SUR%L_STA)%EL_HM(J6)*DEG__TO__RAD
 460             CONTINUE 
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'RECORDER:'  ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%RECORDER = BUF(J3)(IND2(1,4):IND2(2,4))
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'BACKEND:'  ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%BACKEND = BUF(J3)(IND2(1,4):IND2(2,4))
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'CLOCK_DATES:' ) == 1 ) THEN
                 SUR%STA(SUR%L_STA)%CLOCK_DATE_BEG = BUF(J3)(IND2(1,4):IND2(2,4))
                 SUR%STA(SUR%L_STA)%CLOCK_DATE_END = BUF(J3)(IND2(1,5):IND2(2,5))
                 CALL ERR_PASS ( IUER, IER ) 
                 CALL DATE_TO_TIME ( SUR%STA(SUR%L_STA)%CLOCK_DATE_BEG, MJD, SEC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 1692, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                    'line '//TRIM(STR)//' of the station defintion file '// &
     &                    TRIM(STA_FIL)//' wrong start date format' )
                      RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER ) 
                 CALL DATE_TO_TIME ( SUR%STA(SUR%L_STA)%CLOCK_DATE_END, MJD, SEC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 1693, IUER, 'SUR_STA_CONF', 'Error in parsing '// &
     &                    'line '//TRIM(STR)//' of the station defintion file '// &
     &                    TRIM(STA_FIL)//' wrong stop date format' )
                      RETURN 
                 END IF
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'CLOCK_OFFSET:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F12.5)' ) SUR%STA(SUR%L_STA)%CLOCK_OFFSET 
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'STOW_AZIM:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F12.5)' ) SUR%STA(SUR%L_STA)%STOW_AZIM
                 SUR%STA(SUR%L_STA)%STOW_AZIM = SUR%STA(SUR%L_STA)%STOW_AZIM*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'STOW_ELEV:' ) == 1 ) THEN
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT='(F12.5)' ) SUR%STA(SUR%L_STA)%STOW_ELEV
                 SUR%STA(SUR%L_STA)%STOW_ELEV = SUR%STA(SUR%L_STA)%STOW_ELEV*DEG__TO__RAD
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'PREOB:' ) == 1 ) THEN
                 IF ( INDEX(BUF(J3)(IND2(1,4):IND2(2,4)), '.' ) < 1 ) THEN
                      IND2(2,4) = IND2(2,4) + 1
                      BUF(J3)(IND2(2,4):IND2(2,4)) = '.'
                 END IF
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT=* ) SUR%STA(SUR%L_STA)%PREOB
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'POSTOB:' ) == 1 ) THEN
                 IF ( INDEX(BUF(J3)(IND2(1,4):IND2(2,4)), '.' ) < 1 ) THEN
                      IND2(2,4) = IND2(2,4) + 1
                      BUF(J3)(IND2(2,4):IND2(2,4)) = '.'
                 END IF
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT=* ) SUR%STA(SUR%L_STA)%POSTOB
                 N_SUR_STP = N_SUR_STP + 1
               ELSE IF ( INDEX ( BUF(J3), 'SETMODE:' ) == 1 ) THEN
                 IF ( INDEX(BUF(J3)(IND2(1,4):IND2(2,4)), '.' ) < 1 ) THEN
                      IND2(2,4) = IND2(2,4) + 1
                      BUF(J3)(IND2(2,4):IND2(2,4)) = '.'
                 END IF
                 READ ( UNIT=BUF(J3)(IND2(1,4):IND2(2,4)), FMT=* ) SUR%STA(SUR%L_STA)%SETMODE
                 N_SUR_STP = N_SUR_STP + 1
            END IF
 430     CONTINUE 
!
         IF ( N_HAZ .NE. N_HEL ) THEN
              CALL ERR_LOG ( 1694, IUER, 'SUR_STA_CONF', 'Error in processing '// &
     &            'station file '//TRIM(STA_FIL)//' -- the number of words for '// &
     &            'keywords HOR_AZIM and HOR_ELEV does not match' ) 
              RETURN 
         END IF
         SUR%STA(SUR%L_STA)%N_HM = N_HAZ
!
         IF ( MODE == 'stp' .AND. N_SUR_STP < M__SUR_STP ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( N_SUR_STP,  STR  )
              CALL INCH  ( M__SUR_STP, STR1 )
              CALL ERR_LOG ( 1695, IUER, 'SUR_STA_CONF', 'Not all parameters '// &
     &            'were found in station file '//TRIM(STA_FIL)//' -- only '// &
     &             STR(1:I_LEN(STR))//' out of '//STR1 )
              RETURN 
         END IF
!
         IF ( MODE == 'slew' .AND. N_SUR_STP < M__SUR_SLE ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( N_SUR_STP,  STR  )
              CALL INCH  ( M__SUR_SLE, STR1 )
              CALL ERR_LOG ( 1696, IUER, 'SUR_STA_CONF', 'Not all parameters '// &
     &            'were found in station file '//TRIM(STA_FIL)//' -- only '// &
     &             STR(1:I_LEN(STR))//' out of '//STR1 )
              RETURN 
         END IF
 410  CONTINUE
!
      IF ( SUR%REF_STA == 0 ) THEN
           IER = IUER 
           CALL ERR_LOG ( 1697, IUER, 'SUR_STA_CONF', 'Reference '// &
     &         'station is not defined in the station list '// &
     &          STATION_LINE )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_STA_CONF  !#!#
