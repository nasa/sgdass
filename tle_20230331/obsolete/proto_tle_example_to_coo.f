      PROGRAM     TLE_EXAMPLE_TO_COO
!
! *******************************************************************************
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *                                                                             *
! *******************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'ners.i'
      INCLUDE     'ners_local.i'
      INCLUDE     'astro_constants.i'
      TYPE      ( NERS__TYPE ) :: NERS
      CHARACTER   FIL_TLE*128, FIL_COEF*128, FIL_ORB*128
      CHARACTER   SAT_NAM*24, SAT_CLASS, XINT_DES*8
      INTEGER*4   MJD_TLE, MJD0, NREV, NEQ, NCOEF, NC, IDELT
      INTEGER*4   ISAT_CAT, LY, LNY, IET, NTLE, EPH__MEPOC
      INTEGER*4   IUER, IER
      PARAMETER   ( NCOEF =   8  )
      PARAMETER   ( NEQ   =   6  )
      PARAMETER   ( NC    =  10 )
      PARAMETER  ( EPH__MEPOC = 1024*1024 )
      REAL*8      UTC_TLE, ELEM_TLE(6), XNO,  XNDT2O, XNDD6O
      REAL*8      TLE_MM_DOTDOT, BSTAR, PRD, APG, PRG
      REAL*8      X_CRS(3), XDOT_CRS(3), X(5,3), XDOT(5,3)
      REAL*8      X_TRS(3), XDOT_TRS(3), AZ, EL
      REAL*8      UTC0, TINI, TFIN, DELT_T, TIM(EPH__MEPOC), Y0(NEQ)
      REAL*8      ORB_TRU(NC,NEQ)
      REAL*8      SS, Q0MSS4, THETA, XI, BETA0, ETA, C(5), D(4)
      REAL*8      LERAN, LLOPE, LRAN, LINC, LAOP, SMA, LMA
      REAL*8      CA(2,10), CX(2,8), CZ(2,3), CZZ(2,3,3)
      REAL*8      ELMDOT_AD(6), ELMDOT_LS(6), ELMDOT_RE(6)
      REAL*8      UTC(5), DT(5), DELTT, UTC_BEG, EPOCH
      REAL*8      TAI_BEG, UTC_MTAI
      INTEGER*4   MJD(5), MJD_BEG
      INTEGER*4   J1, J2, J3, J4, IORBTYPE
      REAL*8      XPY(3), XDOTPY(3), XDIF(3), XDOTDIF(3)
      REAL*8      D2201, D2211, D3210, D3222, D4410, D4422, D5220
      REAL*8      D5232, D5421, D5433, XDLT1, XDLT2, XDLT3
      CHARACTER   TLEDATE*30, DATE_BEG*21
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      LOGICAL*1   LEX
      CHARACTER,  EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8      DS50, XMO, XNODEO, OMEGAO, EO, XINCL
      COMMON/ELSET/ EPOCH, DS50, XMO, XNODEO, OMEGAO, EO, XINCL, XNO,	&
     &              XNDT2O, XNDD6O, BSTAR
!
! ---
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
! ---
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE',                    &
     &                  'Error in initializing NERS data structure' )
         RETURN
      END IF
!      
! --- Define TLE file
!
      FIL_TLE = '@TLE_SHARE@/GPS43_22004.tle'
!
! --- Define time epoch to compute for the stat 
!
      IUER = -1
      DATE_BEG = '2022.01.04_07:54:25'
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER ) 
!
! --- convert time to TAI
!
      CALL NERS_GET_UTCMTAI ( NERS,                                     &
     &                        (MJD_BEG - J2000__MJD)*86400 + UTC_BEG,   &
     &                        UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE',                      &
     &                  'Error in extracting UTC minus TAI function' )
         RETURN
      END IF
!
      TAI_BEG = UTC_BEG - UTC_MTAI
!
! --- Python computations
!
      XPY(1)   =-25187.80202756842D3
      XPY(2)   = 8150.829911087384D3
      XPY(3)   = 0.8632685584468792D3
      XDOTPY(1)  =-0.66042785809374D3
      XDOTPY(2)  = -2.101401430691626D3
      XDOTPY(3)  = 3.2022526049987032D3


!
! --- Get the state vectors for the input tle file and epoch
!
      CALL TLE_TO_CRS (FIL_TLE, MJD_BEG, TAI_BEG, X_CRS, XDOT_CRS, IUER)
!
! 
!
      IUER = -1
      CALL TLE_TO_TRS ( FIL_TLE, MJD_BEG, TAI_BEG, X_TRS, XDOT_TRS,     &
     &                  AZ, EL, IUER )

!
! --- print output
!
      WRITE ( 6, * ) 'MJD:  ', MJD_BEG, ' UTC= ', UTC_BEG, ' TAI_BEG= ', TAI_BEG 
      WRITE ( 6, * ) 'DATE: ', DATE_BEG
      WRITE ( 6, * ) 'X_CRS:    ', X_CRS
      WRITE ( 6, * ) 'X_CRS_PY: ', XPY
      WRITE ( 6, * ) 'X_TRS:    ', X_TRS
      WRITE ( 6, * ) 'V_CRS:    ', XDOT_CRS
      WRITE ( 6, * ) 'V_CRS_PY: ', XDOTPY
      WRITE ( 6, * ) 'V_TRS:    ', XDOT_TRS

      WRITE ( 6, * ) '-------------------------------------------'

      XDIF = X_CRS - XPY
      XDOTDIF =  XDOT_CRS - XDOTPY

      WRITE ( 6, * ) 'XDIF:    ', XDIF
      WRITE ( 6, * ) 'XDOTDIF: ', XDOTDIF
      WRITE ( 6, * ) '-------------------------------------------'
      WRITE ( 6, * ) 'DIST:  ', DSQRT(X_CRS(1)**2 + X_CRS(2)**2 + X_CRS(3)**2),    &
     &                          DSQRT(XPY(1)**2 + XPY(2)**2 + XPY(3)**2), &
     &                          DSQRT(XDIF(1)**2 + XDIF(2)**2 + XDIF(3)**2)
      WRITE ( 6, * ) 'SPEED: ', DSQRT(XDOT_CRS(1)**2 + XDOT_CRS(2)**2 + XDOT_CRS(3)**2),    &
     &                          DSQRT(XDOTPY(1)**2 + XDOTPY(2)**2 + XDOTPY(3)**2), &
     &                          DSQRT(XDOTDIF(1)**2 + XDOTDIF(2)**2 + XDOTDIF(3)**2)


!
! MJD:         59562
! UTC:     21600.999648000001
! X:       8896974.989423989         12850768.67048051        -21646640.79703702
! XDOT:   -3514.365295411461         1480.5517974069007       -0554.083267864934


      END PROGRAM TLE_EXAMPLE_TO_COO  !#!#
