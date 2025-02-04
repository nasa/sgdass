      PROGRAM    THERM_EXP_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  THERM_EXP  computes time series of VLBI atntenna height   *
! *   variations due to atnenna thermal expansion.                       *
! *                                                                      *
! * ### 20-NOV-2013  THERM_EXP_MAIN  v1.2 (c) L. Petrov  15-JUL-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'anti.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( ANTENNA_DATA__TYPE ) :: ANTI
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_Q, HEB_TEM, HEB_OH, HEB_GEOID_BSPL
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FIL_CONF*128, FIL_MET*128, FIL_STA*128, FIL_TEM*128, &
     &           FIL_DELP*128, FIL_Q*128, FIL_ANT*128, OUT_PREF*128, &
     &           DATE_FIL*21, FILOUT*128
      INTEGER*4     M_MOD, M_INP
      PARAMETER  (  M_MOD = 128 ) 
      PARAMETER  (  M_INP = 128 ) 
      REAL*8     EDGE_SEC
      PARAMETER  ( EDGE_SEC = 0.0D0 ) 
      CHARACTER  MOD_TEXT(M_MOD)*128, INP_TEXT(M_INP)*128, STR*128
      LOGICAL*4  LEX
      REAL*8     SEC_FIL
      INTEGER*4  J1, MJD_FIL, TYP_MET, TYP_DAT, IVRB, N_MOD, N_INP, &
     &           IT, IL, IH, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CALL SET_SIGNAL_CTRLC ( 1 )
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: therm_exp fil_conf fil_met fil_ant '// &
     &                        'out_pref verbosity'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Parse arguments
!
           CALL GETARG ( 1, FIL_CONF )
           INQUIRE ( FILE=FIL_CONF, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5201, -2, 'THERM_EXP_MAIN', 'Wrong '// &
     &              'first argument: configuration file '// &
     &              FIL_CONF(1:I_LEN(FIL_CONF))//' is not found' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 2, FIL_MET  )
           INQUIRE ( FILE=FIL_MET, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5202, -2, 'THERM_EXP_MAIN', 'Wrong '// &
     &              'second argument: meteorological file '// &
     &              FIL_MET(1:I_LEN(FIL_MET))//' is not found' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 3, FIL_ANT )
           INQUIRE ( FILE=FIL_ANT, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5203, -2, 'THERM_EXP_MAIN', 'Wrong '// &
     &              'second argument: meteorological file '// &
     &              FIL_ANT(1:I_LEN(FIL_ANT))//' is not found' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 4, OUT_PREF )
           CALL GETARG ( 5, STR    )
           CALL CHIN   ( STR, IVRB )
      END IF
!
! --- Read and parse configuration file
!
      IUER = -1
      CALL SPD_3D_CONF ( FIL_CONF, SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5203, -2, 'THERM_EXP_MAIN', 'Failure in parsing '// &
     &         'input configuration file '//FIL_CONF )
           CALL EXIT ( 1 )
      END IF
!
! --- Read and parse station file
!
      IUER = -1
      CALL ANTI_PARSE ( FIL_ANT, ANTI, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5205, -2, 'THERM_EXP_MAIN', 'Failure in loading '// &
     &         'the list of stations' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'ANTI%N_ANT= ', ANTI%N_ANT
!
! --- Read and parse station file
!
      IUER = -1
      CALL SPD_3D_INP_STA ( SPD, EDGE_SEC, HEB_GEOID_BSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5206, -2, 'THERM_EXP_MAIN', 'Failure in loading '// &
     &         'the list of stations' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) WRITE ( 6, * ) 'SPD%NSTA=  ', SPD%NSTA
!
      IL = ILEN(FIL_MET)
      IT = LINDEX ( FIL_MET, 't_' ) 
      IF ( IT .LE. 0 ) THEN
           CALL ERR_LOG ( 5207, -2, 'THERM_EXP_MAIN', 'Failure in parsing '// &
     &         'name of the input meteorological file with temperature: '// &
     &          FIL_MET(1:I_LEN(FIL_MET))//' -- we expected to find '// &
     &         'substring t_ there' )
           CALL EXIT ( 1 )
      END IF
      FIL_TEM  = FIL_MET
      FIL_DELP = FIL_MET(1:IT-3)//'d/d_'//FIL_MET(IT+2:)
      FIL_Q    = FIL_MET(1:IT-3)//'q/q_'//FIL_MET(IT+2:)
!
! --- Read TEM-file
!
      IUER = -1
      CALL READ_HEB ( FIL_TEM, HEB_TEM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5208, -2, 'THERM_EXP_MAIN', 'Failure in '// &
     &         'an attempt to read the HEB file with temperature '// &
     &         FIL_TEM )
           CALL EXIT ( 1 )
      END IF
!
! --- Read Q-file
!
      IUER = -1
      CALL READ_HEB ( FIL_Q, HEB_Q, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5209, -2, 'THERM_EXP_MAIN', 'Failure in '// &
     &         'an attempt to read the HEB file with humidity '//FIL_Q )
           CALL EXIT ( 1 )
      END IF
!
! --- Read DELP-file
!
      IUER = -1
      CALL READ_HEB ( FIL_DELP, HEB_DELP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5210, -2, 'THERM_EXP_MAIN', 'Failure in an '// &
     &         'attempt to read the HEB file with pressure layer '// &
     &         'thickness temperature '//FIL_DELP )
           CALL EXIT ( 1 )
      END IF
!
! --- Read OH-file
!
      IUER = -1
      CALL READ_HEB ( SPD%CONF%FIL_OH, HEB_OH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5211, -2, 'THERM_EXP_MAIN', 'Failure in an '// &
     &         'attempt to read the HEB file with ortho-heights '// &
     &          SPD%CONF%FIL_OH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_COMP_PPWTEM ( 1, HEB_DELP, HEB_TEM, HEB_Q, HEB_OH, MALO, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, -2, 'THERM_EXP_MAIN', 'Failure in an '// &
     &         'attempt to expand into B-spline basis 3D fields of '// &
     &         'total atmospheric pressure, partial pressure of water '// &
     &         'vapour, and air temperature' )
           CALL EXIT ( 1 )
      END IF
!
      IH = LINDEX ( FIL_MET, '.heb' )
      DATE_FIL = FIL_MET(IH-13:IH-10)//'.'//FIL_MET(IH-9:IH-8)//'.'// &
     &           FIL_MET(IH-7:IH-3)//':'//FIL_MET(IH-2:IH-1)//':00.0'
      IUER = -1
      CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, SEC_FIL, IUER )
      SEC_FIL = SEC_FIL + SPD%CONF%LAG 
      DATE_FIL = MJDSEC_TO_DATE ( MJD_FIL, SEC_FIL, IUER )
      FILOUT = OUT_PREF(1:I_LEN(OUT_PREF))// &
               DATE_FIL(1:4)//DATE_FIL(6:7)//DATE_FIL(9:10)//'_'// &
     &         DATE_FIL(12:13)//DATE_FIL(15:16)//'.eph'
!
      IUER = -1
      CALL THERM_EXP ( SPD, ANTI, MALO, IVRB, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, -2, 'THERM_EXP_MAIN', 'Failure in an '// &
     &         'attempt to compute site displacements due to thermal '// &
     &         'expansion' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Site displacements '// &
     &     'are written in '//FILOUT(1:I_LEN(FILOUT))
!
      END  PROGRAM  THERM_EXP_MAIN  !#!  
