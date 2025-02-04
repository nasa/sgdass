!
! >>>>> Include block for external decimation 
! >>>>> 2007.10.24 (c)  L. Petrov  v 0.4  2007.10.26_14:47:22
!
	INTEGER*4    DCM__M_OBJ
	PARAMETER  ( DCM__M_OBJ = 8192 )
	CHARACTER    DCM__SOU*3, DCM__STA*3, DCM__BAS*3, DCM__ALL*3
	PARAMETER  ( DCM__SOU = 'SOU' )
	PARAMETER  ( DCM__STA = 'STA' )
	PARAMETER  ( DCM__BAS = 'BAS' )
	PARAMETER  ( DCM__ALL = 'ALL' )
	INTEGER*4    DCM__M_PAR
	PARAMETER  ( DCM__M_PAR = 11 )
	TYPE DCM__TYPE
	     CHARACTER  PRC_NAME*32
	     CHARACTER  OBJECT*3
	     CHARACTER  FILLER*1
	     CHARACTER  FILIN_LIST*128
	     CHARACTER  OUTDIR*128
	     CHARACTER  FIL_INC*128
	     CHARACTER  FIL_EXC*128
	     LOGICAL*4  SESS_RESET
	     INTEGER*4  SELECT_DCM
	     INTEGER*4  TOTAL_DCM
	     INTEGER*4  EDC_TYP
	     INTEGER*4  EDC_PAR
	     INTEGER*4  L_INC
	     INTEGER*4  L_EXC
             INTEGER*4  L_OBJ
	     CHARACTER  NAM_INC(DCM__M_OBJ)*16
	     CHARACTER  NAM_EXC(DCM__M_OBJ)*16
	     CHARACTER  NAM_OBJ(DCM__M_OBJ)*16
        END TYPE DCM__TYPE
	CHARACTER    DCM__CNF_LABEL*65
        PARAMETER  ( DCM__CNF_LABEL = '# DCM Decimation configuration file. Format version of 2007.10.26' )
!
	INTEGER*4  EDC__M_STA
	INTEGER*4  EDC__M_SOU
	PARAMETER  ( EDC__M_STA = 32   )
	PARAMETER  ( EDC__M_SOU = 2048 )
	TYPE EDC_HEA__TYPE
	    CHARACTER  DB_NAME*16  !  Database name
	    CHARACTER  PRC_NAME*32 !  Name of the decimation procedure
	    INTEGER*4  N_OBS       !  Total number of observations
	    INTEGER*4  N_SCA       !  Number of scans
	    INTEGER*4  N_STA       !  Number of stations
	    INTEGER*4  N_SOU       !  Number of sources
	    INTEGER*4  MJD_CRE     !  MJD of midnight of file creation date
	    INTEGER*4  MJD_SES     !  MJD of midnight of nominal session start
	    REAL*8     TIM_CRE     !  Local time of file creation date
	    REAL*8     TAI_SES     !  TAI of nominal session start
        END TYPE EDC_HEA__TYPE 
!
	TYPE EDC_OBS__TYPE
	    REAL*8     TIM_OBS     ! Observation time with resepct to the nominal 
!                                  ! session start in seconds
	    INTEGER*2  IND_STA(2)  ! Station indexces
	    INTEGER*2  IND_SOU     ! Source index
	    INTEGER*1  SUP_STS     ! Suprression status
	    INTEGER*1  DCM_STS     ! Decimation status
	END TYPE EDC_OBS__TYPE
!
	TYPE EDC__TYPE
	     TYPE ( EDC_HEA__TYPE ) HEA
	     CHARACTER*8, POINTER :: C_STA(:)
	     CHARACTER*8, POINTER :: C_SOU(:)
	     TYPE ( EDC_OBS__TYPE ), POINTER :: OBS(:)
	     CHARACTER  EDC_FILE*128
	END TYPE EDC__TYPE
!	
	CHARACTER     EDC__BF_LABEL*36, EDC__AF_LABEL*36
	CHARACTER     EDC__LE*2, EDC__BE*2
	PARAMETER  (  EDC__LE = 'LE' ) ! Little Endian
	PARAMETER  (  EDC__BE = 'BE' ) ! Big Endian
        PARAMETER  ( EDC__BF_LABEL = 'EDC Binary Format of 2007.10.25  xx ' )
        PARAMETER  ( EDC__AF_LABEL = 'EDC Ascii  Format of 2007.10.25     ' )
!
! >>>>> End of include block for external decimation 
!
