!
! >>>>> Include block with antenna infromation.
! >>>>> Used for computing position variations caused by thermal expansion
! >>>>> 2008.04.22  (c)  L. Petrov  v 1.0  2008.04.24_10:14:39
!
      CHARACTER  ANTI__LABEL*56
      PARAMETER  ( ANTI__LABEL = '$ Antenna Information File  Format Version of 2008.04.22' )
!
      TYPE ANTENNA_INFO__TYPE
	   CHARACTER  NAME*8
	   INTEGER*4  FOCUS_TYPE
	   INTEGER*4  MOUNTING_TYPE
	   INTEGER*4  RADOME_HAS
	   INTEGER*4  MEAS_TYPE
	   REAL*8     REF_TEMP      ! Degrees Kelvin
	   REAL*8     ANN_SIN_TEMP  ! Degrees Kelvin
	   REAL*8     ANN_COS_TEMP  ! Degrees Kelvin
	   REAL*8     REF_PRES      ! Pascal
	   REAL*8     DIAMETER
	   REAL*8     FOUNDATION_HEIGHT
	   REAL*8     FOUNDATION_DEPTH
	   REAL*8     FOUNDATION_TE
	   REAL*8     PILLAR_HEIGHT
	   REAL*8     PILLAR_TE
	   REAL*8     AXOF_LEN
	   REAL*8     AXOF_TE
	   REAL*8     VERTEX_LEN
	   REAL*8     VERTEX_TE
	   REAL*8     SUBREFL_HEIGHT
	   REAL*8     SUBREFL_TE
      END TYPE ANTENNA_INFO__TYPE
!
      TYPE ANTENNA_DATA__TYPE
           INTEGER*4  N_ANT
           TYPE ( ANTENNA_INFO__TYPE ), POINTER :: INFO(:)
	   CHARACTER, POINTER ::  STA_NAM(:)*8
!
           INTEGER*4  N_AHM                        ! The number of site with in-situ height measurements
	   CHARACTER, POINTER ::  AHM_STA_NAM(:)*8 ! The list of stations with in-situ height measurements
	   CHARACTER, POINTER ::  AHM_FIL(:)*128   ! The list of file names wwith in-situ height measurements
!
           CHARACTER  FILENAME*128
           INTEGER*4  STATUS
      END  TYPE ANTENNA_DATA__TYPE
!
      INTEGER*4  ANTI_V1__NFL
      PARAMETER  ( ANTI_V1__NFL = 22 )      
      INTEGER*4  ANTI_V1__FLD(2,ANTI_V1__NFL)
      DATA       ANTI_V1__FLD &
     &         /  &
!
     &            1,  12, & ! ( 1) Record label: word ANTENNA_INFO
     &           15,  22, & ! ( 2) IVS station name
     &           25,  31, & ! ( 3) Focus type: FO_PRIM -- primary, FO_SECN -- secondary
     &           33,  39, & ! ( 4) Mounting type: MO_AZEL (azimuthal), MO_EQUA (equatorial),
     &           41,  47, & ! ( 5) Flag, whether the station has radome: RA_NO or RA_YES
     &           49,  55, & ! ( 6) Measurement type ME_COMP (complete) ME_INCM (incomplete) 
     &           58,  61, & ! ( 7) Reference temperature (degree C)
     &           63,  66, & ! ( 8) sin amplitude of annual temperature variations with respect 
     &           68,  71, & ! ( 9) cos amplitude of annual temperature variations with respect 
     &           73,  78, & ! (10) Reference pressure (hPa)
     &           81,  85, & ! (11) Antrenna diameter (m)
     &           87,  93, & ! (12) Height of foundation (m)
     &           95, 100, & ! (13) Depth of foundation (m)
     &          103, 109, & ! (14) Foundation thermal expansion coefficient (1/K)
     &          112, 118, & ! (15) Height of the antenna pillar (m)
     &          120, 126, & ! (16) Antenna pillar thermal expansion coefficient (1/K)
     &          129, 135, & ! (17) Length of the axis offset (m)
     &          137, 143, & ! (18) Axis offset thermal expansion coefficient (1/K)
     &          146, 152, & ! (19) Distance from the movable axis to the antenna vertex (m)
     &          154, 160, & ! (20) Thermal expansion coefficient of the structure from 
     &          163, 169, & ! (21) Height of the sub-reflector above the vertex (m)
     &          171, 177  & ! (22) Sub-reflector mounting legs thermal expansion coefficient (1/K)
!
               /
!
	INTEGER*4    ANTI__FO_PRIM, ANTI__FO_SECN
	CHARACTER    ANTI__FO_PRIM_CH*7, ANTI__FO_SECN_CH*7
	PARAMETER  ( ANTI__FO_PRIM = 14001 )
	PARAMETER  ( ANTI__FO_SECN = 14002 )
	PARAMETER  ( ANTI__FO_PRIM_CH = 'FO_PRIM' )
	PARAMETER  ( ANTI__FO_SECN_CH = 'FO_SECN' )
	REAL*8       ANTI__FO_ADM
	PARAMETER  ( ANTI__FO_ADM = 0.9 ) !   Admittance of the focal length change
!
        INTEGER*4    ANTI__MO_AZEL, ANTI__MO_EQUA, ANTI__MO_XYNO, &
       &             ANTI__MO_XYEA, ANTI__MO_RICH 
        CHARACTER    ANTI__MO_AZEL_CH*7, ANTI__MO_EQUA_CH*7, &
                     ANTI__MO_XYNO_CH*7, ANTI__MO_XYEA_CH*7, &
       &             ANTI__MO_RICH_CH*7
	PARAMETER  ( ANTI__MO_AZEL = 1 )
	PARAMETER  ( ANTI__MO_EQUA = 2 )
	PARAMETER  ( ANTI__MO_XYNO = 3 )
	PARAMETER  ( ANTI__MO_XYEA = 4 )
	PARAMETER  ( ANTI__MO_RICH = 5 )
	PARAMETER  ( ANTI__MO_AZEL_CH = 'MO_AZEL' )
	PARAMETER  ( ANTI__MO_EQUA_CH = 'MO_EQUA' )
	PARAMETER  ( ANTI__MO_XYNO_CH = 'MO_XYNO' )
	PARAMETER  ( ANTI__MO_XYEA_CH = 'MO_XYEA' )
	PARAMETER  ( ANTI__MO_RICH_CH = 'MO_RICH' )
!
	INTEGER*4    ANTI__RA_NO,      ANTI__RA_YES
	CHARACTER    ANTI__RA_NO_CH*6, ANTI__RA_YES_CH*6
	PARAMETER  ( ANTI__RA_NO  = 15001 )
	PARAMETER  ( ANTI__RA_YES = 15002 )
	PARAMETER  ( ANTI__RA_NO_CH  = 'RA_NO ' )
	PARAMETER  ( ANTI__RA_YES_CH = 'RA_YES' )
!
	INTEGER*4  ANTI__ME_COMP, ANTI__ME_INCM, ANTI__ME_ROUG 
	CHARACTER  ANTI__ME_COMP_CH*7, ANTI__ME_INCM_CH*7, ANTI__ME_ROUG_CH*7
	PARAMETER  ( ANTI__ME_COMP = 16001 )
	PARAMETER  ( ANTI__ME_INCM = 16002 )
        PARAMETER  ( ANTI__ME_ROUG = 16003 )
	PARAMETER  ( ANTI__ME_COMP_CH = 'ME_COMP' )
	PARAMETER  ( ANTI__ME_INCM_CH = 'ME_INCM' )
        PARAMETER  ( ANTI__ME_ROUG_CH = 'ME_ROUG' )
!
	INTEGER*4  ANTI__INST, ANTI__AVR, ANTI__LAG2H, ANTI__LAG6H
	PARAMETER  ( ANTI__INST  = 25001 )
	PARAMETER  ( ANTI__AVR   = 25002 )
	PARAMETER  ( ANTI__LAG2H = 25003 )
	PARAMETER  ( ANTI__LAG6H = 25004 )
!
	CHARACTER  ANTI__INST_CH*7, ANTI__AVR_CH*7, ANTI__LAG2H_CH*6, &
     &             ANTI__LAG6H_CH*6
	PARAMETER  ( ANTI__INST_CH  = 'INSTANT' )
	PARAMETER  ( ANTI__AVR_CH   = 'AVERAGE' )
	PARAMETER  ( ANTI__LAG2H_CH = 'LAG_2H'  )
	PARAMETER  ( ANTI__LAG6H_CH = 'LAG_6H'  )
!
	INTEGER*4  ANTI__UNDF, ANTI__LOAD
	PARAMETER  ( ANTI__UNDF =          0 ) ! Undefined
	PARAMETER  ( ANTI__LOAD = 1829927365 ) ! Loaded
!
! >>>>> End of include block with antenna infromation
!
