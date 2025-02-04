      SUBROUTINE GEN_MAPTXT ( MAP, FILIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_MAPTXT 
! *                                                                      *
! *  ### 10-SEP-2016  GEN_MAPTXT   v1.0 (c)  L. Petrov  10-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      CHARACTER  FILIN*(*), FILOUT*(*)
      INTEGER*4  IUER
      CHARACTER  MAPTXT__LABEL*38
      PARAMETER  ( MAPTXT__LABEL = '# MAP_TEXT  Format version  2016.09.10' )
      CHARACTER  STR*128
      TYPE TIMEB__TYPE
           INTEGER*4  TIME
           INTEGER*2  MILLITM
           INTEGER*2  TIMEZONE
           INTEGER*2  DSTFLAG
      END TYPE TIMEB__TYPE
!
      TYPE TIME_TM__TYPE
           INTEGER*4  TM_SEC    ! /* seconds */
           INTEGER*4  TM_MIN    ! /* minutes */
           INTEGER*4  TM_HOUR   ! /* hours */
           INTEGER*4  TM_MDAY   ! /* day of the month */
           INTEGER*4  TM_MON    ! /* month */
           INTEGER*4  TM_YEAR   ! /* year */
           INTEGER*4  TM_WDAY   ! /* day of the week */
           INTEGER*4  TM_YDAY   ! /* day in the year */
           INTEGER*4  TM_ISDST  ! /* daylight saving time */
      END TYPE TIME_TM__TYPE
      TYPE ( TIMEB__TYPE   ) :: TIM_B
      TYPE ( TIME_TM__TYPE ) :: TIM_TM
      INTEGER*4  J1, IS, UNIX_DATE,  LUN, IER
      INTEGER*8  SIZE_I8
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, GET_UNIT
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN 
           CALL ERR_LOG ( 1481, IUER, 'GEN_MAPTXT', 'Error in an attempt '// &
     &         'top open the output ilfe '//FILOUT )
           RETURN 
      END IF
!      
      CALL CLRCH ( STR )
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
      CALL NOUT ( SIZEOF(TIM_B), TIM_B )
      TIM_B%TIME = UNIX_DATE
      CALL LOCALTIME_R ( TIM_B, TIM_TM )
      WRITE ( UNIT=STR, FMT=110 ) TIM_TM%TM_YEAR + 1900, &
     &             TIM_TM%TM_MON + 1, &
     &             TIM_TM%TM_MDAY, &
     &             TIM_TM%TM_HOUR, &
     &             TIM_TM%TM_MIN, &
     &             TIM_TM%TM_SEC, &
     &             TIM_B%MILLITM
 110  FORMAT ( I4, '.', I2, '.', I2, '_', I2, ':', I2, &
                            ':', I2, '.', I3 )
      CALL BLANK_TO_ZERO ( STR(1:23) )
!
      WRITE ( LUN, FMT='(A)' ) MAPTXT__LABEL
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '# Generated on '//GET_CDATE()
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '# Input file: '//TRIM(FILIN)
      WRITE ( LUN, FMT='(A)' ) '# Input file last modification date: '//STR(1:I_LEN(STR))
      WRITE ( LUN, FMT='(A)' ) '#'
!
      WRITE ( LUN, FMT='(A,A)' ) 'SOU_NAME= ', MAP%SOU_NAME
      WRITE ( LUN, FMT='(A,A)' ) 'EXP_NAME= ', MAP%EXP_NAME
      WRITE ( LUN, FMT='(A,A)' ) 'DATE_OBS= ', MAP%DATE_OBS
      WRITE ( LUN, FMT='(A,I5)'   ) 'MJD= ', MAP%MJD
      WRITE ( LUN, FMT='(A,F7.1)' ) 'TAI= ', MAP%TAI
      WRITE ( LUN, FMT='(A,I6)'   ) 'DIM1= ', MAP%DIM1
      WRITE ( LUN, FMT='(A,I6)'   ) 'DIM2= ', MAP%DIM2
      WRITE ( LUN, FMT='(A,I6)'   ) 'NUM_CC= ', MAP%NUM_CC
      WRITE ( LUN, FMT='(A,I6)'   ) 'NUM_SEG= ', MAP%NUM_SEG
      WRITE ( LUN, FMT='(A,I1)'   ) 'NSTK= ', MAP%NSTK
      WRITE ( LUN, FMT='(A,F15.12,A)' ) 'ALPHA= ', MAP%ALPHA, ' rad'
      WRITE ( LUN, FMT='(A,F15.12,A)' ) 'DELTA= ', MAP%DELTA, ' rad'
      WRITE ( LUN, FMT='(A,1PD12.5,A,0PF8.4,A)' ) 'STEP_RA= ', MAP%STEP_RA, ' rad ', &
     &                                                        MAP%STEP_RA*RAD__TO__MAS, 'mas'
      WRITE ( LUN, FMT='(A,1PD12.5,A,0PF8.4,A)' ) 'STEP_DL= ', MAP%STEP_DL, ' rad ', &
     &                                                        MAP%STEP_DL*RAD__TO__MAS, 'mas'
      WRITE ( LUN, FMT='(A,F9.4,A)' ) 'FREQ= ', MAP%FREQ*1.D-9, ' GHz'
      WRITE ( LUN, FMT='(A,F10.5,A)' ) 'FLUX_MAX= ', MAP%FLUX_MAX, 'Jy'
      WRITE ( LUN, FMT='(A,F10.5,A)' ) 'FLUX_INT= ', MAP%FLUX_INT, 'Jy'
      WRITE ( LUN, FMT='(A,F10.5,A)' ) 'FLUX_SHR= ', MAP%FLUX_SHR, 'Jy'
      WRITE ( LUN, FMT='(A,F10.5,A)' ) 'FLUX_UNR= ', MAP%FLUX_UNR, 'Jy'
      WRITE ( LUN, FMT='(A,1PD12.5,A,0PF8.4,A)' ) 'BEAM_MAJ= ', MAP%BEAM_MAJ, ' rad ', &
     &                                                         MAP%BEAM_MAJ*RAD__TO__MAS, ' mas'
      WRITE ( LUN, FMT='(A,1PD12.5,A,0PF8.4,A)' ) 'BEAM_MIN= ', MAP%BEAM_MIN, ' rad ', &
     &                                                         MAP%BEAM_MIN*RAD__TO__MAS, ' mas'
      WRITE ( LUN, FMT='(A,1PD12.5,A,0PF8.4,A)' ) 'BEAM_POS_ANG= ', MAP%BEAM_POS_ANG, ' rad ', &
     &                                                             MAP%BEAM_POS_ANG/DEG__TO__RAD, ' deg'
      WRITE ( LUN, FMT='(A,1PD12.5,A)' ) 'NOISE= ', MAP%NOISE, ' Jy '
      DO 410 J1=1,MAP%NUM_CC
         WRITE ( UNIT=LUN, FMT=120 ) J1, MAP%FLUX_CC(J1), MAP%COOR_CC(1,J1), MAP%COOR_CC(2,J1), &
     &                               MAP%COOR_CC(1,J1)*RAD__TO__MAS, MAP%COOR_CC(2,J1)*RAD__TO__MAS
 120     FORMAT ( 'Clean_component ind= ', I6, ' Flux= ', F10.7, &
     &            ' Jy  u_coo= ', 1PD12.5, ' rad, v_coo= ', 1PD12.5, ' rad; ', &
     &            ' u_coo= ', 0PF9.3, ' mas, v_coo= ', 0PF9.3, ' mas' )
 410  CONTINUE 
      CLOSE ( UNIT=LUN )
!     
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GEN_MAPTXT  !#!#
