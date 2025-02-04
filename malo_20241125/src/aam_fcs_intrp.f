      PROGRAM    AAM_FCS_INTRP_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
!!      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      PARAMETER  ( STACK_SIZE_IN_BYTES = 1 * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL AAM_FCS_INTRP()
      END  PROGRAM  AAM_FCS_INTRP_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  AAM_FCS_INTRP()
! ************************************************************************
! *                                                                      *
! *   Program AAM_FCS_INTRP
! *                                                                      *
! * ### 09-AUG-2015  AAM_FCS_INTRP  v2.0 (c)  L. Petrov  04-DEC-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  MC
      PARAMETER  ( MC = 15 )
      CHARACTER  AAM_DIR*128, DATE_STR*24, FIL_AAM(MALO__FIL)*128, &
     &           DATE_REQ*32, EXT*4, DATA_SOURCE*128, FILOUT*128, &
     &           DATA_TITLE*128, OTP*16, STR*128, STR1*128, STR2*128
      REAL*8     TAI_BEG, TAI_END, TAI_REQ, UTC_REQ, TAI_MOM, TAI_ASM_END, &
     &           TIM_ARR(MALO__FIL), AAM_VAL(MALO__FIL,MC), &
     &           AAM_SPL(MALO__FIL,MC), TIM_ARG, AAM_RES(MC), FCS_AGE 
      REAL*8       EPS_TIM
      PARAMETER  ( EPS_TIM = 100.0D0 )
      CHARACTER    AFI__LABEL*36
      PARAMETER  ( AFI__LABEL = 'AAM_FCS_INTRP  Version of 2015.08.12' )
      INTEGER*4  MJD_BEG, MJD_END, MJD_REQ, MJD_ASM_END, MJD_MOM, L_EPC, &
     &           IX, IDAY, LUN, J1, J2, IUER
      INTEGER*4, EXTERNAL :: IXMN8, ILEN, I_LEN, GET_UNIT
      REAL*8,    EXTERNAL :: FSPL8
      CHARACTER, EXTERNAL :: GET_HR_UTC_CDATE*23, MJDSEC_TO_DATE*30, GET_CDATE*19
!      
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: amm_fcs_intrpl aam_dir date [output_type] [output_file]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, AAM_DIR  )
           CALL GETARG ( 2, DATE_REQ )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, OTP )
              ELSE
                OTP = 'tab'
           END IF
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, FILOUT )
              ELSE
                CALL CLRCH ( FILOUT )
           END IF
      END IF
      IF(  DATE_REQ == 'now' ) THEN
           DATE_REQ = GET_HR_UTC_CDATE()
!
           IUER = -1
           CALL MALO_INIT ( MALO, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5201, IUER, 'AAM_FCS_INTRP', 'Error in initialization '// &
     &              'of malo object' )
                CALL EXIT ( 1 )
           END IF
!
           MALO%LEAPSEC%FINAM_LEAPSEC = MALO_SHARE//'/malo_leapsec.dat'
           CALL MALO_LOAD_LEAPSEC ( MALO, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5202, IUER, 'AAM_FCS_INTRP', 'Error in an attempt '// &
     &              'to load leap second file' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_REQ, MJD_REQ, UTC_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5203, IUER, 'AAM_FCS_INTRP', 'Error in parsing '// &
     &              'the requested date '//DATE_REQ )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL MALO_UTC_TO_TAI ( MALO, MJD_REQ, UTC_REQ, TAI_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5204, IUER, 'AAM_FCS_INTRP', 'Error in '// &
     &              'computing TAI from UTC function '//DATE_REQ )
                CALL EXIT ( 1 )
           END IF
           DATE_REQ = MJDSEC_TO_DATE ( MJD_REQ, TAI_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5205, IUER, 'AAM_FCS_INTRP', 'Trap of internal '// &
     &              'control: error in computing TAI from UTC function' )
                CALL EXIT ( 1 )
           END IF
        ELSE
           IUER = -1
           CALL DATE_TO_TIME ( DATE_REQ, MJD_REQ, TAI_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5206, IUER, 'AAM_FCS_INTRP', 'Error in parsing '// &
     &              'the requested date '//DATE_REQ )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( OTP == 'tab' ) THEN
           CONTINUE 
         ELSE IF ( OTP == 'etab' ) THEN
           CONTINUE 
         ELSE IF ( OTP == 'ser'  ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 5207, IUER, 'AAM_FCS_INTRP', 'Unsupported output '// &
     &         'type '//OTP(1:I_LEN(OTP))//' one of tab, etab, ser were '// &
     &         'expected' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL AAM_FCS_SPL ( AAM_DIR, MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                   MJD_ASM_END, TAI_ASM_END, MALO__FIL, L_EPC, TIM_ARR, &
     &                   AAM_VAL, AAM_SPL, DATA_SOURCE, DATA_TITLE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5208, IUER, 'AAM_FCS_INTRP', 'Error in an attempt'// &
     &         ' to compute interpolating spline for the AAM forecast' )
           CALL EXIT ( 1 )
      END IF
      IF ( OTP .EQ. 'tab' .OR. OTP == 'etab' ) THEN
           IF ( (MJD_REQ*86400.0D0 +  TAI_REQ) < (MJD_BEG*86400.0D0 + TAI_BEG) + EPS_TIM ) THEN
                STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
                IUER = -1
                CALL ERR_LOG ( 5209, IUER, 'AAM_FCS_INTRP', 'Requested date '// &
          &          DATE_REQ(1:24)//' is ealier than the first date with '// &
          &         'the AAM forecast '//STR(1:24) )
                CALL EXIT ( 1 )
           END IF
!
           IF ( (MJD_REQ*86400.0D0 +  TAI_REQ) > (MJD_END*86400.0D0 + TAI_END) - EPS_TIM ) THEN
                STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
                IUER = -1
                CALL ERR_LOG ( 5210, IUER, 'AAM_FCS_INTRP', 'Requested date '// &
          &          DATE_REQ(1:24)//' is later than the last date with '// &
          &         'the AAM forecast '//STR(1:24) )
                CALL EXIT ( 1 )
           END IF
           TIM_ARG = (MJD_REQ*86400.0D0 + TAI_REQ) - (MJD_BEG*86400.0D0 + TAI_BEG)
           IX = IXMN8 ( L_EPC, TIM_ARR, TIM_ARG )
!
           DO 410 J1=1,MC
              AAM_RES(J1) = FSPL8 ( TIM_ARG, L_EPC, TIM_ARR, AAM_VAL(1,J1), &
     &                              IX, AAM_SPL(1,J1) )
 410       CONTINUE 
!
           IF ( OTP == 'etab' ) THEN
                STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
                STR1 = MJDSEC_TO_DATE ( MJD_ASM_END, TAI_ASM_END, IUER )
                STR2 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
                FCS_AGE = ( (MJD_REQ*86400.0 + TAI_REQ) - &
     &                      (MJD_ASM_END*86400.0 + TAI_ASM_END) )/3600.0D0
                IF ( FCS_AGE < 0.0D0 ) FCS_AGE = 0.0
                WRITE ( 6, 110 ) AAM_TAB__LABEL, AFI__LABEL, GET_CDATE(), &
     &                           DATA_SOURCE(1:I_LEN(DATA_SOURCE)),        &
     &                           DATA_TITLE(1:I_LEN(DATA_TITLE)),          &
     &                           STR(1:23),      MJD_BEG,     TAI_BEG,     &
     &                           STR1(1:23),     MJD_ASM_END, TAI_ASM_END, &
     &                           STR2(1:23),     MJD_END,     TAI_END,     &
     &                           DATE_REQ(1:23), MJD_REQ,     TAI_REQ,     &
     &                           FCS_AGE, &
     &                           AAM_RES
 110            FORMAT (                   A/ &
     &                   'Generated_by: ', A/ &
     &                   'Generated_on: ', A/ &
     &                   'Data_source:  ', A/ &
     &                   'Data_title:   ', A/ &
     &                   'Assimilation_start_date:      ', A, ' MJD: ', I5, ' TAI: ', F6.0/ &
     &                   'Assimilation_end_date:        ', A, ' MJD: ', I5, ' TAI: ', F6.0/ &
     &                   'Forecast_end_date:            ', A, ' MJD: ', I5, ' TAI: ', F6.0/ &
     &                   'Request_date:                 ', A, ' MJD: ', I5, ' TAI: ', F6.0/ &
     &                   'Forecast_age:                 ', F5.1, ' hours'/&
     &                   'AAM_Mass_term_noIB_I13=       ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Mass_term_noIB_I23=       ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Mass_term_noIB_I33=       ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Mass_term_IB_I13=         ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Mass_term_IB_I23=         ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Mass_term_IB_I33=         ', 1PD12.5, ' kg/m^2'/     &
     &                   'AAM_Motion_term_H1=           ', 1PD12.5, ' kg/(s*m^2)'/ &
     &                   'AAM_Motion_term_H2=           ', 1PD12.5, ' kg/(s*m^2)'/ &
     &                   'AAM_Motion_term_H3=           ', 1PD12.5, ' kg/(s*m^2)'/ &
     &                   'Excitation_function_noIB_EF1= ', 1PD12.5, ' 1/s'/        &
     &                   'Excitation_function_noIB_EF2= ', 1PD12.5, ' 1/s'/        &
     &                   'Excitation_function_noIB_EF3= ', 1PD12.5, ' 1/s'/        &
     &                   'Excitation_function_IB_EF1=   ', 1PD12.5, ' 1/s'/        &
     &                   'Excitation_function_IB_EF2=   ', 1PD12.5, ' 1/s'/        &
     &                   'Excitation_function_IB_EF3=   ', 1PD12.5, ' 1/s'         )
             ELSE IF ( OTP == 'tab' ) THEN
                WRITE ( 6, 120 ) DATE_REQ(1:23), AAM_RES
 120            FORMAT ( 'Time: ', A, ' TAI  ', &
     &                   'I13_noIB= ', 1PD12.5, ' kg/m^2 ', &
     &                   'I23_noIB= ', 1PD12.5, ' kg/m^2 ', &
     &                   'I33_noIB= ', 1PD12.5, ' kg/m^2 ', &
     &                   'I13_IB=   ', 1PD12.5, ' kg/m^2 ', &
     &                   'I23_IB=   ', 1PD12.5, ' kg/m^2 ', &
     &                   'I33_IB=   ', 1PD12.5, ' kg/m^2 ', &
     &                   'H1=       ', 1PD12.5, ' kg/(s*m^2) ', &
     &                   'H2=       ', 1PD12.5, ' kg/(s*m^2) ', &
     &                   'H3=       ', 1PD12.5, ' kg/(s*m^2) ', &
     &                   'EF1_noIB= ', 1PD12.5, ' 1/s ', &
     &                   'EF2_noIB= ', 1PD12.5, ' 1/s ', &
     &                   'EF3_noIB= ', 1PD12.5, ' 1/s ', &
     &                   'EF1_IB=   ', 1PD12.5, ' 1/s ', &
     &                   'EF2_IB=   ', 1PD12.5, ' 1/s ', &
     &                   'EF3_IB=   ', 1PD12.5, ' 1/s' )
           END IF
         ELSE IF ( OTP == 'ser' ) THEN
           STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
           STR1 = MJDSEC_TO_DATE ( MJD_ASM_END, TAI_ASM_END, IUER )
           STR2 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
           IF ( ILEN(FILOUT) == 0 ) THEN
                LUN = 6
              ELSE
                LUN = GET_UNIT()
                OPEN ( FILE=FILOUT, UNIT=LUN, STATUS='unknown', IOSTAT=IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 5211, IUER, 'AAM_FCS_INTRP', 'Failure '// &
     &                   'in an attempt to open output file '//FILOUT )
                     CALL EXIT ( 1 )
                END IF
           END IF
!           
           WRITE ( LUN, '(A)' ) AAM_SER__LABEL
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# This table contains the atmospheric angular momentum,'
           WRITE ( LUN, '(A)' ) '# mass and motion term separately as well as the atmospheric'
           WRITE ( LUN, '(A)' ) '# excitation function of the Earth rotation.'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The atmospheric angular momentum was computed by integration'
           WRITE ( LUN, '(A)' ) '# of the 3D model of distribution of air density and wind speed'
           WRITE ( LUN, '(A)' ) '# derived from the output of the numerical weather model'
           WRITE ( LUN, '(A)' ) '# Mass term was computed using both an inverted barometer and '
           WRITE ( LUN, '(A)' ) '# non-inverted barometer hypotheses.'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The variant with the non-inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# means that the change of the sea level height induced '
           WRITE ( LUN, '(A)' ) '# by atmospheric pressure change was ignored'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The variant with the inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# means that the reaction of the sea level height induced'
           WRITE ( LUN, '(A)' ) '# by atmospheric pressure change complete counerbalances'
           WRITE ( LUN, '(A)' ) '# the atmospheric pressure and as a results the bottom pressure that'
           WRITE ( LUN, '(A)' ) '# is the some of the atmospheric pressure and the sea column pressure'
           WRITE ( LUN, '(A)' ) '# is constant. The mass term over the ocean was excluded from integration'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# Therefore, the mass term with the inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# is computed by integrarging only over the land. The mass term with the '
           WRITE ( LUN, '(A)' ) '# non-inverted barometer hypothesis invoked # is computed by integrarging'
           WRITE ( LUN, '(A)' ) '# over entire Earth that includes both land and sea.'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# Format:'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '#  Column  1:   1:19   A19    Calendar date'
           WRITE ( LUN, '(A)' ) '#  Column  2:  21:25   I5     Calendar date, MJD part. Units: day'
           WRITE ( LUN, '(A)' ) '#  Column  3:  27:32   F7.1   Calendar date, TAI part. Units: sec'
           WRITE ( LUN, '(A)' ) '#  Column  4:  36:48   E13.6  Mass term   of the angular momentum I31. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  5:  50:62   E13.6  Mass term   of the angular momentum I32. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  6:  64:76   E13.6  Mass term   of the angular momentum I33. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  4:  80:92   E13.6  Mass term   of the angular momentum I31.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  5:  94:106  E13.6  Mass term   of the angular momentum I32.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  6: 108:120  E13.6  Mass term   of the angular momentum I33.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  7: 124:136  E13.6  Motion term of the angular momentum H1.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column  8: 138:150  E13.6  Motion term of the angular momentum H2.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column  9: 152:164  E13.6  Motion term of the angular momentum H3.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column 10: 168:180  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 1. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 11: 182:194  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 2. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 12: 196:208  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 3. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 13: 212:224  E13.6  Atmospheric Earth rotation excitation function,   IB, component 1. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 14: 226:238  E13.6  Atmospheric Earth rotation excitation function,   IB, component 2. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 15: 240:252  E13.6  Atmospheric Earth rotation excitation function,   IB, component 3. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# Generated by: '//AFI__LABEL
           WRITE ( LUN, '(A)' ) '# Generated on: '//GET_CDATE()
           WRITE ( LUN, '(A)' ) '# Data source:  '//DATA_SOURCE(1:I_LEN(DATA_SOURCE))
           WRITE ( LUN, '(A)' ) '# Data title:   '//DATA_TITLE(1:I_LEN(DATA_TITLE))
           WRITE ( LUN,  130  ) '# Assimilation start date: ', STR(1:23),  MJD_BEG, TAI_BEG
           WRITE ( LUN,  130  ) '# Assimilation end date:   ', STR1(1:23), MJD_ASM_END, TAI_ASM_END
           WRITE ( LUN,  130  ) '# Forecast end date:       ', STR2(1:23), MJD_END,     TAI_END
 130       FORMAT ( A, A, ' MJD: ', I5, ' TAI: ', F6.0 )
           WRITE ( LUN,  140  ) '# Number of epochs:        ', L_EPC
 140       FORMAT ( A, I7 )
!
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
           WRITE ( LUN, '(A)' ) '#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
           WRITE ( LUN, '(A)' ) '#  TIME TAG                         AAM MASS TERM, non-IB                       AAM MASS TERM, non-IB                       AAM MOTION TERM                             EXCITATION FUNCTION, no-IB                  EXCITATION FUNCTION, IB                 '
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
           WRITE ( LUN, '(A)' ) '#  Date             MJD   TAI       I31_noIB      I32_noIB      I33_noIB        I31_IB        I32_IB        I33_IB          H1            H2            H3              EF1_noIB      EF2_noIB      EF3_noIB        EF1_IB        EF2_IB        EF3_IB      '
           WRITE ( LUN, '(A)' ) '#                                   kg/m^2        kg/m^2        kg/m^2          kg/m^2        kg/m^2        kg/m^2          kg/(s*m^2)    kg/(s*m^2)    kg/(s*m^2)      1/s           1/s           1/s             1/s           1/s           1/s         '
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
!
           DO 420 J2=1,L_EPC
              MJD_MOM = MJD_BEG
              TAI_MOM = TAI_BEG + TIM_ARR(J2)
              IDAY = TAI_MOM/86400.0D0
              MJD_MOM = MJD_MOM + IDAY
              TAI_MOM = TAI_MOM - IDAY*86400.0D0
              STR  = MJDSEC_TO_DATE ( MJD_MOM, TAI_MOM, IUER )
              WRITE ( LUN, 150 ) STR(1:19), MJD_MOM, TAI_MOM, AAM_VAL(J2,1:MC)
 150          FORMAT ( A, 1X, I5, 1X, F7.1, 1X, 5(3(1X,1PD13.6),2X) )
 420       CONTINUE 
!
           IF ( ILEN(FILOUT) > 0 ) THEN
                CLOSE ( UNIT=LUN )
           END IF
      END IF
!
      END  SUBROUTINE  AAM_FCS_INTRP  !#!#
