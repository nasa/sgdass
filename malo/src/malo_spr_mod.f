      PROGRAM    MALO_SPR_MOD
! ************************************************************************
! *                                                                      *
! *   Progranm  MALO_SPR_MOD
! *                                                                      *
! * ### 19-OCT-2012  MALO_SPR_MOD  v1.0 (c)  L. Petrov 19-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO, MALO_AVR
      INTEGER*4  DAY_TAB(48)
      DATA DAY_TAB / &
     &    31,  29,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & !  1 - 12
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & ! 13 - 24
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & ! 25 - 36
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31   & ! 37 - 48
     &             /
      CHARACTER  DIR_IN*128, DATE_BEG*21, DATE_END*21, DATE_FIL*21, FIL_OUT*128, &
     &           STR*128, YEAR_STR*4, FIL_NC(MALO__FIL)*128, FILNAM*128
      INTEGER*8  DIR_DESC(32)
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 70.0 ) 
      INTEGER*4  J1, J2, J3, J4, MODE, MJD_BEG, MJD_END, MJD_FIL, LEV, L_FIL, &
     &           IP, IS, NB, NE, IVRB, N_EPC, IUER
      REAL*8     UTC_BEG, UTC_END, UTC_FIL, TAI_BEG, TAI_END, TAI_FIL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX
!
! --- Get arguments
!
      IF ( IARGC() .LT. 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_spr_mod mode in_directory date_beg date_end '// &
     &                        'output_file [verbosity_level]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, STR      )
           CALL CHIN   ( STR, MODE   ) 
           IF ( MODE < 1  .OR. MODE > 1 ) THEN
                CALL ERR_LOG ( 5201, -2, 'MALO_SPR_MOD', 'Unsupported mode '// &
     &               STR(1:I_LEN(STR))//' while an integer in range [1,1] '// &
     &               'was expected' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, DIR_IN   )
           CALL GETARG ( 3, DATE_BEG )
           CALL GETARG ( 4, DATE_END )
           CALL GETARG ( 5, FIL_OUT  )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR    )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB .LT. 1 ) IVRB = 0
                IF ( IVRB .GT. 9 ) IVRB = 9
              ELSE 
                IVRB = 0
           END IF
      END IF
      IUER = -1
      CALL DATE_TO_TIME  ( DATE_BEG, MJD_BEG, UTC_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5202, -2, 'MALO_SPR_MOD', 'Wrong 3rd argument '// &
     &          DATE_BEG(1:I_LEN(DATE_BEG))//' a calendar date was expected' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME  ( DATE_END, MJD_END, UTC_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5203, -2, 'MALO_SPR_MOD', 'Wrong 4th argument '// &
     &          DATE_BEG(1:I_LEN(DATE_BEG))//' a calendar date was expected' )
           CALL EXIT ( 1 )
      END IF
!
      L_FIL = 0
      LEV   = 0
      DO 410 J1=1,1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_IN, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 5204, -2, 'MALO_SPR_MOD', 'Error in '// &
     &           'reading input directory '//DIR_IN(1:I_LEN(DIR_IN))// &
     &           '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, '.nc' ) .LE. 0 ) GOTO 410
         L_FIL = L_FIL + 1
         IF ( L_FIL > MALO__FIL )  THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MALO__FIL, STR )
              CALL ERR_LOG ( 5205, -2, 'MALO_SPR_MOD', 'Too many files '// &
     &            'in directory '//DIR_IN(1:I_LEN(DIR_IN))// &
     &            ' -- more than '//STR )
              CALL EXIT ( 1 )
         END IF
         FIL_NC(L_FIL) = FILNAM 
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 5206, -2, 'MALO_SPR_MOD', 'No files with extension '// &
     &         '.nc were found in the input directory '//DIR_IN )
           CALL EXIT ( 1 )
      END IF
      CALL SORT_CH ( L_FIL, FIL_NC )
!
! --- Initialization
!
      IUER = -1
      CALL MALO_INIT ( MALO, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL MALO_INIT ( MALO_AVR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL MALO_UTC_TO_TAI ( MALO, MJD_BEG, UTC_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5207, -2, 'MALO_SPR_MOD', 'Error in an attempt '// &
     &         'to transform begin date to TAI' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_UTC_TO_TAI ( MALO, MJD_END, UTC_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5208, -2, 'MALO_SPR_MOD', 'Error in an attempt '// &
     &         'to transform end date to TAI' )
           CALL EXIT ( 1 )
      END IF
!
      NB = 0
      NE = 0
      DO 420 J2=1,L_FIL
         IP = LINDEX ( FIL_NC(J2), '_' )
         IF ( IP > 1 ) THEN
              IP = LINDEX ( FIL_NC(J2)(1:IP-1), '_' )
         END IF
         IF ( IP > 1 ) THEN
              DATE_FIL = FIL_NC(J2)(IP+1:IP+7)//'_01'
!
              IUER = -1
              CALL DATE_TO_TIME  ( DATE_FIL, MJD_FIL, UTC_FIL, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5209, -2, 'MALO_SPR_MOD', 'Failure in '// &
     &                 'an attempt to transform date '// &
     &                  DATE_BEG(1:I_LEN(DATE_BEG))//' embedded in the file '// &
     &                 'name '//FIL_NC(J2) )
                  CALL EXIT ( 1 )
             END IF
!
             IUER = -1
             CALL MALO_UTC_TO_TAI ( MALO, MJD_FIL, UTC_FIL, TAI_FIL, IUER )
             IF ( IUER .NE. 0 ) THEN
                  CALL ERR_LOG ( 5210, -2, 'MALO_SPR_MOD', 'Error in an attempt '// &
     &                'to transform the UTC time tag to TAI' )
                 CALL EXIT ( 1 )
             END IF
!!
             IF ( (MJD_FIL*86400.0D0 + TAI_FIL) - (MJD_BEG*86400.0D0 + TAI_BEG) .GE. -TIM_EPS .AND. &
     &            (MJD_FIL*86400.0D0 + TAI_FIL) - (MJD_END*86400.0D0 + TAI_END) .LE.  TIM_EPS       ) THEN
                  IF ( NB == 0 ) NB = J2
                  NE = J2
             END IF
         END IF
 420  CONTINUE 
      IF ( NB == 0 ) THEN
           CALL ERR_LOG ( 5211, -2, 'MALO_SPR_MOD', 'No input files that '// &
     &         'satisfy the specified data range were found in the input '// &
     &         'directory '//DIR_IN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL COMP_SPR_MODEL ( MODE, NE-NB+1, FIL_NC(NB), MALO, MALO_AVR, &
     &                      N_EPC, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, -2, 'MALO_SPR_MOD', 'Failure in an attempt '// &
     &         'to compute parameters of the model for the surfase pressure' )
           CALL EXIT ( 1 )
      END IF
!
      MALO_AVR%DATA_TYPE = 'Mean_Pressure_NCEP_Reanalysis_96'
      MALO_AVR%N_ACC_TIM = N_EPC
!
      IUER = -1
      CALL SPR_WRITE_NC ( MALO_AVR, FIL_OUT, MALO_AVR%MJD_BEG, MALO_AVR%UTC_BEG, &
     &                    MALO_AVR%MJD_END, MALO_AVR%UTC_END, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, -3, 'MALO_SPR_MOD', 'Failure in an attempt '// &
     &         'to write the output netCDF file' )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file '//FIL_OUT(1:I_LEN(FIL_OUT))
!
      END  PROGRAM  MALO_SPR_MOD  !#!#
