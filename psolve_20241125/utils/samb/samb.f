      PROGRAM    SAMB_MAIN
! ************************************************************************
! *                                                                      *
! *   Program SAMB
! *                                                                      *
! *  ### 07-FEB-2010      SAMB     v1.1 (c)  L. Petrov  29-NOV-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  PIMA_CNT*128, RES_FILE*128, OUTPUT_FILE*128, &
     &           STR*128, STR1*128, OPT_STR*128, FILTER_RULE*128, VERS_STR*54
      REAL*8     WIN_SEMI_WIDTH, SNR_MIN, WIN_MIN, WIN_MAX
      PARAMETER  ( WIN_MIN = 0.001D0 ) 
      PARAMETER  ( WIN_MAX = 1.D8 )
      LOGICAL*4  LEX
      INTEGER*4  J1, NOBS_REFRI, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INCLUDE 'samb_version.i' ! Set revision date of the current version
!
! --- Intialization
!
      CALL GET_VERSION ( VERS_STR )
!
      CALL CLRCH ( PIMA_CNT    )
      CALL CLRCH ( RES_FILE    )
      CALL CLRCH ( OUTPUT_FILE )
      WIN_SEMI_WIDTH = -1.0
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: samb -p {pima_control_file} '// &
     &                        '-w {window_semi_width_in_nsec} -s {snr_min) '// &
     &                        '-r {residual_file} -o {output_file} -f filter_rule'
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,6
         CALL CLRCH  ( OPT_STR )
         CALL GETARG ( (J1-1)*2+1, OPT_STR )
         IF ( ILEN(OPT_STR) == 0 ) THEN
              CALL ERR_LOG ( 4401, -2, 'SAMB', 'Not all options were supplied' )
              CALL EXIT ( 1 )
         END IF
!
         CALL GETARG ( (J1-1)*2+2,     STR )
         IF ( ILEN(STR) == 0 ) THEN
              CALL ERR_LOG ( 4402, -2, 'SAMB', 'No value for option '// &
     &             OPT_STR(1:I_LEN(OPT_STR))//' was supplied' )
              CALL EXIT ( 1 )
         END IF
         IF ( OPT_STR == '-p' ) THEN
              PIMA_CNT = STR
              INQUIRE ( FILE=PIMA_CNT, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 4403, -2, 'SAMB', 'Cannot find pima '// &
     &                 'control file '//PIMA_CNT(1:I_LEN(PIMA_CNT)) )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( OPT_STR == '-w' ) THEN
              READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IUER ) WIN_SEMI_WIDTH
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4404, -2, 'SAMB', 'Error in decoding '// &
     &                 'window width '//STR )
                   CALL EXIT ( 1 )
              END IF
              IF ( WIN_SEMI_WIDTH < WIN_MIN ) THEN
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1, FMT='(F10.5)' ) WIN_MIN
                   CALL ERR_LOG ( 4405, -2, 'SAMB', 'Parameter '// &
     &                 'window_in_nsec is too small: '//STR(1:I_LEN(STR))// &
     &                 ' -- less than '//STR1 )
                   CALL EXIT ( 1 )
              END IF
              IF ( WIN_SEMI_WIDTH > WIN_MAX ) THEN
                   CALL CLRCH ( STR1 )
                   WRITE ( UNIT=STR1, FMT='(F10.5)' ) WIN_MAX
                   CALL ERR_LOG ( 4406, -2, 'SAMB', 'Parameter '// &
     &                 'window_in_nsec is too big: '//STR(1:I_LEN(STR))// &
     &                 ' -- less than '//STR1 )
                   CALL EXIT ( 1 )
              END IF
              WIN_SEMI_WIDTH = WIN_SEMI_WIDTH*1.D-9
            ELSE IF ( OPT_STR == '-s' ) THEN
              READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IUER ) SNR_MIN
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4404, -2, 'SAMB', 'Error in decoding '// &
     &                 'minimum SNR '//STR )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( OPT_STR == '-r' ) THEN
              RES_FILE = STR
              INQUIRE ( FILE=RES_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 4407, -2, 'SAMB', 'Cannot find Solve '// &
     &                 'resodual file '//RES_FILE ) 
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( OPT_STR == '-o' ) THEN
              OUTPUT_FILE = STR 
            ELSE IF ( OPT_STR == '-f' ) THEN
              FILTER_RULE = STR 
            ELSE 
              CALL ERR_LOG ( 4408, -2, 'SAMB', 'Unsupported option '// &
     &             OPT_STR )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE 
!
      WRITE ( 6, * ) 'SAMB version of 2022.11.29'
      WRITE ( 6, * ) ' '
      WRITE ( 6, * ) 'PIMA_CNT:       ', PIMA_CNT(1:I_LEN(PIMA_CNT))
      WRITE ( 6, * ) 'WIN_SEMI_WIDTH: ', WIN_SEMI_WIDTH
      WRITE ( 6, * ) 'SNR_MIN:        ', SNR_MIN
      WRITE ( 6, * ) 'RES_FILE:       ', RES_FILE(1:I_LEN(RES_FILE))
      WRITE ( 6, * ) 'OUTPUT_FILE:    ', OUTPUT_FILE(1:I_LEN(OUTPUT_FILE))
      WRITE ( 6, * ) 'FILTER_RULE:    ', FILTER_RULE(1:I_LEN(FILTER_RULE))
!
      IUER = -1
      CALL SAMB_DO ( PIMA_CNT, RES_FILE, WIN_SEMI_WIDTH, SNR_MIN, NOBS_REFRI, &
     &               FILTER_RULE, OUTPUT_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4404, -2, 'SAMB', 'Error in an attempt to '// &
     &         'create re-fringing control file' )
           CALL EXIT ( 1 )
      END IF
      IF ( NOBS_REFRI == 0 ) THEN
           WRITE ( 6, '(A)' ) 'No observations eligible for refringing '// &
     &                        'were found'
      END IF
      WRITE ( 6, '(I6,A)' ) NOBS_REFRI, ' observations are to be re-fringed'
      WRITE ( 6, '(A)' ) 'Output C-shell command file '// &
     &                    OUTPUT_FILE(1:I_LEN(OUTPUT_FILE))
!
      END  PROGRAM  SAMB_MAIN  !#!  
