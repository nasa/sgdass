#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_OPAL ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_OPAL  interpolates atmosphere opacity and brightness *
! *   temperature from the results computed before by opag and loads     *
! *   them into internal structure of PIM object.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *       VTD ( VTD__TYP  ) -- Object with information related to        *
! *                            package VTD for computed apriori path     *
! *                            delay.                                    *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ###  17-SEP-2014   PIMA_OPAL  v1.2 (c)  L. Petrov  29-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER 
      TYPE     ( SPD__ASCII__TYPE ), POINTER :: SAT(:)
      TYPE     ( SPD_DEL__TYPE    ), POINTER :: SPD_RES(:)
      CHARACTER  SOB_DIR*128, STR*128, FINAM*128, DATE_BEG_STR*21, &
     &           DATE_END_STR*21
      INTEGER*8  DIR_DESC(16)
      CHARACTER, ALLOCATABLE :: C_FIL(:)*128
      INTEGER*4    M_FIL
      PARAMETER  ( M_FIL = 128 )
      REAL*4     EPS_MAR
      PARAMETER  ( EPS_MAR = 1.0E-5 )
      REAL*8     TIM_BEG_FIL, TIM_END_FIL, TAI_EPC, TIM_SCA(2), &
     &           TIM_STEP, PH_DEL, PH_RATE, AZ(2), EL(2), ELEV, UVW(3), &
     &           OPA_VAL, TAT_VAL, DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      REAL*4     ARGS(3)
      INTEGER*4  LEV, L_FIL, IL, IS, MJD_BEG_FIL, MJD_END_FIL, MJD_EPC, ISTA, &
     &           INDS(3), DIMS(3), IND_POI, KPOL, J1, J2, J3, J4, J5, J6, J7, &
     &           J8, J9, J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, &
     &           J20, J21, J22, IER
      REAL*8,    EXTERNAL :: DEL_ISA 
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, GET_FILE_FROM_DIR, IXMN4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      OBS_TYP%PLRZ       = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS     = VTD__BND 
!
! --- Generate the name of the directory with results of computation of 
! --- atmosphere opacity and reaiative brightness
!
      SOB_DIR = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &          PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_sob'
!
! --- Check whether the SOB_DIR exists
!
      DIR_DESC(1) = FUNC_OPENDIR ( SOB_DIR(1:I_LEN(SOB_DIR))//CHAR(0) )
      IF ( DIR_DESC(1) == 0 ) THEN
           CALL ERR_LOG ( 7711, IUER, 'PIMA_OPAL', 'Directiry with atmosphere '// &
     &         'opacity files, '//SOB_DIR(1:I_LEN(SOB_DIR))//' does not exist.'// &
     &         ' You need compute atmosphere opacity by running task opag' )
           RETURN
         ELSE 
           IS = CLOSEDIR ( %VAL(DIR_DESC(1)) )
      END IF
!
      ALLOCATE ( C_FIL(M_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( M_FIL*SIZEOF(C_FIL(1)), STR )
           CALL ERR_LOG ( 7712, IUER, 'PIMA_OPAL', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array C_FIL' )
           RETURN
      END IF
!
      IF ( PIM%NPOL == 1 ) THEN
           KPOL = 1
         ELSE
           KPOL = 2
      END IF
!
! --- Travel the directory tree and collect relevant files
!
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, SOB_DIR, FINAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 7713, IUER, 'PIMA_OPAL', 'Error in '// &
     &                 'reading input directory '//SOB_DIR )
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FINAM, '#' ) .GT. 0 ) GOTO 410
         IL = ILEN(FINAM)
         IF ( IL < 18 ) GOTO 410
         IF ( FINAM(IL-3:IL) == '.spd' .OR. FINAM(IL-7:IL) == '.spd.bz2' ) THEN
              L_FIL = L_FIL + 1
              IF ( L_FIL > M_FIL) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_FIL, STR )
                   CALL ERR_LOG ( 7714, IUER, 'PIMA_OPAL', 'Too many files '// &
     &                 'in directory '//SOB_DIR(1:I_LEN(SOB_DIR))//' -- '// &
     &                 'more than '//STR )
                   RETURN 
              END IF
              C_FIL(L_FIL) = FINAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL < 1 ) THEN
           CALL ERR_LOG ( 7715, IUER, 'PIMA_OPAL', 'No valid data files '// &
     &         'with atmosphere opatcity/brightness were found in the '// &
     &         'directory '//SOB_DIR )
           RETURN
      END IF
      IF ( L_FIL == 1 ) THEN
           CALL ERR_LOG ( 7716, IUER, 'PIMA_OPAL', 'Only one valid data '// &
     &         'files with atmosphere opatcity/brightness were found '// &
     &         'in the input directory '//SOB_DIR(1:I_LEN(SOB_DIR))// &
     &         ' while at least two data files are needed' )
           RETURN 
      END IF
!
! --- Sort data files in alphabetic order which is equivalent to time order
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
! --- Extract the date of data by parsing file names
!
      IL = ILEN(C_FIL(1))
      IF ( INDEX ( C_FIL(1), '.bz2' ) > 0 ) THEN
            DATE_BEG_STR = C_FIL(1)(IL-20:IL-17)//'.'//C_FIL(1)(IL-16:IL-15)//'.'// &
     &                     C_FIL(1)(IL-14:IL-10)//':'//C_FIL(1)(IL-9:IL-8)//':00.0'
         ELSE
            DATE_BEG_STR = C_FIL(1)(IL-16:IL-13)//'.'//C_FIL(1)(IL-12:IL-11)//'.'// &
     &                     C_FIL(1)(IL-10:IL-6)//':'//C_FIL(1)(IL-5:IL-4)//':00.0'
      END IF
      IL = ILEN(C_FIL(L_FIL))
      IF ( INDEX ( C_FIL(L_FIL), '.bz2' ) > 0 ) THEN
            DATE_END_STR = C_FIL(L_FIL)(IL-20:IL-17)//'.'//C_FIL(L_FIL)(IL-16:IL-15)//'.'// &
     &                     C_FIL(L_FIL)(IL-14:IL-10)//':'//C_FIL(L_FIL)(IL-9:IL-8)//':00.0'
          ELSE 
            DATE_END_STR = C_FIL(L_FIL)(IL-16:IL-13)//'.'//C_FIL(L_FIL)(IL-12:IL-11)//'.'// &
     &                     C_FIL(L_FIL)(IL-10:IL-6)//':'//C_FIL(L_FIL)(IL-5:IL-4)//':00.0'
      END IF 
!
! --- Check the dates. Opacity data should start before the 1st observations
! --- and end after the last observations.
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG_FIL, TIM_BEG_FIL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7717, IUER, 'PIMA_OPAL', 'Wrong format of the '// &
     &         'data file name '//C_FIL(1)(1:I_LEN(C_FIL(1)))// &
     &         ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &         'a valid calendar date' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_END_STR, MJD_END_FIL, TIM_END_FIL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7718, IUER, 'PIMA_OPAL', 'Wrong format of the '// &
     &         'data file name '//C_FIL(L_FIL)(1:I_LEN(C_FIL(L_FIL)))// &
     &         ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &         'a valid calendar date' )
           RETURN 
      END IF
!
      IF ( (MJD_BEG_FIL*86400.0D0 + TIM_BEG_FIL) > ( PIM%MJD_0*86400.0D0 + PIM%TAI_0 ) ) THEN
           CALL CLRCH ( STR )
           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, IER  )
           CALL ERR_LOG ( 7719, IUER, 'PIMA_OPAL', 'The earliest data file '// &
     &         'with atmosphere opacity '//C_FIL(1)(1:I_LEN(C_FIL(1)))// &
     &         ' is for date which is later than the experiment start date '// &
     &         STR(1:21) )
           RETURN 
      END IF
!
      IF ( (MJD_END_FIL*86400.0D0 + TIM_END_FIL) < &
     &     ( PIM%MJD_0*86400.0D0 + PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) ) ) THEN
           CALL CLRCH ( STR )
           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC), IER  )
           CALL ERR_LOG ( 7720, IUER, 'PIMA_OPAL', 'The latest data file '// &
     &         'with atmosphere opacity '//C_FIL(L_FIL)(1:I_LEN(C_FIL(L_FIL)))// &
     &         ' is for date which is earlier than the experiment stop date '// &
     &         STR(1:21) )
           RETURN 
      END IF
!
! --- Allocate memory for the ascii representation of data files with opacity
!
      ALLOCATE ( SAT(L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( L_FIL*SIZEOF(SAT(1)), STR )
           CALL ERR_LOG ( 7721, IUER, 'PIMA_OPAL', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes if dynamic memory for array '// &
     &         'SAT' )
           RETURN
      END IF
!
! --- Allocate memory for SPD_RES data structure
!
      ALLOCATE ( SPD_RES(PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( PIM%NSTA*SIZEOF(SPD_RES(1)), STR )
           CALL ERR_LOG ( 7722, IUER, 'PIMA_OPAL', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes if dynamic memory for array '// &
     &         'SPD_RES' )
           RETURN
      END IF
!
! --- Read ascii data files and copy its contents into array SAT
!
      DO 420 J2=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL SPD_3D_READ_ASCII ( C_FIL(J2), SAT(J2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7723, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to read input file with atmosphere opacity '// &
     &            'and radiative temperature '//C_FIL(J2) )
              RETURN
         END IF
 420  CONTINUE 
!
! --- Now cycle over stations. Read SAT data structures with results of SPD_3D 
! --- in ascii representation, decode them in binary form, and put into
! --- SPD_RES data structures
!
      DO 430 J3=1,PIM%NSTA
         CALL ERR_PASS ( IUER, IER )
         CALL SPD_DEL_INIT ( SPD_RES(J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7724, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to initialize SPD_RES object' )
              RETURN
         END IF
!
! ------ Find ISTA -- index of the J5-th station from the PIM%C_STA list
! ------ in SAT station list
!
         ISTA = 0
         DO 440 J4=1,SAT(1)%NS
            IF ( SAT(1)%SLINE(J4)%STA_NAME == PIM%C_STA(J3) ) ISTA = J4
 440     CONTINUE 
         IF ( ISTA == 0 ) THEN
              CALL ERR_LOG ( 7725, IUER, 'PIMA_OPAL', 'Opacity and '// &
     &            'radiative temperature were not computed for station '// &
     &            PIM%C_STA(J3) )
              RETURN
         END IF
!
         SPD_RES(J3)%ELV%N_EL = SAT(1)%NE
         SPD_RES(J3)%AZM%N_AZ = SAT(1)%NA
         SPD_RES(J3)%N_TIM    = L_FIL
         SPD_RES(J3)%N_FRQ    = SAT(1)%NF
!
         DIMS(1) = SPD_RES(J3)%ELV%N_EL
         DIMS(2) = SPD_RES(J3)%AZM%N_AZ
         DIMS(3) = SPD_RES(J3)%N_TIM
!
! ------ Alocate dynamic memory in SPD_RES data structures fir various fields.
! ------ They will  keep parsed opacity and brightness temperature data
!
         ALLOCATE ( SPD_RES(J3)%TIM_ARR(SPD_RES(J3)%N_TIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7726, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate memory for array TIM_ARR' )
              RETURN
         END IF
!
         ALLOCATE ( SPD_RES(J3)%ELV%ELEV(SPD_RES(J3)%ELV%N_EL), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7727, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J3)%ELV%ELEV' )
              RETURN
         END IF
!
         ALLOCATE ( SPD_RES(J3)%ELV%MAP(SPD_RES(J3)%ELV%N_EL), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7728, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J3)%ELV%MAP' )
              RETURN
         END IF
!
         ALLOCATE ( SPD_RES(J3)%AZM%AZIM(SPD_RES(J3)%AZM%N_AZ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7729, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J3)%AZM%AZ' )
              RETURN
         END IF
!
         ALLOCATE ( SPD_RES(J3)%FRQ_ARR(SPD_RES(J3)%N_FRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7730, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J3)%FRQ_ARR' )
              RETURN
         END IF
!
! ------ Generate the date array TIM_ARR -- time from the the first epoch
! ------ of data that are used for epxansion into 3D B-spline basis
!
         DO 450 J5=1,SPD_RES(J3)%N_TIM
            CALL DATE_TO_TIME ( SAT(J5)%TLINE%DATE_STR, MJD_EPC, TAI_EPC, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7731, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &               'control: wrong date format: '//SAT(J5)%TLINE%DATE_STR )
                 RETURN
            END IF
            IF ( J5 == 1 ) THEN
                 SPD_RES(J3)%TIM%MJD_BEG  = MJD_EPC
                 SPD_RES(J3)%TIM%TAI_BEG  = TAI_EPC
                 SPD_RES(J3)%TIM%TIM_STEP = TIM_STEP
               ELSE 
                 SPD_RES(J3)%TIM%MJD_END  = MJD_EPC
                 SPD_RES(J3)%TIM%TAI_END  = TAI_EPC
            END IF
            SPD_RES(J3)%TIM_ARR(J5) = (MJD_EPC - SPD_RES(J3)%TIM%MJD_BEG)*86400.0D0 + &
     &                                (TAI_EPC - SPD_RES(J3)%TIM%TAI_BEG)
 450     CONTINUE 
!
! ------ Generate the array of elevation angles and mapping function
!
         DO 460 J6=1,SPD_RES(J3)%ELV%N_EL
            READ ( UNIT=SAT(1)%ELINE(J6)%ANG, FMT='(F10.6)', IOSTAT=IER ) SPD_RES(J3)%ELV%ELEV(J6)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7732, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &               'control: wrong elevation angle format: '//SAT(1)%ELINE(J6)%ANG )
                 RETURN 
            END IF
            SPD_RES(J3)%ELV%ELEV(J6) = SPD_RES(J3)%ELV%ELEV(J6)*DEG__TO__RAD
!
            SPD_RES(J3)%ELV%MAP(J6) = DEL_ISA ( DBLE(SPD_RES(J3)%ELV%ELEV(J6)) )/ &
     &                                DEL_ISA ( P2I )
 460     CONTINUE 
!
! ------ Generate the array of azimuth angles
!
         DO 470 J7=1,SPD_RES(J3)%AZM%N_AZ
            READ ( UNIT=SAT(1)%ALINE(J7)%ANG, FMT='(F10.6)', IOSTAT=IER ) SPD_RES(J3)%AZM%AZIM(J7)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7733, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &               'control: wrong azimuth angle format: '//SAT(1)%ELINE(J7)%ANG )
                 RETURN 
            END IF
            SPD_RES(J3)%AZM%AZIM(J7) = SPD_RES(J3)%AZM%AZIM(J7)*DEG__TO__RAD
 470     CONTINUE 
!
! ------ Generate the frequency array
!
         DO 480 J8=1,SPD_RES(J3)%N_FRQ
            READ ( UNIT=SAT(1)%FLINE(J8)%FRQ, FMT='(F15.6)', IOSTAT=IER ) SPD_RES(J3)%FRQ_ARR(J8)
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7734, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &               'control: wrong format of the frequency: '// &
     &                SAT(1)%FLINE(J8)%FRQ )
                 RETURN 
            END IF
  480    CONTINUE 
!
! ------ Allocate memory for 4D arrays that will keep expansion coefficients
! ------ in the 3D B-spline basis. The 4th dimension is over frequency.
! ------ NB: remember: dimensions start from 1-SPD__MDEG.
!
         ALLOCATE ( SPD_RES(J3)%OPA(1-SPD__MDEG:SPD_RES(J3)%ELV%N_EL, &
     &                              1-SPD__MDEG:SPD_RES(J3)%AZM%N_AZ, &
     &                              1-SPD__MDEG:SPD_RES(J3)%N_TIM,    &
     &                              1:SPD_RES(J3)%N_FRQ), STAT=IER   )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL IINCH ( 4*(SPD_RES(J3)%ELV%N_EL + SPD__MDEG)* &
     &                       (SPD_RES(J3)%AZM%N_AZ + SPD__MDEG)* &
     &                       (SPD_RES(J3)%N_TIM    + SPD__MDEG)* &
     &                        SPD_RES(J3)%N_FRQ, STR ) 
              CALL ERR_LOG ( 7735, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'dynamic memory for array SPD_RES(J3)%OPA' )
              RETURN 
         END IF
!
         ALLOCATE ( SPD_RES(J3)%TAT(1-SPD__MDEG:SPD_RES(J3)%ELV%N_EL, &
     &                              1-SPD__MDEG:SPD_RES(J3)%AZM%N_AZ, &
     &                              1-SPD__MDEG:SPD_RES(J3)%N_TIM,    &
     &                              1:SPD_RES(J3)%N_FRQ), STAT=IER   )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL IINCH ( 4*(SPD_RES(J3)%ELV%N_EL  + SPD__MDEG)* &
     &                       (SPD_RES(J3)%AZM%N_AZ  + SPD__MDEG)* &
     &                       (SPD_RES(J3)%N_TIM + SPD__MDEG)* &
     &                        SPD_RES(J3)%N_FRQ, STR ) 
              CALL ERR_LOG ( 7736, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'dynamic memory for array SPD_RES(J3)%TAT' )
              RETURN 
         END IF
!
! ------ Cycle over frequency
!
         DO 490 J9=1,SPD_RES(J3)%N_FRQ
!
! --------- Populate arrays 
! --------- OPA -- atmosphere optical depth and 
! --------- TAT -- atmosphere brightness temperature at the station.
!
            DO 4100 J10=1,SPD_RES(J3)%N_TIM
               DO 4110 J11=1,SPD_RES(J3)%AZM%N_AZ
                  DO 4120 J12=1,SPD_RES(J3)%ELV%N_EL
!
! ------------------ NB: Here we assume opacity was computed only for one azimuth
!
                     READ ( UNIT=SAT(J10)%OLINE(J9,J12,1,ISTA)%OPA, FMT='(F6.4)', IOSTAT=IER ) &
     &                      SPD_RES(J3)%OPA(J12,J11,J10,J9)
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 7737, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &                        'control: error in reading opacity' )
                          RETURN 
                     END IF
!
! ------------------ NB: Here we assume Tatm was computed only for one azimuth
!
                     READ ( UNIT=SAT(J10)%OLINE(J9,J12,1,ISTA)%TAT, FMT='(F6.4)', IOSTAT=IER ) &
     &                      SPD_RES(J3)%TAT(J12,J11,J10,J9)
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 7738, IUER, 'PIMA_OPAL', 'Trap of internal '// &
     &                        'control: error in reading atmosphere radiative '// &
     &                        'temperature' )
                          RETURN 
                     END IF
 4120              CONTINUE 
 4110           CONTINUE 
 4100       CONTINUE 
!
! --------- Expand atmosphere optical depth and atmosphere brightness temperature
! --------- into 3D B-spline basus. Dimensions of the expansion:
! --------- mapping function, azimuth angle, and time
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_3D_CMP ( SPD__MDEG, 10, DIMS, SPD_RES(J3)%ELV%MAP, SPD_RES(J3)%AZM%AZIM, &
     &                          SPD_RES(J3)%TIM_ARR, SPD_RES(J3)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J9), &
     &                          IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7739, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &               'attempt to expand opacity in 3D B-spline basis' )
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_3D_CMP ( SPD__MDEG, 10, DIMS, SPD_RES(J3)%ELV%MAP, SPD_RES(J3)%AZM%AZIM, &
     &                          SPD_RES(J3)%TIM_ARR, SPD_RES(J3)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J9), &
     &                          IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7740, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &               'attempt to expand atmospere radiative temperature '// &
     &               'in 3D B-spline basis' )
                 RETURN 
            END IF
 490     CONTINUE 
!
         DO 4130 J13=1,PIM%NFRG
            PIM%STA(J3)%STMO(J13)%OPA_AVAIL   = .TRUE.
            PIM%STA(J3)%STMO(J13)%TTOA_AVAIL  = .FALSE.
            PIM%STA(J3)%STMO(J13)%TREC_AVAIL  = .FALSE.
            PIM%STA(J3)%STMO(J13)%TSPI_AVAIL  = .FALSE.
            PIM%STA(J3)%STMO(J13)%TSYS_AVAIL  = .FALSE.
            PIM%STA(J3)%STMO(J13)%TSRAT_AVAIL = .FALSE.
            PIM%STA(J3)%STMO(J13)%N_OPA = 2*PIM%NSCA
            PIM%STA(J3)%STMO(J13)%N_TAT = 2*PIM%NSCA
            PIM%STA(J3)%STMO(J13)%N_TTOA = 0
            PIM%STA(J3)%STMO(J13)%N_TREC = 0
            PIM%STA(J3)%STMO(J13)%N_TSPI = 0
            PIM%STA(J3)%STMO(J13)%N_TSYS = 0
!
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%IND_SCA ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%IND_SCA   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TIM ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TIM   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%EL  ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%EL    )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%AZ  ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%AZ    )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%OPA ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%OPA   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TAT ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TAT   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TREC ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TREC   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TSPI ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TSPI   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TSYS_CLN ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TSYS_CLN   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TSYS_MOD ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TSYS_MOD   )
            END IF
            IF ( ASSOCIATED ( PIM%STA(J3)%STMO(J13)%TTOA ) ) THEN
                 DEALLOCATE ( PIM%STA(J3)%STMO(J13)%TTOA   )
            END IF
!
! --------- Allocate memory for System Temperature MOdel objects
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%IND_SCA(PIM%STA(J3)%STMO(J13)%N_OPA), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7741, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%IND_SCA' )
                  RETURN 
            END IF
            PIM%STA(J3)%STMO(J13)%IND_SCA= 0
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%TIM(PIM%STA(J3)%STMO(J13)%N_OPA), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7742, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%TIM' )
                  RETURN 
            END IF
            PIM%STA(J3)%STMO(J13)%TIM = 0.0D0
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%EL(PIM%STA(J3)%STMO(J13)%N_OPA), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7743, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%EL' )
                  RETURN 
            END IF
            PIM%STA(J3)%STMO(J13)%EL = 0.0D0
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%AZ(PIM%STA(J3)%STMO(J13)%N_OPA), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7744, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%AZ' )
                  RETURN 
            END IF
            PIM%STA(J3)%STMO(J13)%AZ = 0.0D0
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%OPA(PIM%STA(J3)%STMO(J13)%N_OPA,PIM%NFRQ), &
     &                 STAT=IER )
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7745, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%OPA' )
                  RETURN 
            END IF
            PIM%STA(J3)%STMO(J13)%OPA = 0.0D0
!
            ALLOCATE ( PIM%STA(J3)%STMO(J13)%TAT(PIM%STA(J3)%STMO(J13)%N_OPA,PIM%NFRQ), &
     &                 STAT=IER )
            PIM%STA(J3)%STMO(J13)%TAT = 0.0D0
            IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7746, IUER, 'PIMA_OPAL', 'Failure in an '// &
     &                'attempt to allocate dynamic memory for array '// &
     &                '%STMO(J13)%TAT' )
                  RETURN 
            END IF
!
            PIM%STA(J3)%STMO(J13)%TSRAT = 0.0D0
 4130    CONTINUE 
 430  CONTINUE 
!
! === Good! We read input data files, parsed their contents and expanded &
! === optical depth and atmosphere brightness into 3D B-spline basus.   
! === Now we can compute opacity and atmosphere atmosphere brightness 
! === for every station, every scan, at scan nominal start and &
! === scan nominal end
!
      IND_POI = 0
      DO 4150 J15=1,PIM%NSCA
!
! ------ TIM_SCA -- time of scan start and scan end
!
         TIM_SCA(1) = PIM%TIM_R8(PIM%SCA(J15)%TIM_IND)
         TIM_SCA(2) = PIM%TIM_R8(PIM%SCA(J15)%TIM_IND+PIM%SCA(J15)%NUM_EPC-1)
         DO 4160 J16=1,2 ! Over scan start and scan end time
            IND_POI = IND_POI + 1
            DO 4170 J17=1,PIM%NSTA
!
! ------------ Compute aziumth elevation for J17-station that observes
! ------------ the source wtih index SOU_IND at moment of time TIM_SCA(J16)
!
               CALL ERR_PASS  ( IUER, IER )
               CALL VTD_DELAY ( PIM%C_SOU(PIM%SCA(J15)%SOU_IND),     &
     &                          PIM%C_STA(J17), PIM%C_STA(J17),      &
     &                          PIM%MJD_0, PIM%TAI_0 + TIM_SCA(J16), &
     &                          OBS_TYP, VTD, PH_DEL, PH_RATE,       &
     &                          DER_DEL, DER_RAT, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J15, STR )
                    CALL ERR_LOG ( 7747, IUER, 'PIMA_OPAL', 'Error in an '// &
     &               'attempt to compute theoretical path delay for '// &
     &               'scan '//STR )
                    RETURN 
               END IF
!
! ------------ Extract azimuth and elevation angles
!
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_GET_AZEL ( PIM%C_SOU(PIM%SCA(J15)%SOU_IND),     &
     &                             PIM%C_STA(J17), PIM%C_STA(J17),      &
     &                             PIM%MJD_0, PIM%TAI_0 + TIM_SCA(J16), &
     &                             VTD, AZ, EL, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J1, STR )
                    CALL ERR_LOG ( 7748, IUER, 'PIMA_OPAL', 'Error in an '// &
     &                  'attempt to compute azimuth and elevation delay for '// &
     &                  'observation '//STR )
                    RETURN 
               END IF
!
! ------------ The elevation angle should be no less than the minumim elevation
! ------------ used by the SPD.
!
               ELEV = MAX ( EL(1), (1.0D0 + 1.0D-6)*SPD_RES(J17)%ELV%ELEV(SPD_RES(J17)%ELV%N_EL) )
!
! ------------ Compute the mapping function and time since the begining of the expansion
! ------------ interval. Computes arguments for interpolation
!
               ARGS(1) = DEL_ISA ( ELEV )/DEL_ISA ( P2I )
               ARGS(2) = AZ(1)
               ARGS(3) = ( PIM%MJD_0*86400.0D0 + PIM%TAI_0 + TIM_SCA(J16) ) - &
     &                   ( SPD_RES(J17)%TIM%MJD_BEG*86400.0D0 + SPD_RES(J17)%TIM%TAI_BEG )
!
! ------------ Find the pivotal indices
!
               INDS(1) = IXMN4 ( DIMS(1), SPD_RES(J17)%ELV%MAP,  ARGS(1) )
               INDS(2) = IXMN4 ( DIMS(2), SPD_RES(J17)%AZM%AZIM, ARGS(2) )
               INDS(3) = IXMN4 ( DIMS(3), SPD_RES(J17)%TIM_ARR,  ARGS(3) )
!
               IF ( INDS(1) < 1 .AND. &
     &              ABS(ARGS(1) - SPD_RES(J17)%ELV%MAP(1)) <        &
     &              EPS_MAR*ABS(ARGS(1) - SPD_RES(J17)%ELV%MAP(1))  ) THEN
                    INDS(1) = 1
               END IF         
               IF ( INDS(2) < 1 .AND. &
     &              ABS( ARGS(1) - SPD_RES(J17)%AZM%AZIM(1) ) <     &
     &              EPS_MAR*ABS(ARGS(2) - SPD_RES(J17)%AZM%AZIM(1)) ) THEN
                    INDS(2) = 1
               END IF         
               IF ( INDS(3) < 1 .AND. &
     &              ABS( ARGS(1) - SPD_RES(J17)%TIM_ARR(1)  ) <     &
     &              EPS_MAR*ABS(ARGS(3) - SPD_RES(J17)%TIM_ARR(1))  ) THEN
                    INDS(3) = 1
               END IF         
!
! ------------ Cycle over frequency groups
!
               DO 4180 J18=1,PIM%NFRG
!
! --------------- Fill fuelds of STMO object
!
                  PIM%STA(J17)%STMO(J18)%IND_SCA(IND_POI) = J15
                  PIM%STA(J17)%STMO(J18)%TIM(IND_POI)     = TIM_SCA(J16)
                  PIM%STA(J17)%STMO(J18)%EL(IND_POI)      = EL(1)
                  PIM%STA(J17)%STMO(J18)%AZ(IND_POI)      = AZ(1)
!
! --------------- Cycle over frequency
!
                  DO 4190 J19=1,PIM%NFRQ
!
! ------------------ Finally, compute the atmosphere optical depth and atmosphere 
! ------------------ brightness for the specified station
!
                     IF ( INDS(1) > 0 ) THEN
                          OPA_VAL = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                  SPD_RES(J17)%ELV%MAP, &
     &                                  SPD_RES(J17)%AZM%AZIM, &
     &                                  SPD_RES(J17)%TIM_ARR, &
     &                                  SPD_RES(J17)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J19) )
                          TAT_VAL = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                  SPD_RES(J17)%ELV%MAP, &
     &                                  SPD_RES(J17)%AZM%AZIM, &
     &                                  SPD_RES(J17)%TIM_ARR, &
     &                                  SPD_RES(J17)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J19) )
                        ELSE 
!
! ----------------------- for the case when elevation angle is less than the limit
! ----------------------- Station may actually not observe that scan because the 
! ----------------------- source was under the horizont
!
                          OPA_VAL = 0.0D0
                          TAT_VAL = 0.0D0
                     END IF
                     PIM%STA(J17)%STMO(J18)%OPA(IND_POI,J19) = OPA_VAL
                     PIM%STA(J17)%STMO(J18)%TAT(IND_POI,J19) = TAT_VAL
 4190             CONTINUE 
 4180          CONTINUE 
 4170       CONTINUE 
 4160    CONTINUE 
 4150 CONTINUE 
!
! --- Put STMO_IND indices into PIM%OBS data structure
!
      DO 4200 J20=1,PIM%NOBS
         PIM%OBS(J20)%STMO_IND(1) = 1 + 2*(PIM%OBS(J20)%SCA_IND-1)
         PIM%OBS(J20)%STMO_IND(2) = 2 + 2*(PIM%OBS(J20)%SCA_IND-1)
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 210 ) J20, PIM%OBS(J20)%SCA_IND, PIM%C_STA(PIM%OBS(J20)%STA_IND(1)), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%EL(PIM%OBS(J20)%STMO_IND(1))/DEG__TO__RAD, &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%EL(PIM%OBS(J20)%STMO_IND(2))/DEG__TO__RAD, &
     &                   PIM%OBS(J20)%ELEV(1)/DEG__TO__RAD, &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%OPA(PIM%OBS(J20)%STMO_IND(1),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%OPA(PIM%OBS(J20)%STMO_IND(2),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%TAT(PIM%OBS(J20)%STMO_IND(1),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%TAT(PIM%OBS(J20)%STMO_IND(2),1)
              WRITE ( 6, 210 ) J20, PIM%OBS(J20)%SCA_IND, PIM%C_STA(PIM%OBS(J20)%STA_IND(2)), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%EL(PIM%OBS(J20)%STMO_IND(1))/DEG__TO__RAD, &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%EL(PIM%OBS(J20)%STMO_IND(2))/DEG__TO__RAD, &
     &                   PIM%OBS(J20)%ELEV(2)/DEG__TO__RAD, &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%OPA(PIM%OBS(J20)%STMO_IND(1),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%OPA(PIM%OBS(J20)%STMO_IND(2),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%TAT(PIM%OBS(J20)%STMO_IND(1),1), &
     &                   PIM%STA(PIM%OBS(J20)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%TAT(PIM%OBS(J20)%STMO_IND(2),1)
 210          FORMAT ( 'PIMA_OPAL: Obs: ', i5, ' sca_ind: ', i4, ' Sta_ind: ', A, &
     &                 ' El_stmo: ', F5.2, 1X, F5.2, ' El_obs: ', F7.2, &
     &                  ' Opa: ', F6.4,1X,F6.4, ' Tat: ', F6.2, 1x, f6.2 )
         END IF
 4200 CONTINUE 
!
! --- Just fot the sake of completness, deallocate dynamic memory for
! --- SPD_RES, SAT, and C_FIL
!
      DO 4210 J21=1,PIM%NSTA
         CALL ERR_PASS ( IUER, IER ) 
         CALL SPD_DEL_QUIT ( SPD_RES(J21), IER )
 4210 CONTINUE 
!
      DO 4220 J22=1,L_FIL
         CALL SAT_QUIT ( SAT(J22) )
 4220 CONTINUE 
!
      DEALLOCATE ( C_FIL )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_OPAL: opacity and Tatm were loaded in pim '// &
     &                        'object of experiment '// &
     &                         PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_OPAL  !#!#
