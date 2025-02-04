      PROGRAM    BNC_PLOT
! *********************************************************************************
! *                                                                               *
! *   Program BNC_PLOT                                                            *
! *                                                                               *
! *   Plot the Tsys from a given experiment.                                      *
! *   INPUT:                                                                      *
! *           BNC_FILE  =  path to binary experiment file             { CHAR }    *
! *                                                                               *
! *           IND_FRQ   =  sky freq index to plot                     { INT }     *
! *                                                                               *
! *           MODE      =  Plotting Mode                              { CHAR }    *
! *                        == time   - Tsys [K] vs Time [Hrs from 00:00UTC] at IF *
! *                                  - Tsys_ave [K] vs Time [Hrs from 00:00UTC]   *
! *                        == elev   - Tsys [K] vs Elevation [deg] at IF          *
! *                        == azim   - Tsys [K] vs Azimuth [deg] at IF            *
! *                        == azel   - [X,Y,Z] = [ Az, El, Tsys_Range]            *
! *                        == spectr - Tsys [K] vs Freq [GHz] at given time       *
! *                                                                               *
! *   USAGE:                                                                      *
! *           bnc_plot BNC_FILE IND_FRQ MODE                                      *
! *                                                                               *
! *   OUTPUT:                                                                     *
! *           pgplot figure(s)                                                    *
! *                                                                               *
! *  ### 25-AUG-2021     BNC_PLOT         v1.3 (c)    L. Petrov  06-NOV-2021 ###  *
! *                                                                               *
! *********************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
!@IN_ATP@!      INCLUDE   'astro_constants.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILIN*128, STR_DO_DATE*24, STR_TSYS_DATE*24, &
     &           STR_MET_DATE*24, STR_EPO_DATE*24, STR*128, TIT*64
      REAL*8     TSYS_MIN, TSYS_MAX, FRQ_MIN
      PARAMETER  ( TSYS_MIN = 9.0D0 )   ! Note that ANC__TSYS_MIN = 5.0D0
      PARAMETER  ( TSYS_MAX = 599.1D0 )
      PARAMETER  ( FRQ_MIN  = 1500.0D0 )
      INTEGER*4  NEP_ARR(ANC__MTPS)
      INTEGER*8  OFF_ARR(ANC__MTPS)
      REAL*8     TIM_1ST, FRQ_ARR(ANC__MTPS)
      REAL*8     TIM_AVR(ANC__MEPC), TSYS_AVR(ANC__MEPC)
      REAL*8     TSYS_RMS(ANC__MEPC)
      REAL*4     TIM_ARR(ANC__MEPC), TSYS_ARR(ANC__MEPC)
      REAL*4     AZ_ARR(ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     T1(ANC__MEPC), X1(ANC__MEPC), T2(ANC__MEPC)
      REAL*8     X2(ANC__MEPC), E2(ANC__MEPC), T11(ANC__MEPC)
      REAL*8     T12(ANC__MEPC), T22(ANC__MEPC), T3(ANC__MEPC)
      REAL*8     X3(ANC__MEPC), T4(ANC__MEPC), X4(ANC__MEPC)
      REAL*8     AZ_SCA(ANC__MEPC), EL_SCA(ANC__MEPC)
      REAL*8     TSYS_SCA(ANC__MEPC)
      REAL*8     FRQ_R8(ANC__MEPC,2), TSYS_FRQ_R8(ANC__MEPC,2)
      REAL*8     TSYS_FRQ_E8(ANC__MEPC,2)
      INTEGER*4  IND_ARR(ANC__MEPC)
      INTEGER*4  SEEK_SET, ARG_LEN, PREF
      INTEGER*1  POL_ARR(ANC__MTPS)
      REAL*8     EL_MIN
      PARAMETER  ( EL_MIN = 4.9*DEG__TO__RAD )
      LOGICAL*1  LEX, FL_APPLY_TATM
      CHARACTER  ID_ARR(ANC__MTPS)*4, MODE*6
      INTEGER*4  ME
      PARAMETER  ( ME = 16 ) 
      REAL*8     ELEV_TATM(ME), VAL_TATM(ME), SPL_TATM(ME), TATM
      INTEGER*4  LUN, NBT, IND_FRQ, IND_TIM, IND_POL, LE
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8
      INTEGER*4  IP, KP, LP, NP, JP, JN, KB, K1, K2, IDEV, IUER
      INTEGER*8  IS
      REAL*8,    EXTERNAL :: FSPL8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8
!
! --- Pre-define variables
!
      IND_FRQ = 0               ! Frequency index
      FL_APPLY_TATM = .FALSE.   ! Should we use the TATM azimuths?
      MODE = 'time'  ! or 'elev'  or 'spectr' or 'azim' or 'azel'
!
! --- Read user input
!
      IF ( IARGC() < 1 ) THEN
         WRITE ( 6, '(A)' ) 'USAGE: bnc_plot bnc_file ind_frq mode'
         CALL EXIT ( 0 )
      ELSE
         CALL GETARG ( 1, FILIN )
         INQUIRE     ( FILE=FILIN, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
            WRITE ( 6, '(A)' ) 'Cannot find file '//TRIM(FILIN)
            CALL EXIT ( 1 )
         END IF
         CALL GETARG ( 2, STR )
         CALL CHIN   ( STR, IND_FRQ )
         IF ( IARGC() > 2 ) THEN
            CALL GETARG ( 3, MODE )
         END IF
      END IF
!
! --- Is this an accepted mode?
!
      IF ( MODE == 'time' .OR. MODE == 'elev' .OR.                      &
     &     MODE == 'azim' .OR. MODE == 'azel'       ) THEN
         CONTINUE
!
! --- Set the Time Index to Freq index if plotting Tsys vs spectrum
!
      ELSEIF ( MODE == 'spectr'  ) THEN
         IND_TIM = IND_FRQ
! ---
      ELSE
         IUER = -1
         CALL ERR_LOG ( 5001, IUER, 'BNC_PLOT',                         &
     &           'Unsupported mode '//MODE//                            &
     &           ' -- time, elev, azim, azel, or spectr were expected' )
         CALL EXIT ( 1 )
      END IF
!
! --- Get the Spline coefficients of the ???effect temperature of the 
!     atmosphere???
!
      CALL GET_SPL_TATM ( ME, LE, ELEV_TATM, VAL_TATM, SPL_TATM )
!
! --- Binary File Unit Number.
!
      LUN = 18
!
! --- Read binary file
!
      IUER = -1
      CALL BNC_READ ( FILIN, ANC, LUN, FRQ_ARR, POL_ARR, ID_ARR,        &
     &                NEP_ARR, OFF_ARR, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -2
         CALL ERR_LOG ( 5002, IUER, 'BNC_PLOT',                         &
     &           'Failure in reading intput file with '//               &
     &           'antenna telemetry in binary format' )
         CALL EXIT ( 1 ) 
      END IF
!
! ---Printouts to check
!
      WRITE ( 6, * ) '_________________________________________________'
      WRITE ( 6 ,* ) 'Exp_code: ', ANC%EXP_CODE
      WRITE ( 6 ,* ) 'Sta_name: ', ANC%STA_NAM
!@NOKH@!      WRITE ( 6 ,* ) 'Obs_date: ', STR_DO_DATE(1:23)
      WRITE ( 6 ,* ) 'Num_tps:  ', ANC%NUM_TPS
      WRITE ( 6, * ) 'Num_tsys: ', ANC%NUM_TSYS
      WRITE ( 6, * ) '_________________________________________________'
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%% NOT SURE THE FOLLOWING CONTENT SERVES ANY SIGNIFICANT PURPOSE %%%!
        PRINT *, "%%%%%%% BNC_PLOT - 153 %%%%%%%%%%%%%%%%%%"
        PRINT *, "MUTED CONTENT TO READ FRQ INDICES AND THEIR POL. "
!
!@_MUTE_@!
!@_MUTE_@!      DO 410 J1=1,ANC%NUM_TPS
!@_MUTE_@!         WRITE ( UNIT=TIT, FMT=110 ) J1, FRQ_ARR(J1),                   &
!@_MUTE_@!     &                               ANC__POL(POL_ARR(J1)), ID_ARR(J1)
!@_MUTE_@!         IF ( IND_FRQ < 1 ) THEN
!@_MUTE_@!            WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
!@_MUTE_@!         END IF
!@_MUTE_@! 410  CONTINUE 
      IF ( IND_FRQ < 1 ) THEN
         IUER = -1 
         CALL ERR_LOG ( 5003, IUER, 'BNC_PLOT',                         &
     &           'Index below zero -- expected IND_FRQ > 0' )
         CALL EXIT ( 0 )
      END IF
!
! --- Write the Plot title to "TIT"
!
      WRITE ( UNIT=TIT, FMT=110 ) IND_FRQ, FRQ_ARR(IND_FRQ),            &
     &                            ANC__POL(POL_ARR(IND_FRQ)),           &
     &                            ID_ARR(IND_FRQ)
!
! --- Get the system constant "SEEK_SET"
!     SEEK_SET moves file pointer to the beginning of the file.
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- 
!
      IF ( MODE == 'time' .OR. &
     &     MODE == 'elev' .OR. &
     &     MODE == 'azim' .OR. &
     &     MODE == 'azel'      ) THEN
!
! ------ Move pointer back to the beginning of file
!
         IS = LSEEK (%VAL(LUN), %VAL(OFF_ARR(IND_FRQ)), %VAL(SEEK_SET))
         CALL GERROR( STR ) 
!
! ------ Get time array for the given index, & parse to TIM_ARR
!

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 197 %%%%%%%%%%%%%%"


         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', NEP_ARR(IND_FRQ), TIM_ARR,       &
     &                      NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            WRITE ( 6, * ) 'Error in reading TIM_ARR NBT= ', NBT,       &
     &                     ' IND_FRQ= ', IND_FRQ
            WRITE ( 6, * ) 'MEL= ', NEP_ARR(IND_FRQ),                   &
     &                     ' OFF_ARR= ', OFF_ARR(IND_FRQ)
            WRITE ( 6, * ) 'NEP_ARR= ', NEP_ARR(1:ANC%NUM_TPS)
            WRITE ( 6, * ) 'OFF_ARR= ', OFF_ARR(1:ANC%NUM_TPS)
            CALL EXIT ( 1 )
         END IF
!
! ------ Get tsys array for the given index, & parse to TSYS_ARR
!
      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 215 %%%%%%%%%%%%%%"

         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', NEP_ARR(IND_FRQ), TSYS_ARR,      &
     &                      NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            WRITE ( 6, * ) 'Error in reading TSYS_ARR' ; CALL EXIT ( 1 )
         END IF

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 224 %%%%%%%%%%%%%%"

!
! ------ Get azimuth array for the given index, & parse to AZ_ARR
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', NEP_ARR(IND_FRQ), AZ_ARR,        &
     &                      NBT, IUER )
         IF ( IUER .NE. 0 ) THEN
            WRITE ( 6, * ) 'Error in reading AZ_ARR' ; CALL EXIT ( 1 )
         END IF

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 236 %%%%%%%%%%%%%%"


!
! ------ Get elevation array for the given index, & parse to EL_ARR
!
         IUER = -1
         CALL RDBIN_ARRAY ( LUN, 'R4', NEP_ARR(IND_FRQ), EL_ARR,        &
     &                      NBT, IUER )

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 246 %%%%%%%%%%%%%%"

         IF ( IUER .NE. 0 ) THEN
            WRITE ( 6, * ) 'Error in reading EL_ARR' ; CALL EXIT ( 1 )
         END IF
!
! ------ Close binary file
!
         CALL BINF_CLOSE ( LUN, IUER )
!
! ------ Copy the parsed arrays of Time, Tsys, Elevation, and Azimuth
!        to variables T1, X1, T11, and T12 respectively.
!
         KP = 0
         DO 420 J2=1,NEP_ARR(IND_FRQ)
!
! --------- If TIM_ARR and TSYS_ARR values are not NaN values
!
            IF ( .NOT. IS_R4_NAN ( TIM_ARR(J2)  ) .AND.                 &
     &           .NOT. IS_R4_NAN ( TSYS_ARR(J2) )       ) THEN
               KP = KP + 1
               T1(KP)  = TIM_ARR(J2)
               T11(KP) = EL_ARR(J2)/DEG__TO__RAD
               T12(KP) = AZ_ARR(J2)/DEG__TO__RAD
               X1(KP)  = TSYS_ARR(J2)
!
! ------------ Don't count outliers
!
               IF ( TSYS_ARR(J2) > TSYS_MAX  .OR.                       &
     &              TSYS_ARR(J2) < TSYS_MIN      ) THEN
                  KP = KP - 1
               END IF
            END IF
 420     CONTINUE 
!
! ------ If T1, X1, T11, and T12 are populated then Define the plot 
!        parameters like title and abscissa information 
!
         IF ( KP > 0 ) THEN
            WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
            CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', TRIM(TIT) )
! ---------
            IF ( MODE == 'time' ) THEN
               CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time [hrs]' )
! ---------
            ELSEIF ( MODE == 'elev' ) THEN
               CALL DIAGI_SETDEF (IUER, 'DIAGI_UNIT', 'Elevation [deg]')
! ---------
            ELSEIF ( MODE == 'azim' ) THEN
               CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Azimuth [deg]' )
            END IF
! ---------
            CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! --------- Filter the Tsys to get the averages and RMS's
!

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 303 %%%%%%%%%%%%%%"


            IUER = -1
            CALL TSYS_TIME_FILTER ( KP, T1, X1, LP, NP, TIM_AVR,        &
     &                              TSYS_AVR, TSYS_RMS, IND_ARR, IUER )

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 310 %%%%%%%%%%%%%%"

!
! --------- Convert time to hours from midnight UTC
!
            DO 430 J3=1,NEP_ARR(IND_FRQ)
               T1(J3) = (T1(J3) + ANC%TAI_TSYS)/3600.0 -24.0
 430        CONTINUE 

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 319 %%%%%%%%%%%%%%"


!
! --------- Popukate the paremeters for representig the average values
!           provided the average Tsys is above 5.0 K
!
            JP = 0
            DO 440 J4=1,NP
! ------------
               IF ( TSYS_AVR(J4) > 5.0D0 ) THEN
!
! --------------- Skip elevations below 4.9 deg
!
                  IF ( MODE == 'elev' ) THEN
                     IF ( EL_ARR(IND_ARR(J4)) < EL_MIN ) GOTO 440
                  END IF
! ---------------
                  JP = JP + 1                                            ! Counter
! --------------- 
                  AZ_SCA(JP)   = AZ_ARR(IND_ARR(J4))/DEG__TO__RAD        ! Az in Degrees
                  EL_SCA(JP)   = EL_ARR(IND_ARR(J4))/DEG__TO__RAD        ! El > 4.9 Degrees
                  TSYS_SCA(JP) = TSYS_AVR(J4)                            ! Tsys > 5.0 K 
! ---------------
                  T2(JP)  = (TIM_AVR(J4) + ANC%TAI_TSYS)/3600.D0 - 24.D0 ! Time in hrs from the first 00:00UTC
                  T3(JP)  = (TIM_AVR(J4) + ANC%TAI_TSYS)                 ! Time in seconds 
                  T22(JP) = EL_ARR(IND_ARR(J4))/DEG__TO__RAD             ! Same as EL_SCA
                  X2(JP)  = TSYS_AVR(J4)                                 ! Same as TSYS_SCA
                  E2(JP)  = TSYS_RMS(J4)                                 ! Tsys error
!
! --------------- If using the effective atmospheric temperature
! --------------- Use Spline coefficients to compute teh TATM and 
!                 substract its effect from the total Tsys
!
                  IF ( FL_APPLY_TATM ) THEN
                     IP   = IXMN8 ( LE, ELEV_TATM, T22(JP) )
                     TATM = FSPL8 ( T22(JP), LE, ELEV_TATM, VAL_TATM,   &
     &                              IP, SPL_TATM )
                     X2(JP) = X2(JP) - TATM
                     E2(JP) = E2(JP) - TATM
                  END IF
               END IF
 440        CONTINUE 

      PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - 363 %%%%%%%%%%%%%%"

!
! --------- make plots depending on the mode
!
            IF ( MODE == 'time' ) THEN
               CALL DIAGI_1  ( KP, T1, X1, IUER )
               CALL DIAGI_1E ( JP, T2, X2, E2, IUER )
! ---------
            ELSEIF ( MODE == 'elev' ) THEN
               CALL ELEV_NRML ( JP, T3, T22, X2, JN, T4, X4 )
               CALL DIAGI_1  ( KP, T11, X1, IUER )
               CALL DIAGI_1E ( JP, T22, X2, E2, IUER )
               CALL DIAGI_1  ( JN, T4, X4, IUER )
! ---------
            ELSEIF ( MODE == 'azim' ) THEN
               CALL DIAGI_1  ( KP, T12, X1, IUER )
            END IF
         END IF
!
! --- Plotting Tsys vs Spectrum at given time stamps
!
      ELSEIF ( MODE == 'spectr' ) THEN
! ------
         FRQ_R8 = 0
         TSYS_FRQ_R8 = 0
         TSYS_FRQ_E8 = 0
! ------
         K1 = 0
         K2 = 0
! ------

       PRINT *, "%%%%%%%%%%%% BNC_PLOT - 395 %%%%%%%%%%%%%%%"
       PRINT *, "SPECTRAL MODE "
         DO 450 J5=1,ANC%NUM_TPS
!
! --------- Move pointer back to the beginning of file
!
            IS = LSEEK ( %VAL(LUN), %VAL(OFF_ARR(J5)), %VAL(SEEK_SET) )
!
! --------- Skip empty points of NEP_ARR
!
            IF ( NEP_ARR(J5) == 0 ) GOTO 450
!
! --------- Parse Time, Tsys, azimuth, and elevation arrays
!
            IUER = -1
            CALL RDBIN_ARRAY(LUN, 'R4',NEP_ARR(J5), TIM_ARR,  NBT, IUER)
            CALL RDBIN_ARRAY(LUN, 'R4',NEP_ARR(J5), TSYS_ARR, NBT, IUER)
            CALL RDBIN_ARRAY(LUN, 'R4',NEP_ARR(J5), AZ_ARR,   NBT, IUER)
            CALL RDBIN_ARRAY(LUN, 'R4',NEP_ARR(J5), EL_ARR,   NBT, IUER)

        PRINT *, "%%", J5, NEP_ARR(J5) !, SIZE(TIM_ARR), SIZE(TSYS_ARR), SIZE(AZ_ARR), SIZE(EL_ARR), NBT, LUN
!        IF ( J5 > 3 ) THEN 
 !          DO J8 = 1, SIZE(TIM_ARR)/2000
!
 !             PRINT *, J5, TIM_ARR(J8), TSYS_ARR(J8), FRQ_ARR(J8)
  !         END DO

   !     ENDIF 
   !     PRINT *, "%%%%%%%%%%%%%%%% BNC_PLOT - ??? %%%%%%%%%%%%%"

!
! --------- Eliminate NaN values from TSYS_ARR, and TIM_ARR
!
            KP = 0
            DO 460 J6=1,NEP_ARR(J5)
               IF ( .NOT. IS_R4_NAN ( TIM_ARR(J6)  ) .AND.              &
     &              .NOT. IS_R4_NAN ( TSYS_ARR(J6) )       ) THEN
                  KP = KP + 1
                  T1(KP) = TIM_ARR(J6)
                  X1(KP) = TSYS_ARR(J6)
               END IF
 460        CONTINUE 
!
! --------- If T1, and X1 are populated. 
!
            IF ( KP > 0 ) THEN
!
! ------------ Filter the Tsys to get the averages and RMS's
!
               IUER = -1
               CALL TSYS_TIME_FILTER_FRQ ( KP, T1, X1, LP, NP, TIM_AVR, &
     &                                     TSYS_AVR, TSYS_RMS, IUER )
!
! ------------ Date we are printing
!
!@#@!               STR_EPO_DATE = MJDSEC_TO_DATE ( ANC%MJD_TSYS,            &
!@#@!     &                             ANC%TAI_TSYS + TIM_AVR(MAX(1,NP/2)), &
!@#@!     &                             IUER )
               STR_EPO_DATE = MJDSEC_TO_DATE ( ANC%MJD_TSYS,            &
     &                             ANC%TAI_TSYS + TIM_AVR(MAX(1,NP/2)), &
     &                             IUER )
!
!
! ------------
!
               IF ( IND_TIM .LE. NP ) THEN
                  IND_FRQ = J5/2+1
                  IND_POL = MOD(J5+1,2) + 1
! ---------------
                  IF ( TSYS_AVR(IND_TIM) > TSYS_MIN .AND.               &
     &                 TSYS_AVR(IND_TIM) < TSYS_MAX       ) THEN
                     IF ( IND_POL == 1 ) THEN
                        K1 = K1 + 1
                        FRQ_R8(K1,IND_POL) = 1.D-3*FRQ_ARR(J5)
                        TSYS_FRQ_R8(K1,IND_POL) = TSYS_AVR(IND_TIM)
                        TSYS_FRQ_E8(K1,IND_POL) = TSYS_RMS(IND_TIM)
                     ELSEIF ( IND_POL == 2 ) THEN
                        K2 = K2 + 1
                        FRQ_R8(K2,IND_POL) = 1.D-3*FRQ_ARR(J5)
                        TSYS_FRQ_R8(K2,IND_POL) = TSYS_AVR(IND_TIM)
                        TSYS_FRQ_E8(K2,IND_POL) = TSYS_RMS(IND_TIM)
                     END IF
                  END IF
               END IF
            END IF
 450     CONTINUE

         PRINT *, "%%%%%%%%%%%% BNC_PLOT - 482 %%%%%%%%%%"
         PRINT *, "EPO_DATE: ", STR_EPO_DATE, TIM_AVR(MAX(1,NP/2)), TIM_AVR(NP/2), NP


!
! ------ Close the binary file
!
         CALL BINF_CLOSE ( LUN, IUER )
!
! ------ Define abscissa and plot title
!
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR(1:4), FMT='(F4.1)' ) EL_ARR(IND_TIM)/         &
     &                                         DEG__TO__RAD
         CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT',                        &
     &                       'Tsys at '//ANC%STA_NAM//' on '//          &
     &                       STR_EPO_DATE(1:19)//' UTC ' )                 !! at elev '//STR(1:4) )
         CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Frequency [GHz]' )
         CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! ------ Two plots in one file. For the different polarizations
!
         CALL DIAGI_2E ( K1, FRQ_R8(1,1), TSYS_FRQ_R8(1,1),             &
     &                   TSYS_FRQ_E8(1,1), K2, FRQ_R8(1,2),             &
     &                   TSYS_FRQ_R8(1,2), TSYS_FRQ_E8(1,2), IUER )
      END IF
!
! --- Plot the azel mode
!
      IF ( MODE == 'azel' ) THEN
         IDEV = 1
         CALL TSYS_AZEL_PLOT ( ANC%STA_NAM, FRQ_ARR(IND_FRQ),           &
     &                         ANC__POL(POL_ARR(IND_FRQ)), IDEV, JP,    &
     &                         EL_SCA, AZ_SCA, TSYS_SCA )
      END IF
!
! --- Printing formats
!
 110  FORMAT ( 'IND_FRQ= ', I4, ' Frq= ', F8.1, ' Pol: ', A1,           &
     &         ' ID_ARR= ', A4 )
!
      END  PROGRAM   BNC_PLOT  !#!
