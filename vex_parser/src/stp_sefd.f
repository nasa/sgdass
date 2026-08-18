      SUBROUTINE  STP_SEFD ( STP, IND_STA, MJD, TAI, FREQ, ELEV, TSYS_VAL, &
     &                       GAIN_VAL, SEFD, IUER )
! ***************************************************************************
! *                                                                         *
! *   Routine STP_SEFD computes the System Equivalent Flux Density at a     *
! *   given time and elevatin from the station parameter file.              *
! *                                                                         *
! *   INPUT:                                                                *
! *            STP   =  Station Parameter File Object   { DERIVED }         *
! *                                                                         *
! *            IND_STA   =  INTEGER*4  The station index. { INT }           *
! *                                                                         *
! *            MJD       =  Observation MJD              { INT }            *
! *                                                                         *
! *            TAI       =  Observation TAI              { REAL }           *
! *                                                                         *
! *            FREQ      =  Frequency                    { REAL }  [ Hz ]   *
! *                                                                         *
! *            ELEV      =  Elevation                    { REAL }  [ rad ]  *
! *                                                                         *
! *            IUER      =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            SEFD      =  System Equivalent Flux Density   { REAL }       *
! *                                                                         *
! *   ###  29-OCT-2020  STP_SEFD   v1.0 (c)  N. Habana  29-OCT-2020  ###    *
! *                                                                         *
! *   ###  06-NOV-2020  STP_SEFD   v2.0 (c)  N. Habana  06-NOV-2020  ###    *
! *     -  Using STP object directly, instead of the STP File.              *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'stp.i'
      INCLUDE     'astro_constants.i'
      TYPE      ( STP__TYPE ) :: STP
      INTEGER*4   IND_STA, MJD, IUER
      REAL*8      TAI, FREQ, ELEV, SEFD
      REAL*8      TSYS_VAL, GAIN_VAL
      REAL*8      DELTA_ELV, DELTA_TSYS, DELTA_GAIN, RATIO
      CHARACTER   STR*32
      INTEGER*4   J0, J1, J2, J3, J4
      INTEGER*4   MJD_CHK, TAI_CHK, FRQ_CHK
      INTEGER*4   IDX      
      INTEGER*4,  EXTERNAL :: ILEN, IXMN8, IXMN8_S
!
! --- Find the Tsys with a date range includes the observation date
!
      MJD_CHK = 0
      TAI_CHK = 0
      FRQ_CHK = 0
!
      DO 110 J1 = 1, STP%STA(IND_STA)%NTSYS
!
! ------ First check if you are looking at the right frequency range
!
         IF ( (FREQ .GE. STP%STA(IND_STA)%TSYS(J1)%FRQ_RANGE(1)) .AND. &
     &        (FREQ .LE. STP%STA(IND_STA)%TSYS(J1)%FRQ_RANGE(2))       ) THEN 
              FRQ_CHK = 1
!
! --------- Second check if the MJD is within range
!
            IF ( (MJD .GE. STP%STA(IND_STA)%TSYS(J1)%MJD_RANGE(1)) .AND. &
     &           (MJD .LE. STP%STA(IND_STA)%TSYS(J1)%MJD_RANGE(2))       ) THEN 
                 MJD_CHK = 1
!
! ------------ Third check if the TAI also within range
!
!
! --------------- Get The TSYS value
!                   
                  IDX = IXMN8_S ( 1, STP%STA(IND_STA)%TSYS(J1)%NEL,              &
     &                       STP%STA(IND_STA)%TSYS(J1)%TSYS_ELEV, ELEV )
!
! --------------- Interpolate or extrapolate based on the value of IDX
! --------------- Is the node we are looking at smaller than the smallest
!                 smallest element of the observed elevations? Then use
!                 Tsys at initial element.
!
                  IF ( IDX == -2 ) THEN                 
                     TSYS_VAL = STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,1)
!
! --------------- Is the node we are looking at larger than the largest
!                 element of the observed elevations? Then use Tsys at
!                 last element
!
                  ELSEIF ( IDX == -1 ) THEN
                     TSYS_VAL = STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,           &
     &                   STP%STA(IND_STA)%TSYS(J1)%NEL)
!
! --------------- If observation angle is within stp file angle 
!                 boundaries, then interpolate.

                  ELSE 
!
! ------------------ Is the desired node already observed?
!
                     IF ( ELEV == STP%STA(IND_STA)%TSYS(J1)%TSYS_ELEV(IDX) ) THEN
                        TSYS_VAL = STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,IDX)
!
! ------------------ Or is it between the successive observations?
!
                     ELSE
                        DELTA_ELV  = STP%STA(IND_STA)%TSYS(J1)%TSYS_ELEV(IDX+1)  &
     &                       - STP%STA(IND_STA)%TSYS(J1)%TSYS_ELEV(IDX)
! ---------------------
                        DELTA_TSYS =                                    &
     &                    STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,IDX + 1)  -      &
     &                    STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,IDX)
! ---------------------
                        RATIO      =  DELTA_TSYS/DELTA_ELV
! ---------------------
                        TSYS_VAL = STP%STA(IND_STA)%TSYS(J1)%TSYS_VALS(1,IDX) +  &
     &                    RATIO*(ELEV - STP%STA(IND_STA)%TSYS(J1)%TSYS_ELEV(IDX))
                     END IF
                  END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$               
!%TAI%!               ELSE
!%TAI%!                  GO TO 110
!%TAI%!               END IF
            END IF
         END IF
!
! ------ If you have gone through the whole file and no Tsys was not 
!        within range, then use the lowest
!        
         IF ( J1 .EQ. STP%STA(IND_STA)%NTSYS ) THEN
!
! --------- Were any of the frequencies within range?
!
            IF ( FRQ_CHK .EQ. 0 ) THEN
                 CALL CLRCH ( STR )
                 WRITE ( UNIT=STR, FMT='(1PD12.5)' ) FREQ
                 CALL ERR_LOG ( 3251, IUER, 'STP_SEFD', 'The observation '// &
     &               'frequency '//TRIM(STR)//' is out of range for any '// &
     &               'of the Tsys frequency bands in STP file for '// &
     &               STP%STA(IND_STA)%NAME )
                 RETURN
            ELSE
!
! ------------ No MJD is within range, then take the one which is 
!              closest within the right frequency.
!
               IF ( MJD_CHK .EQ. 0 ) THEN
!
! -------------- Go again through Tsys to find the closest area
!
                  DO 130 J2 = 1, STP%STA(IND_STA)%NTSYS
!
! ------------------ Check if this is the appropriate frequency
!
                     IF ( (FREQ .GE. STP%STA(IND_STA)%TSYS(J2)%FRQ_RANGE(1)) .AND.   &
     &                    (FREQ .LE. STP%STA(IND_STA)%TSYS(J2)%FRQ_RANGE(2)) ) THEN 
!
! --------------------- Find closest MJD
! --------------------- Is the observation MJD lower than Maximum Range?
!
                        IF ( MJD .LT. STP%STA(IND_STA)%TSYS(J2)%MJD_RANGE(2) ) THEN 
!
! ------------------------ Get The TSYS value
!                   
                           IDX = IXMN8_S ( 1, STP%STA(IND_STA)%TSYS(J2)%NEL,     &
     &                           STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV, ELEV )
!
! ------------------------ Interpolate or extrapolate based on the 
!                          value of IDX
! ------------------------ Is the node we are looking at smaller than 
!                          the smallest element of the observed 
!                          elevations? Then use Tsys at initial element.
!
                           IF ( IDX == -2 ) THEN
                              TSYS_VAL = STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,1)
!
! ------------------------ Is the node we are looking at larger than the 
!                          largest element of the observed elevations? 
!                          Then use Tsys at last element
!
                           ELSEIF ( IDX == -1 ) THEN
                              TSYS_VAL = STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,  &
     &                            STP%STA(IND_STA)%TSYS(J2)%NEL)
!
! ------------------------ If observation angle is within stp file angle 
!                          boundaries, then interpolate.

                           ELSE 
!
! --------------------------- Is the desired node already observed?
!
                              IF ( ELEV ==                              &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX))     &
     &                                                             THEN
                                 TSYS_VAL = STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS( &
     &                               1,IDX)
!
! --------------------------- Or is it between the successive observations?     
!
                              ELSE
                                 DELTA_ELV =                            &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX + 1)  &
     &                             - STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX)
! ------------------------------
                                 DELTA_TSYS =                           &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX +1) &
     &                             - STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX)
! ------------------------------
                                 RATIO  =  DELTA_TSYS/DELTA_ELV
! ------------------------------
                                 TSYS_VAL =                             &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX) +  &
     &                             RATIO*( ELEV -                       &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX) )
                              END IF
                           END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --------------------- Is the Observation MJD higher that Maximum Range?
!
                        ELSE 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------------------ Get The TSYS value
!                   
                           IDX = IXMN8_S ( 1, STP%STA(IND_STA)%TSYS(J2)%NEL,         &
     &                           STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV, ELEV )
!
! ------------------------ Interpolate or extrapolate based on the 
!                          value of IDX
! ------------------------ Is the node we are looking at smaller than 
!                          the smallest element of the observed 
!                          elevations? Then use Tsys at initial element.
!
                           IF ( IDX == -2 ) THEN
                              TSYS_VAL = STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,1)
!
! ------------------------ Is the node we are looking at larger than the 
!                          largest element of the observed elevations? 
!                          Then use Tsys at last element
!
                           ELSEIF ( IDX == -1 ) THEN
                              TSYS_VAL =                                &
     &                            STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS( 1,        &
     &                            STP%STA(IND_STA)%TSYS(J2)%NEL)
!
! ------------------------ If observation angle is within stp file angle 
!                          boundaries, then interpolate.

                           ELSE 
!
! --------------------------- Is the desired node already observed?
!
                              IF ( ELEV              .EQ.               &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX) )THEN
                                 TSYS_VAL =                             &
     &                               STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX)
!
! --------------------------- Or is it between the successive observations?     
!
                              ELSE
                                 DELTA_ELV =                            &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX + 1)  &
     &                             - STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX)
! ------------------------------
                                 DELTA_TSYS =                           &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX + 1)&
     &                             - STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX)
! ------------------------------
                                 RATIO  =  DELTA_TSYS/DELTA_ELV
! ------------------------------
                                 TSYS_VAL =                             &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_VALS(1,IDX) +  &
     &                             RATIO*( ELEV -                       &
     &                             STP%STA(IND_STA)%TSYS(J2)%TSYS_ELEV(IDX))
                              END IF
                           END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                        END IF
                     END IF
 130              CONTINUE
               END IF  ! if mjd_chk .eq. 0
            END IF ! if frq_chk .eq. 0
         ELSE
           GO TO 110
         END IF  ! if j1 .eq. stp%ntsys

 110  CONTINUE
!
! --- Find the Gain with a date range includes the observation date
!
      MJD_CHK = 0
      TAI_CHK = 0
      FRQ_CHK = 0
!
      DO 210 J1 = 1, STP%STA(IND_STA)%NGAIN
!
! ------ First check if you are looking at the right frequency range
!
         IF ( (FREQ .GE. STP%STA(IND_STA)%GAIN(J1)%FRQ_RANGE(1)) .AND.           &
     &        (FREQ .LE. STP%STA(IND_STA)%GAIN(J1)%FRQ_RANGE(2)) ) THEN 
            FRQ_CHK = 1
!
! --------- Second check if the MJD is within range
!
            IF ( (MJD .GE. STP%STA(IND_STA)%GAIN(J1)%MJD_RANGE(1)) .AND.         &
     &           (MJD .LE. STP%STA(IND_STA)%GAIN(J1)%MJD_RANGE(2)) ) THEN 
               MJD_CHK = 1
!
! --------------- Get The GAIN value
!                   
                  IDX = IXMN8_S ( 1, STP%STA(IND_STA)%GAIN(J1)%NEL,              &
     &                       STP%STA(IND_STA)%GAIN(J1)%GAIN_ELEV, ELEV )
!
! --------------- Interpolate or extrapolate based on the value of IDX
! --------------- Is the node we are looking at smaller than the smallest
!                 smallest element of the observed elevations? Then use
!                 Gain at initial element.
!
                  IF ( IDX == -2 ) THEN
                     GAIN_VAL = STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS(1,1)
!
! --------------- Is the node we are looking at larger than the largest
!                 element of the observed elevations? Then use Gain at
!                 last element
!
                  ELSEIF ( IDX == -1 ) THEN
                     GAIN_VAL =                                         &
     &                  STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS( 1,                  &
     &                  STP%STA(IND_STA)%GAIN(J1)%NEL)
!
! --------------- If observation angle is within stp file angle 
!                 boundaries, then interpolate.

                  ELSE 
!
! ------------------ Is the desired node already observed?
!
                     IF ( ELEV == STP%STA(IND_STA)%GAIN(J1)%GAIN_ELEV(IDX) ) THEN
                        GAIN_VAL = STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS(1,IDX)
!
! ------------------ Or is it between the successive observations?
!
                     ELSE
                        DELTA_ELV  = STP%STA(IND_STA)%GAIN(J1)%GAIN_ELEV(IDX + 1) &
     &                    - STP%STA(IND_STA)%GAIN(J1)%GAIN_ELEV(IDX)
! ---------------------
                        DELTA_GAIN =                                    &
     &                    STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS(1,IDX + 1)  -      &
     &                    STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS(1,IDX) 
! ---------------------
                        RATIO      =  DELTA_GAIN/DELTA_ELV
! ---------------------
                        GAIN_VAL = STP%STA(IND_STA)%GAIN(J1)%GAIN_VALS(1,IDX) +  &
     &                    RATIO*(ELEV - STP%STA(IND_STA)%GAIN(J1)%GAIN_ELEV(IDX))
                     END IF
                  END IF
            END IF
         END IF
!
! ------ If you have gone through the whole file and no Gain was not 
!        within range, then use the lowest
!
         IF ( J1 .EQ. STP%STA(IND_STA)%NGAIN ) THEN
!
! --------- Were any of the frequencies within range?
!
            IF ( FRQ_CHK .EQ. 0 ) THEN
                 WRITE ( 6, * ) 'STP%STA(IND_STA)%NGAIN= ', STP%STA(IND_STA)%NGAIN
                 WRITE ( 6, '(A, 2X, 1PD15.7)' ) 'FREQ= ', FREQ 
                 CALL ERR_LOG ( 3252, IUER, 'STP_SEFD', 'The observation '// &
     &               'frequency is out of range for any of the Gains '//     &
     &               'frequency bands in STP file for '//STP%STA(IND_STA)%NAME )
                 RETURN
            ELSE
!
! ------------ No MJD is within range, then take the one which is 
!              closest within the right frequency.
!
               IF ( MJD_CHK .EQ. 0 ) THEN
!
! -------------- Go again through Gain to find the closest area
!
                  DO 230 J2 = 1, STP%STA(IND_STA)%NGAIN
!
! ------------------ Check if this is the appropriate frequency
!
                     IF ( (FREQ .GE. STP%STA(IND_STA)%GAIN(J2)%FRQ_RANGE(1)) .AND.   &
     &                    (FREQ .LE. STP%STA(IND_STA)%GAIN(J2)%FRQ_RANGE(2)) ) THEN 
!
! --------------------- Find closest MJD
! --------------------- Is the observation MJD lower than Maximum Range?
!
                        IF ( MJD .LT. STP%STA(IND_STA)%GAIN(J2)%MJD_RANGE(2) ) THEN 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------------------ Get The GAIN value
!                   
                           IDX = IXMN8_S ( 1, STP%STA(IND_STA)%GAIN(J2)%NEL,         &
     &                           STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV, ELEV )
!
! ------------------------ Interpolate or extrapolate based on the 
!                          value of IDX
! ------------------------ Is the node we are looking at smaller than 
!                          the smallest element of the observed 
!                          elevations? Then use Gain at initial element.
!
                           IF ( IDX == -2 ) THEN
                              GAIN_VAL = STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,1)
!
! ------------------------ Is the node we are looking at larger than the 
!                          largest element of the observed elevations? 
!                          Then use Gain at last element
!
                           ELSEIF ( IDX == -1 ) THEN
                              GAIN_VAL =                                &
     &                          STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,               &
     &                          STP%STA(IND_STA)%GAIN(J2)%NEL)
!
! ------------------------ If observation angle is within stp file angle 
!                          boundaries, then interpolate.
!
                           ELSE 
!
! --------------------------- Is the desired node already observed?
!
                              IF ( ELEV .EQ.                            &
     &                             STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX) )THEN
                                 GAIN_VAL =                             &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX)
!
! --------------------------- Or is it between the successive observations?     
!
                              ELSE
                                 DELTA_ELV =                            &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX + 1) &
     &                              - STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX)
! ------------------------------
                                 DELTA_GAIN =                           &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX+1) &
     &                               - STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX) 
! ------------------------------
                                 RATIO  =  DELTA_GAIN/DELTA_ELV
! ------------------------------
                                 GAIN_VAL =                             &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX) + &
     &                              RATIO*( ELEV -                      &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX) )
                              END IF
                           END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --------------------- Is the Observation MJD higher that Maximum Range?
!
                        ELSE 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------------------------ Get The GAIN value
!                   
                           IDX = IXMN8_S ( 1, STP%STA(IND_STA)%GAIN(J2)%NEL,         &
     &                           STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV, ELEV )
!
! ------------------------ Interpolate or extrapolate based on the 
!                          value of IDX
! ------------------------ Is the node we are looking at smaller than 
!                          the smallest element of the observed 
!                          elevations? Then use Gain at initial element.
!
                           IF ( IDX == -2 ) THEN
                              GAIN_VAL = STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,1)
!
! ------------------------ Is the node we are looking at larger than the 
!                          largest element of the observed elevations? 
!                          Then use Gain at last element
!
                           ELSEIF ( IDX == -1 ) THEN
                              GAIN_VAL =                                &
     &                          STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS( 1,              &
     &                          STP%STA(IND_STA)%GAIN(J2)%NEL )
!
! ------------------------ If observation angle is within stp file angle 
!                          boundaries, then interpolate.
!
                           ELSE 
!
! --------------------------- Is the desired node already observed?
!
                              IF ( ELEV             .EQ.   &
     &                             STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX) )THEN
                                 GAIN_VAL =                             &
     &                               STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX)
!
! --------------------------- Or is it between the successive observations?     
!
                              ELSE
                                 DELTA_ELV =                            &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX + 1) &
     &                               - STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX)
! ------------------------------
                                 DELTA_GAIN =                           &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX+1) &
     &                              - STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX) 
! ------------------------------
                                 RATIO  =  DELTA_GAIN/DELTA_ELV
! ------------------------------
                                 GAIN_VAL =                             &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_VALS(1,IDX) + &
     &                              RATIO*( ELEV -                      &
     &                              STP%STA(IND_STA)%GAIN(J2)%GAIN_ELEV(IDX) )
                              END IF
                           END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                        END IF
                     END IF
 230              CONTINUE
               END IF  ! if mjd_chk .eq. 0
            END IF ! if frq_chk .eq. 0
         ELSE
           GO TO 210
         END IF  ! if j1 .eq. stp%ngain

 210  CONTINUE
!
! --- Compute the SEFD
!
      SEFD = TSYS_VAL/GAIN_VAL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE
