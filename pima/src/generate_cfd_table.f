      SUBROUTINE GENERATE_CFD_TABLE ( VIS, MAP, MODE, FL_WEI_USE, CUTOFF_NERR, &
     &                                IMAGE_NAME, IMAGE_BAND, &
     &                                IMAGE_DATE, L_OBS, L_SCA, FLUX_INT, &
     &                                FLUX_SHR, FLUX_MID, FLUX_UNR, FLUX_NOI, &
     &                                MO, OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GENERATE_CFD_TABLE  computes various correlated flux      *
! *   density for a source observed with VLBA. The quantities are        *
! *   formatted in the outpuit table returned in array OUT.              *
! *                i                                                     *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    VIS ( VIS__TYPE    ) -- object which keeps variables related to   *
! *                            the visibility data for this source.      *
! *    MAP ( SOUMAP__TYPE ) -- object that keeps variables which         *
! *                            describe the image.                       *
! *    MODE ( INTEGER*4   ) -- code of the computation algorithm.        *
! *                            Reserved for future use.                  *
! *  FL_WEI_USE ( LOGICAL*4 ) -- If .TRUE., then weights from the        *
! *                              file with visibilities will be used     *
! *                              for averaging and computing the         *
! *                              statistical uncertainties. If .FALSE.,  *
! *                              then the weights 1.0 will used for      *
! *                              computations, provided the weight in    *
! *                              the visibility data is positive (and    *
! *                              the point will be discarded if weight   *
! *                              is negative).                           *
! * CUTOFF_NERR ( REAL*8    ) -- The points with the normalized          *
! *                              statistical uncertainties determined    *
! *                              as Err(Amp)/Amp exceeding CUTOFF_NERR   *
! *                              are flagged out and removed from the    *
! *                              output array. Here Err(Amp) is the      *
! *                              statistical error of the averaged       *
! *                              amplitude determined on the basis of    *
! *                              the scatter with respect to average and *
! *                              Amp is the averaged amplitude.          *
! *   MO       ( INTEGER*4 ) -- The number of lines in the output array. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * IMAGE_NAME ( CHARACTER ) -- Name of the source.                      *
! * IMAGE_BAND ( CHARACTER ) -- Code of the observed band: Q,K,U,X,C,S,L *
! * IMAGE_DATE ( CHARACTER ) -- Date of the observation. Only with 1 day *
! *                             accuracy.                                *
! *      L_OBS ( INTEGER*4 ) -- The number of used observations.         *
! *      L_SCA ( INTEGER*4 ) -- The number of used scans.                *
! *   FLUX_INT ( REAL*8    ) -- Intetgral flux densities. This quantity  *
! *                             is computed by summing amplitudes of     *
! *                             all components represented by the        *
! *                             delta-functions. Units: Jy.              *
! *   FLUX_SHR ( REAL*8    ) -- Correlated flux density at short         *
! *                             baselines. This quantity is computed as  *
! *                             the median over calibrated visibilities  *
! *                             at baseline with projectsions shorter    *
! *                             than some limit. Units: Jy.              *
! *   FLUX_MID ( REAL*8    ) -- Correlated  flux density at the          *
! *                             baseline lenghts between short and long. *
! *   FLUX_UNR ( REAL*8    ) -- Correlated  flux density of the          *
! *                             unresolved component. This quantity is   *
! *                             computed as the median over calibrated   *
! *                             visibilities at baseline with            *
! *                             projectsions shorter than some limit.    *
! *                             Units: Jy.                               *
! *   FLUX_NOI ( REAL*8    ) -- The rms of the image noise. Units: Jy.   *
! *   OUT      ( CHARACTER ) -- The output array.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
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
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! * ## 31-JAN-2007  GENERATE_CFD_TABLE v3.1 (c) L. Petrov 06-JAN-2025 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      TYPE     ( VIS__TYPE    ) :: VIS
      INTEGER*4  MODE, MO, IUER
      CHARACTER  IMAGE_BAND*(*), IMAGE_DATE*(*), IMAGE_NAME*(*), OUT(MO)*(*)
      CHARACTER   STR*256
      INTEGER*4    M_POI
      PARAMETER  ( M_POI = 8192 )
      REAL*8     UV_LEN(M_POI), AMP(M_POI), EPS(M_POI), &
     &           FLUX_LOW_UVLEN, FLUX_HIGH_UVLEN, UVLEN_LOW, UVLEN_HIGH, &
     &           FLUX_LOW(M_POI), FLUX_BTW(M_POI), FLUX_HIG(M_POI), TAI_AVR(M_POI)
      REAL*8     C__VEL, BASLEN__LOW, BASLEN__HIGH, FLUX_MIN, FLUX_MAX
      PARAMETER  ( C__VEL = 299792458.0D0  )
      PARAMETER  ( BASLEN__LOW  = 1.0D6 )
      PARAMETER  ( BASLEN__HIGH = 5.0D6 )
      PARAMETER  ( FLUX_MIN = 0.00001 )
      PARAMETER  ( FLUX_MAX = 90.0    )
      LOGICAL*4  FL_WEI_USE, FL_AUTO, FL_ERROR
      REAL*8     CUTOFF_NERR
      INTEGER*4  LUN, J1, J2, J3, J4, L_OBS, L_SCA, L_LOW, L_BTW, L_HIG, &
     &           IND_BAS(M_POI), IER
      REAL*8     FLUX_INT, FLUX_SHR, FLUX_MID, FLUX_UNR, FLUX_NOI, &
     &           GAP_SCAN, PHI_MIN
      PARAMETER  (  GAP_SCAN = 300.0D0 ) 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_BAND*1
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!
! --- Computed the integral flux density
!
      FLUX_INT = 0.0D0
      DO 410 J1=1,MAP%NUM_CC
         FLUX_INT = FLUX_INT + MAP%FLUX_CC(J1)
 410  CONTINUE 
!
! --- Compute the rms of the image noise
!
      CALL FIND_IMAGE_NOISE ( MAP )
      FLUX_NOI = MAP%NOISE
!
! --- Set bounds for the length of the baseline projection
!
      IMAGE_BAND = GET_BAND ( MAP%FREQ )
      UVLEN_HIGH = BASLEN__HIGH/C__VEL*MAP%FREQ 
      UVLEN_LOW  = BASLEN__LOW /C__VEL*MAP%FREQ 
!
!      IF ( IMAGE_BAND == '?' ) THEN
!           WRITE ( 6, * )' image_band = ', image_band,' freq =', map%freq
!           call exit ( 1 )
!      END IF
!
! --- Computed the number of scans, the averated amplitude and averaged uv
! --- coordinates
!
      CALL ERR_PASS ( IUER, IER )
      FL_AUTO = .FALSE.
      PHI_MIN = 0.0D0
      CALL GET_UVLEN_AMP ( VIS, GAP_SCAN, FL_WEI_USE, FL_AUTO, CUTOFF_NERR, M_POI, &
     &                     L_OBS, L_SCA, UV_LEN, AMP, EPS, TAI_AVR, &
     &                     IND_BAS, 0, PHI_MIN, ' ', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4512, IUER, 'GENERATE_CFD_TABLE', 'Error in an '// &
     &         'attempt to compute the avereged UV length and correlated '// &
     &         'flux density for source '//VIS%SOU_NAME )
           RETURN
      END IF
!
! --- Compute arrays of visibility amplitudes at lenghts of baseline 
! --- projections lower and upper than limits
!
      L_LOW = 0
      L_BTW = 0
      L_HIG = 0
      DO 420 J2=1,L_OBS
         IF ( UV_LEN(J2) < UVLEN_LOW ) THEN
              L_LOW = L_LOW + 1
              FLUX_LOW(L_LOW) = AMP(J2)
         END IF
         IF ( UV_LEN(J2) .GE. UVLEN_LOW .AND. &
     &        UV_LEN(J2) .LE. UVLEN_HIGH ) THEN
              L_BTW = L_BTW + 1
              FLUX_BTW(L_BTW) = AMP(J2)
         END IF
         IF ( UV_LEN(J2) > UVLEN_HIGH ) THEN
              L_HIG = L_HIG + 1
              FLUX_HIG(L_HIG) = AMP(J2)
         END IF
 420  CONTINUE 
!
      IF ( L_LOW == 0 ) THEN
           FLUX_SHR = -1.0
         ELSE
!
! -------- Sort array of visibility amplitudes at short lengths of the 
! -------- baseline projection
!
           CALL SORT_R8 ( L_LOW, FLUX_LOW )
!
! -------- Get the median value
!
           IF ( MOD(L_LOW,2) == 0 ) THEN
                FLUX_SHR = ( FLUX_LOW(L_LOW/2) + FLUX_LOW(L_LOW/2+1) )/2.0D0
              ELSE
                FLUX_SHR = FLUX_LOW( (L_LOW+1)/2 )
           END IF
      END IF
!
      IF ( L_BTW == 0 ) THEN
           FLUX_MID = -1.0
         ELSE
!
! -------- Sort array of visibility amplitudes at long lengths of the 
! -------- baseline projection
!
           CALL SORT_R8 ( L_BTW, FLUX_BTW )
!
! -------- Get the median value
!
           IF ( MOD(L_BTW,2) == 0 ) THEN
                FLUX_MID = ( FLUX_BTW(L_BTW/2) + FLUX_BTW(L_BTW/2+1) )/2.0D0
              ELSE
                FLUX_MID =   FLUX_BTW( (L_BTW+1)/2 )
           END IF
      END IF
!
      IF ( L_HIG == 0 ) THEN
           FLUX_UNR = -1.0
         ELSE
!
! -------- Sort array of visibility amplitudes at long lengths of the 
! -------- baseline projection
!
           CALL SORT_R8 ( L_HIG, FLUX_HIG )
!
! -------- Get the median value
!
           IF ( MOD(L_HIG,2) == 0 ) THEN
                FLUX_UNR = ( FLUX_HIG(L_HIG/2) + FLUX_HIG(L_HIG/2+1) )/2.0D0
              ELSE
                FLUX_UNR = FLUX_HIG( (L_HIG+1)/2 )
           END IF
      END IF
!
      IMAGE_NAME = MAP%SOU_NAME
!
! --- Get the image date
!
      IER = -1
      STR = MJDSEC_TO_DATE ( MAP%MJD, MAP%TAI, IER )
      IMAGE_DATE = STR(1:10)
      IMAGE_DATE(5:5) = '_'
      IMAGE_DATE(8:8) = '_'
!
      OUT(1) = '#Sour_name B YYYY_MM_DD  Fl_int  Fl_shr  Fl_med  Fl_unr  Fl_noi(Jy) #obs. #sca  mode:   '
      CALL INCH  ( MODE, OUT(1)(88:88) )
!
      CALL CLRCH ( OUT(2) )
      OUT(2)(1:10)  = IMAGE_NAME
      OUT(2)(12:12) = IMAGE_BAND
      OUT(2)(14:23) = IMAGE_DATE
      WRITE ( UNIT=OUT(2)(26:31), FMT='(F6.3)' ) FLUX_INT
      WRITE ( UNIT=OUT(2)(34:39), FMT='(F6.3)' ) FLUX_SHR
      WRITE ( UNIT=OUT(2)(42:47), FMT='(F6.3)' ) FLUX_MID
      WRITE ( UNIT=OUT(2)(50:55), FMT='(F6.3)' ) FLUX_UNR
      WRITE ( UNIT=OUT(2)(58:63), FMT='(F6.4)' ) FLUX_NOI 
      WRITE ( UNIT=OUT(2)(69:72), FMT='(I4)'   ) L_OBS
      WRITE ( UNIT=OUT(2)(75:79), FMT='(I4)'   ) L_SCA
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GENERATE_CFD_TABLE  !#!#
