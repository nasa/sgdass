      SUBROUTINE GET_UV_AVR ( VIS, GAP_SCAN, M_POI, L_POI, L_SCA, UV_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_UV_AVR  analyzes the array of calibrated              *
! *   visibilities, splits the data into scans, and averages the UV      *
! *   coordaintes over channels and over time within one scan            *
! *   at one baseline.                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VIS ( VIS__TYPE ) -- object which keeps variables related to    *
! *                           the visibility data for this source.       *
! * GAP_SCAN ( REAL*8    ) -- The maximum gap in the data within one     *
! *                           scan. The data points with time tag        *
! *                           greater than GAP_SCAN are considered       *
! *                           to belonging to different scans.           *
! *    M_POI ( INTEGER*4 ) -- The maximum number of points of the output *
! *                           arrays.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_POI ( INTEGER*4 ) -- The number of points of the averaged       *
! *                           visibility data.                           *
! *    L_SCA ( INTEGER*4 ) -- The number of scans.                       *
! *   UV_ARR ( REAL*4    ) -- Array of averaged UV lengths. Dimension:   *
! *                           (M_POI,2).                                 *
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
! *  ### 30-JAN-2007  GET_UV_AVR  v1.0 (c)  L. Petrov 30-JAN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      INTEGER*4  M_POI, L_POI, L_SCA, IUER
      REAL*8     GAP_SCAN
      REAL*4     UV_ARR(M_POI,2)
      INTEGER*4  M_SCA, M_BAS
      REAL*8     WEI_MIN
      PARAMETER  ( M_SCA = 8192 )
      PARAMETER  ( M_BAS = 1024 )
      PARAMETER  ( WEI_MIN = 1.D-15 )
      REAL*8     TAI_SCA(M_SCA)
      REAL*4     WW_BAS(M_BAS), UV_BAS(M_BAS,2), RR
      INTEGER*4  J1, J2, J3, J4, J5, IND_SCA(2,M_SCA), &
     &           MJD_SCA(M_SCA), IND_BAS(M_BAS), I_BAS, L_BAS, IER
      CHARACTER  STR*256
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ADD_LIS, IFIND_PL, ILEN, I_LEN
!
! --- Get the number of scans and their dates. 
! --- Create array of scan indexes IND_SCA and dates of the beginning of the
! --- scan
!
      L_SCA = 0
      CALL NOUT_I4 ( 2*L_SCA, IND_SCA )
      IF ( VIS%NP .LE. 0 ) THEN
         ELSE IF ( VIS%NP == 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           IND_SCA(2,L_SCA) = 1
         ELSE IF ( VIS%NP > 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           MJD_SCA(L_SCA) = VIS%MJD(1) 
           TAI_SCA(L_SCA) = VIS%TAI(1) 
!
! -------- Check all points
!
           DO 410 J1=2,VIS%NP
              IF ( ( VIS%MJD(J1) - VIS%MJD(J1-1) )*86400.0D0 + &
     &             ( VIS%TAI(J1) - VIS%TAI(J1-1) ) > GAP_SCAN  ) THEN
!
! ---------------- The gap between points is grteater than GAP_SCAN
!
                   IND_SCA(2,L_SCA) = J1-1
                   L_SCA = L_SCA + 1        ! increment the scan copunter
                   IND_SCA(1,L_SCA) = J1
                   MJD_SCA(L_SCA) = VIS%MJD(J1) 
                   TAI_SCA(L_SCA) = VIS%TAI(J1) 
              END IF
 410       CONTINUE 
           IND_SCA(2,L_SCA) = VIS%NP
      END IF
!
! --- Compute the correlated flux density averaged over scans and over 
! --- intermediate frequencies
!
      L_POI = 0
      DO 420 J2=1,L_SCA  ! Cycle over scans
!
! ------ Initialization of accumulators
!
         CALL NOUT_R4 (   M_BAS, WW_BAS  )
         CALL NOUT_R4 ( 2*M_BAS, UV_BAS  )
         L_BAS = 0
!
! ------ Cycle over epochs of the scan
!
         DO 430 J3=IND_SCA(1,J2),IND_SCA(2,J2)
!
! --------- ... and over intermediate frequencies of the scan
!
!
! --------- Get the baseline index
!
            CALL ERR_PASS ( IUER, IER )
            I_BAS = ADD_LIS ( M_BAS, L_BAS, IND_BAS, VIS%IND_BAS(J3), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( M_BAS, STR )
                 CALL ERR_LOG ( 1611, IUER, 'GET_UV_AVR ', &
     &               'Trap of internal control: too many baselines. '// &
     &               'Increase parameters M_BAS: '//STR )
                 RETURN 
            END IF
!
            DO 440 J4=1,VIS%NFRQ
               IF ( VIS%WEI(J4,J3) > WEI_MIN ) THEN
!
! ----------------- Only points with weights more than a limit are used
!
! ----------------- Update accumulators
!
                    UV_BAS(I_BAS,1) = UV_BAS(I_BAS,1) + VIS%UV(1,J4,J3)
                    UV_BAS(I_BAS,2) = UV_BAS(I_BAS,2) + VIS%UV(2,J4,J3)
                    WW_BAS(I_BAS)   = WW_BAS(I_BAS)   + 1.0
!@  rr = (VIS%UV(2,J4,J3) - UV_BAS(I_BAS,2)/WW_BAS(I_BAS))*1.d-6  ! %%%
!@  if ( rr > 1.0 ) write ( 6, * ) ' uv_bas_dif =', rr ! %%%
               END IF
 440        CONTINUE 
 430     CONTINUE 
!
         IF ( L_BAS > 0 ) THEN
!
! ----------- Cycle over baselines. Compute the averaged uv coordinates
!
              DO 450 J5=1,L_BAS
                 IF ( WW_BAS(J5) > WEI_MIN ) THEN
                      L_POI = L_POI + 1
                      IF ( L_POI > M_POI ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( L_POI, STR )
                           CALL ERR_LOG ( 1612, IUER, 'GET_UV_AVR ', &
     &                         'Trap of internal control: too many '// &
     &                         'points. Increase parameters M_POI: '//STR )
                           RETURN 
                      END IF
!        
                      UV_ARR(L_POI,1) = UV_BAS(J5,1)/WW_BAS(J5)
                      UV_ARR(L_POI,2) = UV_BAS(J5,2)/WW_BAS(J5)
                 END IF
 450          CONTINUE 
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_UV_AVR  !#!#
