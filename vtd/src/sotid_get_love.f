      SUBROUTINE SOTID_GET_LOVE ( TIDCNF, L_DEG, M_ORD, FRQ, LOVE_VEC, IUER )
! ************************************************************************
! *                                                                      *
! *   You shouldn't interpret the name of this routine literally.        *
! *   SOTID_GET_LOVE  returns vector of generalized Love numbers of L-th *
! *   degree, M-th order and frequency FRQ for the model specified in    *
! *   object TIDCNF.                                                     *
! *                                                                      *
! *   LOVE_VEC(1) = h(0) -- Principal Love number;                       *
! *   LOVE_VEC(2) = h(i) -- Out-of-phase radial Love number;             *
! *   LOVE_VEC(3) = h(2) -- Latitude Love number following Mathews       *
! *                         notation;                                    *
! *   LOVE_VEC(4) = h'   -- Additional zero degree Love number;          *
! *   LOVE_VEC(5) = l(0) -- Principal Shida number;                      *
! *   LOVE_VEC(6) = l(i) -- Out-of-phase Shida number;                   *
! *   LOVE_VEC(7) = l(1) -- Second degree toroidal Love number;          *
! *   LOVE_VEC(8) = l(2) -- Latitude Shida number following Mathews      *
! *                         notation;                                    *
! *   LOVE_VEC(9) = l'   -- First degree toroidal Shida number.          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    TIDCNF ( RECORD     ) -- Object which holds configuration         *
! *                             parameters of SOTID.                     *
! *     L_DEG ( INTEGER*4  ) -- Degree of the tidal wave. Currently only *
! *                             degree 2 is supported.                   *
! *     M_ORD ( INTEGER*4  ) -- Order of the tide. 0,1,2 are supported.  *
! *       FRQ ( REAL*8     ) -- Frequency of the tidal wave in rad/sec.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  LOVE_VEC ( REAL*8     ) -- Vector of the generalized Love numbers.  *
! *                             Dimension: SOTID__NLOVE.                 *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 16-JUL-1998  SOTID_GET_LOVE  v2.2 (c) L. Petrov  05-JUL-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'sotid_type.i'
      INCLUDE   'sotid_data.i'
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      INTEGER*4  L_DEG, M_ORD, IUER
      REAL*8     FRQ, LOVE_VEC(SOTID__NLOVE), FRQ_USE
      COMPLEX*16 LV_RES(6), CFRQ
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14
!
! --- Initilaize generalized Love numbers with zeros
!
      CALL NOUT_R8 ( SOTID__NLOVE, LOVE_VEC )
!
! --- Set "used frequency" -- if it is lower than the limit
!
      FRQ_USE = FRQ
!
      IF ( L_DEG .EQ. 2 ) THEN
           IF ( M_ORD .EQ. 0  .AND. &
     &          ( TIDCNF%ORDER_2D .EQ. SOTID__2D_0ORD   .OR. &
     &            TIDCNF%ORDER_2D .EQ. SOTID__2D_01ORD  .OR. &
     &            TIDCNF%ORDER_2D .EQ. SOTID__2D_02ORD  .OR. &
     &            TIDCNF%ORDER_2D .EQ. SOTID__2D_012ORD      ) ) THEN
                IF ( FRQ_USE .LT. SOTID__LOW_FREQ_LIM ) THEN
!
! ------------------ Treatment of permanent tide
!
                     IF ( TIDCNF%ZF_LOVE .EQ. SOTID__ZF_ZERO ) THEN
!
! ----------------------- Zero
!
                          CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                          RETURN
                       ELSE IF ( TIDCNF%ZF_LOVE .EQ. SOTID__ZF_LOVE ) THEN
!
! ----------------------- Frequency independent Love numbers model
!
                          LOVE_VEC(1) = H2_LOVE
                          LOVE_VEC(5) = L2_LOVE
                          CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                          RETURN
                       ELSE IF ( TIDCNF%ZF_LOVE .EQ. SOTID__ZF_MDG97EL ) THEN
                          LOVE_VEC(1) =  0.6026D0
                          LOVE_VEC(3) = -0.0006D0
                          LOVE_VEC(5) =  0.0831D0
                          LOVE_VEC(8) =  0.0002D0
                          CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                          RETURN
                       ELSE IF ( TIDCNF%ZF_LOVE .EQ. SOTID__ZF_MDG97AN ) THEN
                          LOVE_VEC(1) = H22R_MAT00  !  0.6078D0
                          LOVE_VEC(3) = H2Z_MAT00   ! -0.0006D0
                          LOVE_VEC(5) = L22R_MAT00  !  0.0847D0
                          LOVE_VEC(8) = L2Z_MAT00   !  0.0002D0
                          CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                          RETURN
                       ELSE IF ( TIDCNF%ZF_LOVE .EQ. SOTID__ZF_FLUID ) THEN
                          LOVE_VEC(1) = H2_FLUID
                          LOVE_VEC(5) = L2_FLUID
                          CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                          RETURN
                       ELSE
                          CALL CLRCH ( STR )
                          WRITE ( UNIT=STR, FMT='(I12)' ) TIDCNF%ZF_LOVE
                          CALL CHASHL ( STR )
                          CALL ERR_LOG ( 5731, IUER, 'SOTID_GET_LOVE', &
     &                        'Wrong argument TIDCNF%ZF_LOVE: '// &
     &                        'TIDCNF%ZF_LOVE='//STR )
                          RETURN
                     END IF ! tidcnf.zf_love
                END IF ! frq_use ?
!
! ------------- If the freqnuency is too low -- then set to to the lowest limit
!
                IF ( FRQ_USE .LT. SOTID__LOW_FREQ_LIM ) THEN
                     FRQ_USE = SOTID__LOW_FREQ_LIM  !  set low limit
                END IF
!
! ============= Long period tides
!
                IF ( TIDCNF%MODEL_2D .EQ. SOTID__LOVE ) THEN
!
! ------------------ Love model
!
                     LOVE_VEC(1) = H2_LOVE
                     LOVE_VEC(5) = L2_LOVE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97EL ) THEN
!
! ------------------ MDG97 elastic
!
                     DO 410 J1=1,SOTID__NLOVE
                        LOVE_VEC(J1) = LOVE_MDG97(J1,1)
 410                 CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97AN ) THEN
!
! ------------------ MDG97 Anelastic
!
                     DO 420 J2=1,SOTID__NLOVE
                        LOVE_VEC(J2) = LOVE_MDG97(J2,4)
 420                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = MDG97_LP_H0R(1) + &
     &                             MDG97_LP_H0R(2)*( DLOG(FRQ_USE) ) + &
     &                             MDG97_LP_H0R(3)*( DLOG(FRQ_USE) )**2
                     LOVE_VEC(2) = MDG97_LP_H0I(1) + &
     &                             MDG97_LP_H0I(2)*( DLOG(FRQ_USE) ) + &
     &                             MDG97_LP_H0I(3)*( DLOG(FRQ_USE) )**2
                     LOVE_VEC(5) = MDG97_LP_L0R(1) + &
     &                             MDG97_LP_L0R(2)*( DLOG(FRQ_USE) ) + &
     &                             MDG97_LP_L0R(3)*( DLOG(FRQ_USE) )**2
                     LOVE_VEC(6) = MDG97_LP_L0I(1) + &
     &                             MDG97_LP_L0I(2)*( DLOG(FRQ_USE) ) + &
     &                             MDG97_LP_L0I(3)*( DLOG(FRQ_USE) )**2
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99EH ) THEN
!
! ------------------ DDW99 elastic hydrostatic
!
                     DO 430 J3=1,SOTID__NLOVE
                        LOVE_VEC(J3) = LOVE_DDW99(J3,1)
 430                 CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99IN ) THEN
!
! ------------------ DDW99 inelastic non-hydrostatic
!
                     DO 440 J4=1,SOTID__NLOVE
                        LOVE_VEC(J4) = LOVE_DDW99(J4,1)
 440                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = DDW99_LP_H0R(1) + &
     &                             DDW99_LP_H0R(2)*( DLOG(FRQ_USE) ) + &
     &                             DDW99_LP_H0R(3)*( DLOG(FRQ_USE) )**2
                     LOVE_VEC(5) = DDW99_LP_L0R(1) + &
     &                             DDW99_LP_L0R(2)*( DLOG(FRQ_USE) ) + &
     &                             DDW99_LP_L0R(3)*( DLOG(FRQ_USE) )**2
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT00 ) THEN
!
! ------------------ P. Mathews 2000 Love numbers (resonance formula)
! ------------------ for IERS Conventions 2000, revision of 10-OCT-2001
!
                     LOVE_VEC(1) = H20R_MAT01 + &
     &                             (H20I_MAT01/DTAN(SOTID__P2I*ALPAN_MAT01))* &
     &                             (1.D0 - (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01 )
                     LOVE_VEC(2) = H20I_MAT01* &
     &                                 (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01
                     LOVE_VEC(3) = H2Z_MAT01
                     LOVE_VEC(4) = HPZ_MAT01
                     LOVE_VEC(5) = L20R_MAT01 + &
     &                             L20I_MAT01/DTAN(SOTID__P2I*ALPAN_MAT01)* &
     &                             (1.D0 - (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01 )
                     LOVE_VEC(6) = L20I_MAT01* &
     &                                 (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01
                     LOVE_VEC(8) = L2Z_MAT01
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT01 ) THEN
!
! ------------------ P. Mathews 2001 Love numbers (resonance formula)
! ------------------ from Mathews, P.M., Love numbers and gravimetric factor 
! ------------------ for diurnal tides, Journal of the Geodetic Society of 
! ------------------ Japan, 47(1), 231--236, 2001.
!
                     LOVE_VEC(1) = H20R_MAT01 + &
     &                             (H20I_MAT01/DTAN(SOTID__P2I*ALPAN_MAT01))* &
     &                             (1.D0 - (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01 )
                     LOVE_VEC(2) = H20I_MAT01* &
     &                                 (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01
                     LOVE_VEC(3) = H2Z_MAT01
                     LOVE_VEC(4) = HPZ_MAT01
                     LOVE_VEC(5) = L20R_MAT01 + &
     &                             L20I_MAT01/DTAN(SOTID__P2I*ALPAN_MAT01)* &
     &                             (1.D0 - (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01 )
                     LOVE_VEC(6) = L20I_MAT01* &
     &                                 (ANFRQ_MAT01/FRQ_USE)**ALPAN_MAT01
                     LOVE_VEC(8) = L2Z_MAT01
                END IF
           ELSE IF ( M_ORD .EQ. 1  .AND. &
     &       ( TIDCNF%ORDER_2D .EQ. SOTID__2D_01ORD  .OR. &
     &         TIDCNF%ORDER_2D .EQ. SOTID__2D_12ORD  .OR. &
     &         TIDCNF%ORDER_2D .EQ. SOTID__2D_012ORD      ) ) THEN
!
! ============= Diurnal tides
!
                IF ( TIDCNF%MODEL_2D .EQ. SOTID__LOVE ) THEN
!
! ------------------ Love model
!
                     LOVE_VEC(1) = H2_LOVE
                     LOVE_VEC(5) = L2_LOVE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97EL ) THEN
!
! ------------------ MDG97 elastic
!
                     DO 450 J5=1,SOTID__NLOVE
                        LOVE_VEC(J5) = LOVE_MDG97(J5,2)
 450                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = MDG97_DP_HER(1) + &
     &                             MDG97_DP_HER(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_HER(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(3) = MDG97_DP_HE2(1) + &
     &                             MDG97_DP_HE2(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_HE2(3)/MDG97_DP_FSC/ &
     &                                              (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(5) = MDG97_DP_LER(1) + &
     &                             MDG97_DP_LER(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LER(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(7) = MDG97_DP_LE1(1) + &
     &                             MDG97_DP_LE1(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LE1(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(9) = MDG97_DP_LEP(1) + &
     &                             MDG97_DP_LEP(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LEP(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97AN ) THEN
!
! ------------------ MDG97 anelastic
!
                     DO 460 J6=1,SOTID__NLOVE
                        LOVE_VEC(J6) = LOVE_MDG97(J6,5)
 460                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = MDG97_DP_HAR(1) + &
     &                             MDG97_DP_HAR(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_HAR(3)/MDG97_DP_FSC/ &
     &                                            (MDG97_DP_NDFWA - FRQ_USE)
                     LOVE_VEC(2) = MDG97_DP_HAI(1) + &
     &                             MDG97_DP_HAI(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_HAI(3)/MDG97_DP_FSC/ &
     &                                            (MDG97_DP_NDFWA - FRQ_USE)
                     LOVE_VEC(3) = MDG97_DP_HE2(1) + &
     &                             MDG97_DP_HE2(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_HE2(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(5) = MDG97_DP_LAR(1) + &
     &                             MDG97_DP_LAR(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LAR(3)/MDG97_DP_FSC/ &
     &                                            (MDG97_DP_NDFWA - FRQ_USE)
                     LOVE_VEC(6) = MDG97_DP_LAI(1) + &
     &                             MDG97_DP_LAI(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LAI(3)/MDG97_DP_FSC/ &
     &                                            (MDG97_DP_NDFWA - FRQ_USE)
                     LOVE_VEC(7) = MDG97_DP_LE1(1) + &
     &                             MDG97_DP_LE1(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LE1(3)/MDG97_DP_FSC/ &
     &                                            (MDG97_DP_NDFWE - FRQ_USE)
                     LOVE_VEC(9) = MDG97_DP_LEP(1) + &
     &                             MDG97_DP_LEP(2)*(FRQ_USE*MDG97_DP_FSC)**3 + &
     &                             MDG97_DP_LEP(3)/MDG97_DP_FSC/ &
     &                                             (MDG97_DP_NDFWE - FRQ_USE)
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99EH ) THEN
!
! ------------------ DDW99 elastic hydrostatic
!
                     DO 470 J7=1,SOTID__NLOVE
                        LOVE_VEC(J7) = LOVE_DDW99(J7,2)
 470                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = DDW99_DP_H0E(1) + &
     &                             DDW99_DP_H0E(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_H0E(3)/DDW99_DP_FSC/ &
     &                                            (DDW99_DP_NDFWE- FRQ_USE)
                     LOVE_VEC(3) = DDW99_DP_H2E(1) + &
     &                             DDW99_DP_H2E(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_H2E(3)/DDW99_DP_FSC/ &
     &                                            (DDW99_DP_NDFWE- FRQ_USE)
                     LOVE_VEC(5) = DDW99_DP_L0E(1) + &
     &                             DDW99_DP_L0E(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L0E(3)/DDW99_DP_FSC/ &
     &                                            (DDW99_DP_NDFWE- FRQ_USE)
                     LOVE_VEC(7) = DDW99_DP_L1E(1) + &
     &                             DDW99_DP_L1E(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L1E(3)/DDW99_DP_FSC/ &
     &                                            (DDW99_DP_NDFWE- FRQ_USE)
                     LOVE_VEC(8) = DDW99_DP_L2E(1) + &
     &                             DDW99_DP_L2E(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L2E(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWE- FRQ_USE)
                     LOVE_VEC(9) = DDW99_DP_LPE(1) + &
     &                             DDW99_DP_LPE(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_LPE(3)/DDW99_DP_FSC/ &
     &                                            (DDW99_DP_NDFWE- FRQ_USE)
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99IN ) THEN
!
! ------------------ DDW99 inelastic non-hydrostatic
!
                     DO 480 J8=1,SOTID__NLOVE
                        LOVE_VEC(J8) = LOVE_DDW99(J8,5)
 480                 CONTINUE
!
! ------------------ Frequency dependent numbers
!
                     LOVE_VEC(1) = DDW99_DP_H0I(1) + &
     &                             DDW99_DP_H0I(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_H0I(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                     LOVE_VEC(3) = DDW99_DP_H2I(1) + &
     &                             DDW99_DP_H2I(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_H2I(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                     LOVE_VEC(5) = DDW99_DP_L0I(1) + &
     &                             DDW99_DP_L0I(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L0I(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                     LOVE_VEC(7) = DDW99_DP_L1I(1) + &
     &                             DDW99_DP_L1I(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L1I(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                     LOVE_VEC(8) = DDW99_DP_L2I(1) + &
     &                             DDW99_DP_L2I(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_L2I(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                     LOVE_VEC(9) = DDW99_DP_LPI(1) + &
     &                             DDW99_DP_LPI(2)*(FRQ_USE*DDW99_DP_FSC)**3+ &
     &                             DDW99_DP_LPI(3)/DDW99_DP_FSC/ &
     &                                             (DDW99_DP_NDFWI- FRQ_USE)
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT00 ) THEN
!
! ------------------ P. Mathews 2000 Love numbers (resonance formula)
! ------------------ for IERS Conventions 2000, revision of 10-OCT-2001
!
                     LV_RES(1) = H0_MAT00(0)
                     LV_RES(2) = H2_MAT00(0)
                     LV_RES(3) = L0_MAT00(0)
                     LV_RES(4) = L1_MAT00(0)
                     LV_RES(5) = L2_MAT00(0)
                     LV_RES(6) = LP_MAT00(0)
                     CFRQ = DCMPLX ( FRQ/SIDFREQ_MAT00, 0.D0 )
                     DO 490 J9=1,3
                        LV_RES(1) = LV_RES(1) + H0_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
                        LV_RES(2) = LV_RES(2) + H2_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
                        LV_RES(3) = LV_RES(3) + L0_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
                        LV_RES(4) = LV_RES(4) + L1_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
                        LV_RES(5) = LV_RES(5) + L2_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
                        LV_RES(6) = LV_RES(6) + LP_MAT00(J9)/ &
     &                              ( CFRQ - FREQ_MAT00(J9) )
 490                 CONTINUE
!
                     LOVE_VEC(1) = DREAL ( LV_RES(1) )
                     LOVE_VEC(2) = DIMAG ( LV_RES(1) )
                     LOVE_VEC(3) = DREAL ( LV_RES(2) )
                     LOVE_VEC(5) = DREAL ( LV_RES(3) )
                     LOVE_VEC(6) = DIMAG ( LV_RES(3) )
                     LOVE_VEC(7) = DREAL ( LV_RES(4) )
                     LOVE_VEC(8) = DREAL ( LV_RES(5) )
                     LOVE_VEC(9) = DREAL ( LV_RES(6) )
!
! ------------------ Set of exceptions from the table
!
                     IF ( FRQ .GT. 6.2308D-5 .AND. FRQ .LT. 6.2320D-5 ) THEN
!
! ----------------------- 2Q1 (1)
!
                          LOVE_VEC(1) =  0.6039
                          LOVE_VEC(2) = -0.0027
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 6.2661D-5 .AND. FRQ .LT. 6.2673D-5 ) THEN
!
! ----------------------- SIG1 (2)
!
                          LOVE_VEC(1) =  0.6039
                          LOVE_VEC(2) = -0.0026
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 6.4947D-5 .AND. FRQ .LT. 6.4948D-5 ) THEN
!
! ----------------------- 134645 (3)
!
                          LOVE_VEC(1) =  0.6036
                          LOVE_VEC(2) = -0.0026
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 6.4958D-5 .AND. FRQ .LT. 6.4959D-5 ) THEN
!
! ----------------------- Q1 (4)
!
                          LOVE_VEC(1) =  0.6036
                          LOVE_VEC(2) = -0.0026
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0846
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 6.5301D-5 .AND. FRQ .LT. 6.5312D-5 ) THEN
!
! ----------------------- RO1 (5)
!
                          LOVE_VEC(1) =  0.6035
                          LOVE_VEC(2) = -0.0026
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 6.7587D-5 .AND. FRQ .LT. 6.7588D-5 ) THEN
!
! ----------------------- 145545 (6)
!
                          LOVE_VEC(1) =  0.6028
                          LOVE_VEC(2) = -0.0025
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0846
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 6.7597D-5 .AND. FRQ .LT. 6.7598D-5 ) THEN
!
! ----------------------- O1 (7)
!
                          LOVE_VEC(1) =  0.6028
                          LOVE_VEC(2) = -0.0025
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0846
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 6.7995D-5 .AND. FRQ .LT. 6.8007D-5 ) THEN
!
! ----------------------- TAU1 (8)
!
                          LOVE_VEC(1) =  0.6026
                          LOVE_VEC(2) = -0.0025
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 6.9873D-5 .AND. FRQ .LT. 6.9884D-5 ) THEN
!
! ----------------------- NTAU1 (9)
!
                          LOVE_VEC(1) =  0.6011
                          LOVE_VEC(2) = -0.0024
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 7.0271D-5 .AND. FRQ .LT. 7.0293D-5 ) THEN
!
! ----------------------- NO1=M1 (10)
!
                          LOVE_VEC(1) =  0.6005
                          LOVE_VEC(2) = -0.0023
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0847
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 7.0535D-5 .AND. FRQ .LT. 7.0646D-5 ) THEN
!
! ----------------------- CHI1 (11)
!
                          LOVE_VEC(1) =  0.5998
                          LOVE_VEC(2) = -0.0023
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 7.2323D-5 .AND. FRQ .LT. 7.2324D-5 ) THEN
!
! ----------------------- PI1 (12)
!
                          LOVE_VEC(1) =  0.5878
                          LOVE_VEC(2) = -0.0015
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 7.2512D-5 .AND. FRQ .LT. 7.2523D-5 ) THEN
!
! ----------------------- P1 (13)
!
                          LOVE_VEC(1) =  0.5817
                          LOVE_VEC(2) = -0.0011
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0853
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 7.2722D-5 .AND. FRQ .LT. 7.2723D-5 ) THEN
!
! ----------------------- S1 (14)
!
                          LOVE_VEC(1) =  0.5692
                          LOVE_VEC(2) = -0.0004
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 7.2910D-5 .AND. FRQ .LT. 7.2911D-5 ) THEN
!
! ----------------------- K1-nod (15)
!
                          LOVE_VEC(1) =  0.5283
                          LOVE_VEC(2) =  0.0023
                          LOVE_VEC(3) = -0.0007
                          LOVE_VEC(5) =  0.0869
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0011
                          LOVE_VEC(9) = -0.0003
                     END IF
!
                     IF ( FRQ .GT. 7.2921D-5 .AND. FRQ .LT. 7.2922D-5 ) THEN
!
! ----------------------- K1 (16)
!
                          LOVE_VEC(1) =  0.5236
                          LOVE_VEC(2) =  0.0030
                          LOVE_VEC(3) = -0.0007
                          LOVE_VEC(5) =  0.0870
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0011
                          LOVE_VEC(9) = -0.0003
                     END IF
!
                     IF ( FRQ .GT. 7.2931D-5 .AND. FRQ .LT. 7.2932D-5 ) THEN
!
! ----------------------- K1+nod (17)
!
                          LOVE_VEC(1) =  0.5182
                          LOVE_VEC(2) =  0.0036
                          LOVE_VEC(3) = -0.0007
                          LOVE_VEC(5) =  0.0872
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0011
                          LOVE_VEC(9) = -0.0003
                     END IF
!
                     IF ( FRQ .GT. 7.2942D-5 .AND. FRQ .LT. 7.2943D-5 ) THEN
!
! ----------------------- 166575 (18)
!
                          LOVE_VEC(1) =  0.5120
                          LOVE_VEC(2) =  0.0043
                          LOVE_VEC(3) = -0.0007
                     END IF
!
                     IF ( FRQ .GT. 7.3120D-5 .AND. FRQ .LT. 7.3121D-5 ) THEN
!
! ----------------------- PSI1 (19)
!
                          LOVE_VEC(1) =  1.0569
                          LOVE_VEC(2) =  0.0036
                          LOVE_VEC(3) = -0.0001
                          LOVE_VEC(5) =  0.0710
                          LOVE_VEC(6) = -0.0020
                          LOVE_VEC(7) =  0.0019
                          LOVE_VEC(9) =  0.0001
                     END IF
!
                     IF ( FRQ .GT. 7.3130D-5 .AND. FRQ .LT. 7.3131D-5 ) THEN
!
! ----------------------- 166564 (20)
!
                          LOVE_VEC(1) =  0.9387
                          LOVE_VEC(2) = -0.0050
                          LOVE_VEC(3) = -0.0003
                     END IF
!
                     IF ( FRQ .GT. 7.3319D-5 .AND. FRQ .LT. 7.3320D-5 ) THEN
!
! ----------------------- FI1 (21)
!
                          LOVE_VEC(1) =  0.6645
                          LOVE_VEC(2) = -0.0059
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0828
                          LOVE_VEC(6) = -0.0007
                          LOVE_VEC(7) =  0.0013
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 7.5207D-5 .AND. FRQ .LT. 7.5218D-5 ) THEN
!
! ----------------------- THE1 (22)
!
                          LOVE_VEC(1) =  0.6117
                          LOVE_VEC(2) = -0.0030
                          LOVE_VEC(3) = -0.0006
                     END IF
!
                     IF ( FRQ .GT. 7.5549D-5 .AND. FRQ .LT. 7.5572D-5 ) THEN
!
! ----------------------- J1 (23)
!
                          LOVE_VEC(1) =  0.6108
                          LOVE_VEC(2) = -0.0030
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0845
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
!
                     IF ( FRQ .GT. 7.8244D-5 .AND. FRQ .LT. 7.8266D-5 ) THEN
!
! ----------------------- OO1 (24)
!
                          LOVE_VEC(1) =  0.6080
                          LOVE_VEC(2) = -0.0028
                          LOVE_VEC(3) = -0.0006
                          LOVE_VEC(5) =  0.0846
                          LOVE_VEC(6) = -0.0006
                          LOVE_VEC(7) =  0.0012
                          LOVE_VEC(9) = -0.0002
                     END IF
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT01 ) THEN
!
! ------------------ P. Mathews 2001 Love numbers (resonance formula)
! ------------------ from Mathews, P.M., Love numbers and gravimetric factor 
! ------------------ for diurnal tides, Journal of the Geodetic Society of 
! ------------------ Japan, 47(1), 231--236, 2001.
!
                     LV_RES(1) = H0_MAT01(0)
                     LV_RES(2) = H2_MAT01(0)
                     LV_RES(3) = L0_MAT01(0)
                     LV_RES(4) = L1_MAT01(0)
                     LV_RES(5) = L2_MAT01(0)
                     LV_RES(6) = LP_MAT01(0)
                     CFRQ = DCMPLX ( FRQ/SIDFREQ_MAT01, 0.D0 )
                     DO 4100 J10=1,3
                        LV_RES(1) = LV_RES(1) + H0_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
                        LV_RES(2) = LV_RES(2) + H2_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
                        LV_RES(3) = LV_RES(3) + L0_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
                        LV_RES(4) = LV_RES(4) + L1_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
                        LV_RES(5) = LV_RES(5) + L2_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
                        LV_RES(6) = LV_RES(6) + LP_MAT01(J10)/ &
     &                              ( CFRQ - FREQ_MAT01(J10) )
 4100                 CONTINUE
!
                     LOVE_VEC(1) = DREAL ( LV_RES(1) )
                     LOVE_VEC(2) = DIMAG ( LV_RES(1) )
                     LOVE_VEC(3) = DREAL ( LV_RES(2) )
                     LOVE_VEC(5) = DREAL ( LV_RES(3) )
                     LOVE_VEC(6) = DIMAG ( LV_RES(3) )
                     LOVE_VEC(7) = DREAL ( LV_RES(4) )
                     LOVE_VEC(8) = DREAL ( LV_RES(5) )
                     LOVE_VEC(9) = DREAL ( LV_RES(6) )
!
! ------------------ Set of exceptions from the table
!
                     IF ( FRQ .GT. 6.2308D-5 .AND. FRQ .LT. 6.2320D-5 ) THEN
!
! ----------------------- 2Q1 (1)
!
                          LOVE_VEC(1) =  0.60391
                          LOVE_VEC(2) = -0.00265
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08450
                          LOVE_VEC(6) = -0.00055
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 6.2661D-5 .AND. FRQ .LT. 6.2673D-5 ) THEN
!
! ----------------------- SIG1 (2)
!
                          LOVE_VEC(1) =  0.60387
                          LOVE_VEC(2) = -0.00264
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08451
                          LOVE_VEC(6) = -0.00055
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 6.4958D-5 .AND. FRQ .LT. 6.4959D-5 ) THEN
!
! ----------------------- Q1 (3)
!
                          LOVE_VEC(1) =  0.60355
                          LOVE_VEC(2) = -0.00269
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08455
                          LOVE_VEC(6) = -0.00057
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 6.5301D-5 .AND. FRQ .LT. 6.5312D-5 ) THEN
!
! ----------------------- RO1 (4)
!
                          LOVE_VEC(1) =  0.60348
                          LOVE_VEC(2) = -0.00259
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08456
                          LOVE_VEC(6) = -0.00057
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 6.7597D-5 .AND. FRQ .LT. 6.7598D-5 ) THEN
!
! ----------------------- O1 (5)
!
                          LOVE_VEC(1) =  0.60278
                          LOVE_VEC(2) = -0.00252
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08462
                          LOVE_VEC(6) = -0.00058
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
                     IF ( FRQ .GT. 7.0271D-5 .AND. FRQ .LT. 7.0293D-5 ) THEN
!
! ----------------------- NO1=M1 (6)
!
                          LOVE_VEC(1) =  0.60047
                          LOVE_VEC(2) = -0.00234
                          LOVE_VEC(3) = -0.00062
                          LOVE_VEC(5) =  0.08473
                          LOVE_VEC(6) = -0.00059
                          LOVE_VEC(7) =  0.00120
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 7.2323D-5 .AND. FRQ .LT. 7.2324D-5 ) THEN
!
! ----------------------- PI1 (7)
!
                          LOVE_VEC(1) =  0.58777
                          LOVE_VEC(2) = -0.00144
                          LOVE_VEC(3) = -0.00063
                          LOVE_VEC(5) =  0.08515
                          LOVE_VEC(6) = -0.00059
                          LOVE_VEC(7) =  0.00119
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00023
                     END IF
!
                     IF ( FRQ .GT. 7.2512D-5 .AND. FRQ .LT. 7.2523D-5 ) THEN
!
! ----------------------- P1 (8)
!
                          LOVE_VEC(1) =  0.58169
                          LOVE_VEC(2) = -0.00104
                          LOVE_VEC(3) = -0.00063
                          LOVE_VEC(5) =  0.08534
                          LOVE_VEC(6) = -0.00059
                          LOVE_VEC(7) =  0.00118
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00024
                     END IF
!
                     IF ( FRQ .GT. 7.2722D-5 .AND. FRQ .LT. 7.2723D-5 ) THEN
!
! ----------------------- S1 (9)
!
                          LOVE_VEC(1) =  0.56920
                          LOVE_VEC(2) = -0.00041
                          LOVE_VEC(3) = -0.00064
                          LOVE_VEC(5) =  0.08574
                          LOVE_VEC(6) = -0.00059
                          LOVE_VEC(7) =  0.00116
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00024
                     END IF
!
                     IF ( FRQ .GT. 7.2910D-5 .AND. FRQ .LT. 7.2911D-5 ) THEN
!
! ----------------------- K1-nod (10)
!
                          LOVE_VEC(1) =  0.52829
                          LOVE_VEC(2) =  0.00248
                          LOVE_VEC(3) = -0.00071
                          LOVE_VEC(5) =  0.08689
                          LOVE_VEC(6) = -0.00061
                          LOVE_VEC(7) =  0.00110
                          LOVE_VEC(8) = -0.00031
                          LOVE_VEC(9) = -0.00028
                     END IF
!
                     IF ( FRQ .GT. 7.2921D-5 .AND. FRQ .LT. 7.2922D-5 ) THEN
!
! ----------------------- K1 (11)
!
                          LOVE_VEC(1) =  0.52359
                          LOVE_VEC(2) =  0.00310
                          LOVE_VEC(3) = -0.00072
                          LOVE_VEC(5) =  0.08704
                          LOVE_VEC(6) = -0.00060
                          LOVE_VEC(7) =  0.00109
                          LOVE_VEC(8) = -0.00031
                          LOVE_VEC(9) = -0.00028
                     END IF
!
                     IF ( FRQ .GT. 7.2931D-5 .AND. FRQ .LT. 7.2932D-5 ) THEN
!
! ----------------------- K1+nod (12)
!
                          LOVE_VEC(1) =  0.51818
                          LOVE_VEC(2) =  0.00376
                          LOVE_VEC(3) = -0.00072
                          LOVE_VEC(5) =  0.08720
                          LOVE_VEC(6) = -0.00061
                          LOVE_VEC(7) =  0.00108
                          LOVE_VEC(8) = -0.00031
                          LOVE_VEC(9) = -0.00028
                     END IF
!
                     IF ( FRQ .GT. 7.3120D-5 .AND. FRQ .LT. 7.3121D-5 ) THEN
!
! ----------------------- PSI1 (13)
!
                          LOVE_VEC(1) =  1.05684
                          LOVE_VEC(2) =  0.00344
                          LOVE_VEC(3) = -0.00013
                          LOVE_VEC(5) =  0.07103
                          LOVE_VEC(6) = -0.00204
                          LOVE_VEC(7) =  0.00188
                          LOVE_VEC(8) = -0.00033
                          LOVE_VEC(9) =  0.00009
                     END IF
!
                     IF ( FRQ .GT. 7.3319D-5 .AND. FRQ .LT. 7.3320D-5 ) THEN
!
! ----------------------- FI1 (14)
!
                          LOVE_VEC(1) =  0.66446
                          LOVE_VEC(2) = -0.00597
                          LOVE_VEC(3) = -0.00055
                          LOVE_VEC(5) =  0.08282
                          LOVE_VEC(6) = -0.00068
                          LOVE_VEC(7) =  0.00130
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00018
                     END IF
!
                     IF ( FRQ .GT. 7.5549D-5 .AND. FRQ .LT. 7.5572D-5 ) THEN
!
! ----------------------- J1 (15)
!
                          LOVE_VEC(1) =  0.61078
                          LOVE_VEC(2) = -0.00297
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08448
                          LOVE_VEC(6) = -0.00063
                          LOVE_VEC(7) =  0.00122
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
!
                     IF ( FRQ .GT. 7.8244D-5 .AND. FRQ .LT. 7.8266D-5 ) THEN
!
! ----------------------- OO1 (16)
!
                          LOVE_VEC(1) =  0.60796
                          LOVE_VEC(2) = -0.00276
                          LOVE_VEC(3) = -0.00061
                          LOVE_VEC(5) =  0.08461
                          LOVE_VEC(6) = -0.00064
                          LOVE_VEC(7) =  0.00121
                          LOVE_VEC(8) = -0.00032
                          LOVE_VEC(9) = -0.00022
                     END IF
               END IF
           ELSE IF ( M_ORD .EQ. 2  .AND. &
     &       ( TIDCNF%ORDER_2D .EQ. SOTID__2D_02ORD  .OR. &
     &         TIDCNF%ORDER_2D .EQ. SOTID__2D_12ORD  .OR. &
     &         TIDCNF%ORDER_2D .EQ. SOTID__2D_012ORD      ) ) THEN
!
! ============= Semidiurnal tides
!
                IF ( TIDCNF%MODEL_2D .EQ. SOTID__LOVE ) THEN
!
! ------------------ Love model
!
                     LOVE_VEC(1) = H2_LOVE
                     LOVE_VEC(5) = L2_LOVE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97EL ) THEN
!
! ------------------ MDG97 elastic
!
                     DO 4110 J11=1,SOTID__NLOVE
                        LOVE_VEC(J11) = LOVE_MDG97(J11,3)
 4110                CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MDG97AN ) THEN
!
! ------------------ MDG97 anelastic
!
                     DO 4120 J12=1,SOTID__NLOVE
                        LOVE_VEC(J12) = LOVE_MDG97(J12,6)
 4120                CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99EH ) THEN
!
! ------------------ DDW99 elastic hydrostatic
!
                     DO 4130 J13=1,SOTID__NLOVE
                        LOVE_VEC(J13) = LOVE_DDW99(J13,3)
 4130                CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__DDW99IN ) THEN
!
! ------------------ DDW99 inlastic non-hydrostatic
!
                     DO 4140 J14=1,SOTID__NLOVE
                        LOVE_VEC(J14) = LOVE_DDW99(J14,6)
 4140                CONTINUE
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT00 ) THEN
!
! ------------------ P. Mathews 2000 Love numbers (resonance formula)
! ------------------ for IERS Conventions 2000, revision of 10-OCT-2001
!
                     LOVE_VEC(1) = H22R_MAT00
                     LOVE_VEC(2) = H22I_MAT00
                     LOVE_VEC(3) = H2Z_MAT00
                     LOVE_VEC(4) = 0.0D0
                     LOVE_VEC(5) = L22R_MAT00
                     LOVE_VEC(6) = L22I_MAT00
                     LOVE_VEC(7) = L221_MAT00
                     LOVE_VEC(8) = L2Z_MAT00
                     LOVE_VEC(9) = 0.0D0
                  ELSE IF ( TIDCNF%MODEL_2D .EQ. SOTID__MAT01 ) THEN
!
! ------------------ P. Mathews 2001 Love numbers (resonance formula)
! ------------------ from Mathews, P.M., Love numbers and gravimetric factor 
! ------------------ for diurnal tides, Journal of the Geodetic Society of 
! ------------------ Japan, 47(1), 231--236, 2001.
!
                     LOVE_VEC(1) = H22R_MAT00
                     LOVE_VEC(2) = H22I_MAT00
                     LOVE_VEC(3) = H2Z_MAT00
                     LOVE_VEC(4) = 0.0D0
                     LOVE_VEC(5) = L22R_MAT00
                     LOVE_VEC(6) = L22I_MAT00
                     LOVE_VEC(7) = L221_MAT00
                     LOVE_VEC(8) = L2Z_MAT00
                     LOVE_VEC(9) = 0.0D0
                END IF
           END IF ! M_ORD
      END IF ! L_DEG=2
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  SOTID_GET_LOVE
