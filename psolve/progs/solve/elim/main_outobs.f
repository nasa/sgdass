      FUNCTION   MAIN_OUTOBS ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, RES, RST, &
     &                         PSF, WNPR, NAMC, AMBS )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine  MAIN_OUTOBS  returns the index in the list of    *
! *   observations  for the main outlier (if ELIM_MOD is .TRUE.) or for  *
! *   the main candidate for restoration (of ELIM_MOD is .FALSE.).       *
! *   It also returns postfit residual and normalized postfit residual.  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  ELIM_MOD ( LOGICAL*4 ) -- mode switch. If .TRUE. than main outlier  *
! *                            will be sought. If .FALSE. than the main  *
! *                            candidate in restoration will be sought.  *
! *  ELIM_TYP ( CHARACTER ) -- Mode of normalization.                    *
! *  ELIM_THR ( REAL*8    ) -- Postfit threshold (sec). ELIM mode:       *
! *                            If used observation with postfit          *
! *                            residual exceeds ELIM_THR it marked as    *
! *                            outlier. MILE mode: if recoverable        *
! *                            observation has postfit residual less     *
! *                            than ELIM_THR it marked as candidate in   *
! *                            restoration.                              *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs of  *
! *                            postfit residuals.                        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <MAIN_OUTOBS> ( INTEGER*4 ) -- The index of the main outlier or      *
! *                                candidate in restoration.             *
! *       PSF ( REAL*8    ) -- Postfit residual of the main outlier or   *
! *                            candidate in restoration (with correction *
! *                            for ambiguity even if it hass been        *
! *                            applied).                                 *
! *      WNPR ( REAL*8    ) -- Normalized residual of the main outlier   *
! *                            or main candidate in restoration.         *
! *      NAMC ( INTEGER*4 ) -- Number of ambiguities which has been      *
! *                            added to observable after solution to     *
! *                            get modified residual.                    *
! *      AMBS ( REAL*8    ) -- AMbiguity spasing which has been used to  *
! *                            get modified residual.                    *
! *                                                                      *
! *  ###  25-SEP-97   MAIN_OUTOBS   v3.0 (c)  L. Petrov  20-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      LOGICAL*4  ELIM_MOD
      CHARACTER  ELIM_TYP*2
      INTEGER*4  N_OBS, NAMC
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      INTEGER*4  MAIN_OUTOBS, IOBS
      REAL*8     ELIM_THR, WNPR, PSF, AMBS
!
      NAMC = 0
      AMBS = 0.0D0
      IF ( ELIM_TYP .EQ. 'GL' ) THEN
!
! -------- Global elimination mode
!
           IF ( ELIM_MOD ) THEN
                IOBS = RST%INDX_MXOU_G
                WNPR = RST%WNPR_MXOU_G
                IF ( IOBS .EQ. 0 ) THEN
                     IOBS = RST%INDX_MXOP_G
                     WNPR = RST%WNPR_MXOP_G
                END IF
              ELSE
                IOBS = RST%INDX_MINN_G
                WNPR = RST%WNPR_MINN_G
                NAMC = RST%NAMC_MINN_G
                AMBS = RST%AMBS_MINN_G
                IF ( IOBS .EQ. 0 ) THEN
!
! ------------------ Last resort
!
                     IOBS = RST%INDX_MINP_G
                     WNPR = RST%WNPR_MINP_G
                     NAMC = RST%NAMC_MINP_G
                     AMBS = RST%AMBS_MINP_G
                END IF
           END IF
         ELSE IF ( ELIM_TYP .EQ. 'BA' ) THEN
!
! -------- Baseline elimination mode
!
           IF ( ELIM_MOD ) THEN
                IOBS = RST%INDX_MXOU_B
                WNPR = RST%WNPR_MXOU_B
                IF ( IOBS .LE. 0 ) THEN
                     IOBS = RST%INDX_MXOU_G
                     WNPR = RST%WNPR_MXOU_G
                END IF
                IF ( IOBS .EQ. 0 ) THEN
                     IOBS = RST%INDX_MXOP_G
                     WNPR = RST%WNPR_MXOP_G
                END IF
              ELSE
                IOBS = RST%INDX_MINN_B
                WNPR = RST%WNPR_MINN_B
                NAMC = RST%NAMC_MINN_B
                AMBS = RST%AMBS_MINN_B
                IF ( IOBS .LE. 0 ) THEN
                     IOBS = RST%INDX_MINN_G
                     WNPR = RST%WNPR_MINN_G
                     NAMC = RST%NAMC_MINN_G
                     AMBS = RST%AMBS_MINN_G
                END IF
                IF ( IOBS .EQ. 0 ) THEN
!
! ------------------ Last resort
!
                     IOBS = RST%INDX_MINP_G
                     WNPR = RST%WNPR_MINP_G
                     NAMC = RST%NAMC_MINP_G
                     AMBS = RST%AMBS_MINP_G
                END IF
           END IF
      END IF
!
      PSF = 1.D20
      IF ( IOBS .GT. 0 ) PSF = RES(IOBS)%PSF_DEL + NAMC*AMBS
      MAIN_OUTOBS = IOBS
!
      RETURN
      END  !#!  MAIN_OUTOBS  #!#
