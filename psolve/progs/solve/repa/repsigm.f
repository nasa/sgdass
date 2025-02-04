      SUBROUTINE REPSIGM ( MEAN, VAR, ROC_FG, ERR_FG, NUM_G )
!
!23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
!
!
! **********************************************************************
! *                                                                    *
! *  REPSIGM IS A SUBROUTINE WHICH COMPUTES VALUES FOR                 *
! *  THE STATISTICS WEIGHTED MEAN RESIDUAL, RMS WEIGHTED RESIDUAL      *
! *                                                                    *
! **********************************************************************
!
!    Input:  ROC_FG, ERR_FG, NUM_G
!
!    Output: MEAN, VAR
!
!
!    called subroutines: none
!
!    calling routine: REPA
!
!    Written:
!
!       Gerald Engelhardt  23.09.2002
!
      REAL*8     ROC_FG(*)        ! residuals
      REAL*8     ERR_FG(*)        ! sigma
      REAL*8     MEAN             ! weighted mean residual
      REAL*8     VAR              ! rms weighted residual
      REAL*8     SUM_WTG_RES      ! sum of weighted residuals
      REAL*8     SUM_WEIGHTS      ! sum of weights
      REAL*8     SUM_WRES_SQ      ! sum of squares of weighted residuals
      REAL*8     SUM_VARS         ! sum of squares of weights
!
      INTEGER*4  NUM_G            ! # of "good" observations in current baseline (G, green)
      INTEGER*4  I
!
! --- sums of weighted residuals, weights
!
      SUM_WTG_RES = 0.D0          ! sum of weighted residuals
      SUM_WEIGHTS = 0.D0          ! sum of weights
!
      DO I = 1, NUM_G
         SUM_WTG_RES = SUM_WTG_RES + ( ROC_FG(I) / ERR_FG(I) )
         SUM_WEIGHTS = SUM_WEIGHTS + ( 1.D0      / ERR_FG(I) )
      END DO
!
! --- weighted mean residual
!
      MEAN = SUM_WTG_RES / SUM_WEIGHTS
!
! --- sums of squares of weighted residuals
! --- sums of squares of weights (sum of reciprocals of variances)
!
      SUM_WRES_SQ = 0.D0          ! sum of squares of weighted residuals
      SUM_VARS    = 0.D0          ! sum of squares of weights
!
      DO I = 1, NUM_G
         SUM_WRES_SQ = SUM_WRES_SQ + ( (ROC_FG(I) / ERR_FG(I) )**2 )
         SUM_VARS    = SUM_VARS    + ( 1.D0      / ERR_FG(I)**2 )
      END DO
!
! --- rms weighted residual
!
      VAR = DSQRT ( SUM_WRES_SQ / SUM_VARS )
!
      RETURN
      END
