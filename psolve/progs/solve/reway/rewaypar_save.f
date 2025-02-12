      SUBROUTINE REWAYPAR_SAVE ( IWT_MODE, PAUSE_REWAY, KFALL_BACK, &
     &                           KNEW_UPDATE, MAX_ITER, CHI_TOL, WT_FLOOR, &
     &                           WT_CEILING )
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*2  IWT_MODE, MAX_ITER
      LOGICAL*2  PAUSE_REWAY, KFALL_BACK, KNEW_UPDATE
      REAL*8     CHI_TOL, WT_FLOOR(2), WT_CEILING(2)
!
      IF ( IWT_MODE .EQ. 0 ) THEN
           REWAY_TYPE = 'GL'
        ELSE IF ( IWT_MODE .EQ. 1 ) THEN
           REWAY_TYPE = 'SI'
        ELSE IF ( IWT_MODE .EQ. 2 ) THEN
           REWAY_TYPE = 'BA'
      END IF
!
      REWAY_VERBOSE  = PAUSE_REWAY
      REWAY_FALLBACK = KFALL_BACK
      REWAY_NEWUPD   = KNEW_UPDATE
      REWAY_MAXIT    = MAX_ITER
      REWAY_CHITOL   = CHI_TOL
      REWAY_FLODEL   = WT_FLOOR(1)
      REWAY_FLORATE  = WT_FLOOR(2)
      REWAY_CEIDEL   = WT_CEILING(1)
      REWAY_CEIRATE  = WT_CEILING(2)
!
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      RETURN
      END  !#!  REWAYPAR_SAVE  #!#
