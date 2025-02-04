      FUNCTION   REPINFO ( DIAGI_S, M, INFO_CHR_G, INFO_CHR_M, INFO_CHR_B, &
     &                     GMB, FUNC_N, FUNC_B, FUNC_KEY, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Function REPINFO pasts information about the points into the       *
! *   bottom line of the current DiaGi plot.                             *
! *                                                                      *
! *   called subroutines:                                                *
! *   REPCCLO, REPBOTT, DIAGI_PURGE_BOT, PGSCI, REPHEAD                  *
! *                                                                      *
! *   calling routines:                                                  *
! *   DIAGI (via REPA and MULTI_DIAGI)                                   *
! *                                                                      *
! * ### 03-SEP-2002             REPINFO                  V. Thorandt ### *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      INTEGER*4  REPINFO
!------------------------------------------------------------------------------------------
      TYPE ( DIAGI_STRU ) ::  DIAGI_S  ! DiaGi record
      INTEGER*4  M                          ! # of parameters (G+M+B)
      CHARACTER  INFO_CHR_G(M)*87           ! information (G)
      CHARACTER  INFO_CHR_M(M)*87           ! information (M)
      CHARACTER  INFO_CHR_B(M)*87           ! information (B)
      CHARACTER  GMB(M)*1                   ! flags for good(G), man.down(M) or bad(B) obs.
      INTEGER*4  FUNC_N                     ! current # of function keys
      CHARACTER  FUNC_KEY(FUNC_N)*1         ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8           ! current button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                 ! chosen button
      INTEGER*4  IUER                       ! universal error handler
!------------------------------------------------------------------------------------------
!
      INTEGER*4  IPQ                         ! return index
      INTEGER*4  ICLR                        ! color index
      REAL*4     DIST_MM                     ! max. distance point-curser (mm)
      PARAMETER  ( DIST_MM  = 10.0 )         ! max. distance point-curser (mm)
      REAL*4     DIST_X, DIST_Y              ! distances
      CHARACTER  MESS_BOT_SAV*128            ! copy of DIAGI_S.MESS_BOT
      CHARACTER  STRING*128                  ! REPBOTT parameter (empty)
!------------------------------------------------------------------------------------------
!
      CHARACTER*1 dummy
      ICLR = 1
      IPQ = 0
      REPINFO = 0
!
! --- write button headline
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
      CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
      IF ( DIAGI_S%XC .LT. DIAGI_S%XMIN .OR. DIAGI_S%XC .GT. DIAGI_S%XMAX .OR. &
     &     DIAGI_S%YC .LT. DIAGI_S%YMIN .OR. DIAGI_S%YC .GT. DIAGI_S%YMAX ) GOTO 410
      MESS_BOT_SAV = DIAGI_S%MESS_BOT
!
! --- Set maximal distance: DIST_MM mm for each coordinate. If the point is
! --- located at more than sqrt(2)*10.0 mm, then it will be ignored.
!
      DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
      DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! --- search for the nearest point within sqrt(dist_2** + dist_y**2) area,
! --- points beyond the plotting area are ignored
!
      CALL REPCCLO ( &
     &     DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &     DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &     DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
!
!
! -------- purge the current bottom message
!
           CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --  the point was found --> print bottom message
!
      IF ( IPQ .GT. 0 ) THEN
!
         IF ( ICLR .EQ. 1 ) THEN
            DIAGI_S%MESS_BOT = INFO_CHR_G(IPQ)
            WRITE ( 6, '(A87)' ) INFO_CHR_G(IPQ)
         ELSE IF ( ICLR .EQ. 2 ) THEN
            DIAGI_S%MESS_BOT = INFO_CHR_M(IPQ)
            WRITE ( 6, '(A87)' ) INFO_CHR_M(IPQ)
         ELSE
            DIAGI_S%MESS_BOT = INFO_CHR_B(IPQ)
            WRITE ( 6, '(A87)' ) INFO_CHR_B(IPQ)
         END IF
!
! --- the point was not found
!
      ELSE
         DIAGI_S%MESS_BOT = 'Please move the cursor closer to observation to get information about it.'
      END IF
!
! --- print new bottom message
!
      CALL CLRCH ( STRING )
      CALL REPBOTT ( DIAGI_S, STRING, 7 )
!
      DIAGI_S%MESS_BOT = MESS_BOT_SAV
!
410   CONTINUE
      REPINFO = 1   ! SUB_GETINFO user function returns 1 in case of no error
!
      END  !#!  REPINFO  #!#
