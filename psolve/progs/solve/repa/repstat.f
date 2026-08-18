      SUBROUTINE REPSTAT ( DIAGI_S, YG, EG )
!
! ************************************************************************
! *                                                                      *
! *   REPSTAT recalculates the statistics WM and RWR and redraws the     *
! *   plot headline.                                                     *
! *                                                                      *
! *   called subroutines: REPSIGM, REPMVVA, CLRCH, CHASHL,               *
! *                       DIAGI_SET_FRAME DIAGI_DRAW                     *
! *                                                                      *
! *   calling routines:   REPPTSH, REPGRSH, REPPTSU, REPGRSU, REPGRRS    *
! *                                                                      *
! *  ### 16-JAN-2003  REPSTAT              V. Thorandt  16-JAN-2003 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
!
      TYPE ( DIAGI_STRU ) ::  DIAGI_S   ! DIAGI_STRU record
      REAL*8     YG(*)                       ! values (G)
      REAL*8     EG(*)                       ! errors (G)
      INTEGER*4  ILEN                        ! length of a character string (function)
      CHARACTER  ZAG_SAV*128                 ! copy of DIAGI_S.ZAG
      REAL*8     MEAN                        ! weighted mean residual
      REAL*8     VAR                         ! rms weighted residual
      REAL*4     VAR3                        ! 3*VAR
      CHARACTER  MEAN_CH*11                  ! WTD. Mean of baseline (character string)
      CHARACTER  VAR_CH*11                   ! RMS W.Res (character string)
      INTEGER*4  BL2                         ! temporary value
      CHARACTER  STAT_TMP*128                ! temporary character string
      INTEGER*4  J1, J2                      ! loop variables
!
! --- calculate new values of MEAN and VAR
!
      IF ( DIAGI_S%NPOI(1) .GT. 0 ) THEN
         CALL REPSIGM ( MEAN, VAR, YG, EG, DIAGI_S%NPOI(1) )
      ELSE
         MEAN = 0.0D0
         VAR  = 0.0D0
      END IF
!
! --- new sigmaline values
!
      VAR3 = 3*VAR           ! (real*8 --> real*4)
      CALL REPMVVA ( 1, VAR3, %VAL(DIAGI_S%ADR_Y4(5)) )  ! 1st sigmaline
      CALL REPMVVA ( 2, VAR3, %VAL(DIAGI_S%ADR_Y4(5)) )
      VAR3 = 0.0 - VAR3
      CALL REPMVVA ( 1, VAR3, %VAL(DIAGI_S%ADR_Y4(6)) )  ! 2nd sigmaline
      CALL REPMVVA ( 2, VAR3, %VAL(DIAGI_S%ADR_Y4(6)) )
!
      ZAG_SAV = DIAGI_S%ZAG                              ! copy title
      CALL CLRCH ( MEAN_CH )
      CALL CLRCH ( VAR_CH )
      WRITE ( MEAN_CH, '(F9.1)' ) MEAN
      WRITE ( VAR_CH, '(F9.1)' ) VAR
      CALL CHASHL ( MEAN_CH )
      CALL CHASHL ( VAR_CH )
!
!     create new DIAGI_S.ZAG
!
      CALL CLRCH ( DIAGI_S%ZAG )
      BL2 = 0
      CALL CLRCH ( STAT_TMP )
      DO J1=1,128                                  ! search for station names
         IF ( ZAG_SAV(J1:J1) .EQ. '=' ) THEN
            BL2 = BL2 + 1
            IF ( BL2 .EQ. 2 ) THEN
               DO J2=J1+1,128
                  IF ( ZAG_SAV(J2:J2) .EQ. ' ' ) THEN
                     STAT_TMP = ZAG_SAV(J2+1:J2+16)
!C                   write(6,*) 'REPSTAT: ZAG_SAV(J2+1:J2+16)=',ZAG_SAV(J2+1:J2+16)
                     CALL CHASHL ( STAT_TMP )
!C                   write(6,*) 'REPSTAT: STAT_TMP=',ILEN(STAT_TMP)
                     GOTO 420
                  END IF
               END DO
            END IF
         END IF
      END DO
  420 CONTINUE
      DO J1=1,128                    ! compose new DIAGI_S.ZAG
         IF ( ZAG_SAV(J1:J1) .EQ. '=' ) THEN
!C            write(6,*) 'REPSTAT: length=',J1+ILEN(MEAN_CH)+5+ILEN(VAR_CH)+ILEN(STAT_TMP)
              DIAGI_S%ZAG(1:J1+ILEN(MEAN_CH)+5+ILEN(VAR_CH)+1+ILEN(STAT_TMP)) = &
     &               ZAG_SAV(1:J1)// &
     &               MEAN_CH(1:ILEN(MEAN_CH))// &
     &               ' RWR='//VAR_CH(1:ILEN(VAR_CH))//' '// &
     &               STAT_TMP(1:ILEN(STAT_TMP))
            GOTO 430
         END IF
      END DO
  430 CONTINUE
!
! --- redraw the plot
!
      CALL DIAGI_SET_FRAME ( DIAGI_S, DIAGI_S%MESS_BOT  )
      DO J1=1,DIAGI_S%NCLR
         CALL DIAGI_DRAW ( DIAGI_S, J1, 0, &
     &                DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_X4(J1)), &
     &                %VAL(DIAGI_S%ADR_Y4(J1)), %VAL(DIAGI_S%ADR_E4(J1)), &
     &                0.D0, 0.D0 )
      END DO
!
! --- copy new title into initial plot title (for plot reset)
!
      DIAGI_S%ZAG_SAV = DIAGI_S%ZAG
!
      RETURN
      END
!
