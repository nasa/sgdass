      SUBROUTINE REPBOTT ( DIAGI_S, STRING, COLOUR )
!
! ************************************************************************
! *                                                                      *
! *   REPBOTT displays the bottom info message including a headline.     *
! *                                                                      *
! *   called subroutines:                                                *
! *   CLRCH, PGSCI, PGSCF, PGPTXT                                        *
! *                                                                      *
! *   calling routines:                                                  *
! *   REPINFO, REPPTSU, REPCONN, REPHEAD                                 *
! *                                                                      *
! * ### 10-DEC-2002             REPBOTT  v1.0            V. Thorandt ### *
! * 02-12-11 VT STRING variable added                                    *
! * 03-01-17 VT display units                                            *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S  ! DiaGi record
!
      INTEGER*4  COLOUR                     ! colour of single bottom line
      CHARACTER  BOTT*128                   ! bottomline
      CHARACTER  STRING*128                 ! string to be displayed
      REAL*4     CH_SIZE                    ! save character size
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128
      REAL*4     YS(2)                      ! shift factor dependig on screen size (line 1 & 2)
      DATA YS / -0.060, -0.085 /            ! defaults for small screen
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- scaling depending on DIAGI_SCREEN environment
!
      CALL PGQCH ( CH_SIZE )                ! inquire old PGPLOT font size
      CALL PGSCH ( 1.0 )
!
! --- Learh the screen size
!
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                       UNIT, ICL1, ICL2, ICL3, IER )
      IF ( IDEV .EQ. 1 ) THEN   ! big screen
         CALL PGSCH ( 0.8 )
         YS(1) = -0.050
         YS(2) = -0.075
      END IF
!
! --- erase DiaGi bottomline
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )      ! purge bottom display
!
! --- compose new bottomline(s)
!
      CALL CLRCH ( BOTT )
!
      IF ( STRING(1:1) .NE. ' ' ) THEN      ! only one bottomline will be displayed
         BOTT = STRING                      ! copy of BOTT
         DIAGI_S%MESS_BOT = ' '             ! clear DiaGi bottom message
         CALL PGSCI ( COLOUR )              ! set PGPLOT colour (set in calling routine)
         IF ( IDEV .EQ. 1 ) THEN   ! big screen
              CALL PGSCH ( 1.0 )
         END IF
      ELSE                                  ! two bottomlines will be displayed
!C                     0         1         2         3         4         5         6         7         8
!C                     123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
         BOTT(1:85) = 'REC# IRUN  SOURCE        TIME     SNR  QC  ICORR     VALUE FSIG CSIG AZ1 AZ2 E1 E2 ST'
         CALL PGSCI ( 7 )      ! set PGPLOT colour (dark red)
      ENDIF
!
      CALL PGSCF ( 2 )         ! set PGPLOT character font
!
! --- write first bottom line
!
      CALL PGPTXT ( DIAGI_S%XMIN, &
     &              DIAGI_S%YMIN + (DIAGI_S%YMAX-DIAGI_S%YMIN)*YS(1), &
     &              0.0, 0.0, BOTT(1:ILEN(BOTT)) )
!
      CALL PGSCI ( 1 )         ! set PGPLOT colour
!
! --- WRITE the second bottom line
!
      CALL PGPTXT ( DIAGI_S%XMIN, &
     &              DIAGI_S%YMIN + (DIAGI_S%YMAX-DIAGI_S%YMIN)*YS(2), &
     &              0.0, 0.0, DIAGI_S%MESS_BOT(1:ILEN(DIAGI_S%MESS_BOT)) )
!
! ---- rewrite unit because DiaGi unit display has been erased
!
      CALL PGSCI ( 1 )                               ! set PGPLOT colour (black)
      CALL PGSCH ( 0.65 )                            ! set PGPLOT font size
      CALL PGPTXT ( DIAGI_S%XMAX, &
     &              DIAGI_S%YMIN + 0.55*(DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_LAB, &
     &              0.0, 0.0, DIAGI_S%ARG_UNITS(1:ILEN(DIAGI_S%ARG_UNITS)) )
       CALL PGSCH ( CH_SIZE )                         ! reset old PGPLOT font size
!
      RETURN
!
      END  !#!  REPBOTT  #!#
