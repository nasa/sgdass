      SUBROUTINE REPA_SHOW_MS ( DIAGI_S, REP, IND_BAS, NCLR, RING_CLR,  &
     &                          N1, XARR1, YARR1, EARR1, &
     &                          N2, XARR2, YARR2, EARR2, &
     &                          N3, XARR3, YARR3, EARR3  )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_SHOW_MS puts a special mark: a cycle of the certain   *
! *   color around the point which correspnds to the specific source.    *
! *                                                                      *
! * ________________________ Input parameters: ______________________    *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *  IND_BAS ( INTEGER*4 ) -- baseline index.                            *
! *     NCLR ( INTEGER*4 ) -- The total number of cagegories (colors).   *
! *       N1 ( INTEGER*4 ) -- The number of good points.                 *
! *    XARR1 ( REAL*4    ) -- Array of good point arguments.             *
! *                           Dimension: N1.                             *
! *    EARR1 ( REAL*4    ) -- Array of good point errors.                *
! *                           Dimension: N1.                             *
! *       N2 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR2 ( REAL*4    ) -- Array of bad point arguments.              *
! *                           Dimension: N2.                             *
! *    EARR2 ( REAL*4    ) -- Array of bad point errors.                 *
! *                           Dimension: N2.                             *
! *       N3 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR3 ( REAL*4    ) -- Array of unrecoverable point arguments.    *
! *                           Dimension: N3.                             *
! *    EARR3 ( REAL*4    ) -- Array of unrecoverable point errors.       *
! *                           Dimension: N3.                             *
! *                                                                      *
! * ### 07-DEC-2004   REPA_SHOW_MS   v1.0 (c) L. Petrov  08-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_BAS, NCLR, RING_CLR, N1, N2, N3
      REAL*4     XARR1(*), YARR1(*), EARR1(*), &
     &           XARR2(*), YARR2(*), EARR2(*), &
     &           XARR3(*), YARR3(*), EARR3(*) 
      REAL*4     XMIN, XMAX, YMIN, YMAX, XRAD_WC, YRAD_WC, RAD_MM, &
     &           DIST, DIST_MIN
      INTEGER*4  MARR
      PARAMETER  ( MARR = 4096 )
      INTEGER*4  N_RING
      REAL*4     RAD_MIN, RAD_STEP
      PARAMETER  ( RAD_MIN  = 1.25 ) ! How many times inner circle of the &
!                                    ! the ring should exceed the radius of
!                                    ! the point disk
      PARAMETER  ( RAD_STEP = 0.03 ) ! Step of increase the ring
      PARAMETER  ( N_RING   = 12 ) ! The number of steps for ring increase
      INTEGER*4  IND_PT(MARR,2), LARR
!
      CHARACTER  STR*20
      INTEGER*4  J1, J2, J3, J4, J5, IL, IMARK, MARK_CLR, NPTS, &
     &           IND_OBS, IND_SOU, IND_CLR_OLD
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      XMIN = DIAGI_S%XMIN
      XMAX = DIAGI_S%XMAX
      YMIN = DIAGI_S%YMIN
      YMAX = DIAGI_S%YMAX
      CALL PGCOL_RGB ( MARK_CLR, IRGB_DEF(2,1,1), IRGB_DEF(2,1,2), &
     &                                            IRGB_DEF(2,1,3)  )
!
! --- Determine the radii of the circle
!
      IF ( DIAGI_S%IPST(1) .EQ. 2  .OR.  DIAGI_S%IPST(1) .EQ. 4 ) THEN
           RAD_MM = DIAGI_S%RAD_SMALL*RAD_MIN
           NPTS   = NPTS_SMALL
         ELSE IF ( DIAGI_S%IPST(1) .EQ. 3  .OR.  DIAGI_S%IPST(1) .EQ. 5 ) THEN
           RAD_MM = DIAGI_S%RAD_LARGE*RAD_MIN
           NPTS   = NPTS_LARGE
      END IF
!
      IF ( DIAGI_S%IPST(1) .GE. 2  .AND.  DIAGI_S%IPST(1) .LE. 5 ) THEN
           XRAD_WC = RAD_MM*(XMAX-XMIN)/(DIAGI_S%XRIGHT - DIAGI_S%XLEFT )
           YRAD_WC = RAD_MM*(YMAX-YMIN)/(DIAGI_S%YTOP   - DIAGI_S%YBOT  )
      END IF
!
! --- Search the point which is the closest to the cursor
!
      LARR = 0
      IF ( REP%PLT(IND_BAS)%N_GOO > 0 ) THEN
           DO 410 J1=1,REP%PLT(IND_BAS)%N_GOO
              IND_OBS = REP%PLT(IND_BAS)%IND_GOO(J1)
              IND_SOU = REP%OBS(IND_OBS)%IND_SOU
              IF ( REP%LSEL_SOU(IND_SOU) ) THEN
                   LARR = LARR + 1
                   IND_PT(LARR,1) = J1
                   IND_PT(LARR,2) = 1
              END IF
 410       CONTINUE 
      END IF
!    
      IF ( REP%CNF%SHOW_CBAD  .AND.  REP%PLT(IND_BAS)%N_BAD > 0 ) THEN
           DO 420 J2=1,REP%PLT(IND_BAS)%N_BAD
              IND_OBS = REP%PLT(IND_BAS)%IND_BAD(J2)
              IND_SOU = REP%OBS(IND_OBS)%IND_SOU
              IF ( REP%LSEL_SOU(IND_SOU) ) THEN
                   LARR = LARR + 1
                   IND_PT(LARR,1) = J2
                   IND_PT(LARR,2) = 2
              END IF
 420       CONTINUE 
      END IF
!    
      IF ( REP%CNF%SHOW_UNRC  .AND.  REP%PLT(IND_BAS)%N_UNR > 0 ) THEN
           DO 430 J3=1,REP%PLT(IND_BAS)%N_UNR
              IND_OBS = REP%PLT(IND_BAS)%IND_UNR(J3)
              IND_SOU = REP%OBS(IND_OBS)%IND_SOU
              IF ( REP%LSEL_SOU(IND_SOU) ) THEN
                   LARR = LARR + 1
                   IND_PT(LARR,1) = J3
                   IND_PT(LARR,2) = 3
              END IF
 430       CONTINUE 
      END IF
!
! --- No points selected? nothing to do!
!
      IF ( LARR == 0 ) RETURN 
!
      CALL PGQCI ( IND_CLR_OLD )
      CALL PGSCI ( ITAB_CLR(RING_CLR,1) )
      CALL PGSFS ( 2 )
      DO 440 J4=1,LARR
         DO 450 J5=1,N_RING
            IF ( IND_PT(J4,2) == 1 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR1(IND_PT(J4,1)), &
     &                                   YARR1(IND_PT(J4,1)), &
     &                                   XRAD_WC*(1.0+0.03*(J5-1)), &
     &                                   YRAD_WC*(1.0+0.03*(J5-1)) )
              ELSE IF ( IND_PT(J4,2) == 2 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR2(IND_PT(J4,1)), &
     &                                   YARR2(IND_PT(J4,1)), &
     &                                   XRAD_WC*(1.0+0.03*(J5-1)), &
     &                                   YRAD_WC*(1.0+0.03*(J5-1)) )
              ELSE IF ( IND_PT(J4,2) == 3 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR3(IND_PT(J4,1)), &
     &                                   YARR3(IND_PT(J4,1)), &
     &                                   XRAD_WC*(1.0+0.03*(J5-1)), &
     &                                   YRAD_WC*(1.0+0.03*(J5-1)) )
            END IF
 450     CONTINUE 
 440  CONTINUE 
      CALL PGSCI ( IND_CLR_OLD )
!
      RETURN
      END  SUBROUTINE  REPA_SHOW_MS  !#!#
