      SUBROUTINE REPA_DOINQ ( DIAGI_S, REP, XC, YC,  IND_BAS, NCLR,  &
     &                        N1, XARR1, YARR1, EARR1, &
     &                        N2, XARR2, YARR2, EARR2, &
     &                        N3, XARR3, YARR3, EARR3, &
     &                        IND_PT, IND_CLR )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_DOINQ inquires the information about the current      *
! *   point. It updates the bottom label and put there information       *
! *   about the argument and the value of the point which is the closest *
! *   to the cursor. It also indicates the point by printing a small     *
! *   blue asterisk on the point.                                        *
! *                                                                      *
! * ________________________ Input parameters: ______________________    *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
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
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  IND_CLR ( INTEGER*4 ) -- Caterory index. One of                     *
! *                           REPA__I_GOO -- Good point.                 *
! *                           REPA__I_BAD -- Bad, but recoverable point. *
! *                           REPA__I_UNR -- Bad, unrecoverable point.   *
! *   IND_PT ( INTEGER*4 ) -- point index **within** the categroy.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       XC ( REAL*4    ) -- X-coordinate of the current cursor         *
! *                           position.                                  *
! *       YC ( REAL*4    ) -- Y-coordinate of the current cursor         *
! *                           position.                                  *
! *  IND_BAS ( INTEGER*4 ) -- baseline index.                            *
! *     NCLR ( INTEGER*4 ) -- The total number of cagegories (colors).   *
! *                                                                      *
! * ### 07-DEC-2004    REPA_DOINQ   v1.0 (c) L. Petrov  08-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_BAS, NCLR, N1, N2, N3, IND_PT, IND_CLR
      REAL*4     XC, YC
      REAL*4     XARR1(*), YARR1(*), EARR1(*), &
     &           XARR2(*), YARR2(*), EARR2(*), &
     &           XARR3(*), YARR3(*), EARR3(*) 
      REAL*4     XMIN, XMAX, YMIN, YMAX, XRAD_WC, YRAD_WC, RAD_MM, &
     &           DIST, DIST_MIN
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, IL, NPTS, IMARK, MARK_CLR
      PARAMETER  ( IMARK    = 12 )
      PARAMETER  ( MARK_CLR = 43 )
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
           RAD_MM = DIAGI_S%RAD_SMALL
           NPTS   = NPTS_SMALL
         ELSE IF ( DIAGI_S%IPST(1) .EQ. 3  .OR.  DIAGI_S%IPST(1) .EQ. 5 ) THEN
           RAD_MM = DIAGI_S%RAD_LARGE
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
      CALL REPA_SEARCH_CLOSEST ( XC, YC, NCLR,            &      
                                 N1, XARR1, YARR1,        &
     &                           N2, XARR2, YARR2,        &
     &                           N3, XARR3, YARR3,        &
     &                           XMIN, XMAX, YMIN, YMAX,  &
     &                           IND_CLR, IND_PT )
      IF ( IND_PT .EQ. 0 ) RETURN 
!
      CALL PGBBUF  ! starting bufferization
      IF ( DIAGI_S%IPQ .EQ. 0  .OR.  DIAGI_S%ICQ .EQ. 0 ) THEN
!
! -------- There were no previous requests. Then we should extinguish DiaGi
! -------- label at the bottom line
!
           CALL DIAGI_PURGE_BOT ( DIAGI_S )
         ELSE
!
! -------- There were previous requests
!
           CALL PGSAVE
!
! -------- Extinguishing the marker at the point previously requested
!
           CALL PGSCI  ( 0 )
           IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                CALL PGPT ( 1, XARR1(DIAGI_S%IPQ), YARR1(DIAGI_S%IPQ), IMARK )
              ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                CALL PGPT ( 1, XARR2(DIAGI_S%IPQ), YARR2(DIAGI_S%IPQ), IMARK )
              ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                CALL PGPT ( 1, XARR3(DIAGI_S%IPQ), YARR3(DIAGI_S%IPQ), IMARK )
           END IF
!
! -------- Lighting again the point previously requested since marker spoiled
! -------- point representation
!
           CALL PGSCI  ( ITAB_CLR(DIAGI_S%ICQ,1) )
           IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 1 ) THEN
                IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                     CALL PGPNTS ( 1, XARR1(DIAGI_S%IPQ), YARR1(DIAGI_S%IPQ), &
     &                             1, 1, 1 )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                     CALL PGPNTS ( 1, XARR2(DIAGI_S%IPQ), YARR2(DIAGI_S%IPQ), &
     &                             1, 1, 1 )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                     CALL PGPNTS ( 1, XARR3(DIAGI_S%IPQ), YARR3(DIAGI_S%IPQ), &
     &                             1, 1, 1 )
                END IF
             ELSE IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 2  .OR.  &
     &                 DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 3        ) THEN
!
! ------------- Outlined circles
!
                CALL PGSLW ( 1 )
                CALL PGSFS ( 1 )
                CALL PGSCI ( 0 )
                IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                 YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                 YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                 YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                END IF
                CALL PGSFS ( 2 )
                CALL PGSCI ( ITAB_CLR(DIAGI_S%ICQ,1) )
                IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                 YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                 YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                 YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                END IF
             ELSE IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 4  .OR.  &
     &                 DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 5        ) THEN
                CALL PGSLW ( 1 )
                CALL PGSFS ( 1 )
                CALL PGSCI ( ITAB_CLR(DIAGI_S%ICQ,1) )
                IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                 YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                 YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                 YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                END IF
           END IF
!
! -------- Now we shoud extinguish information message about the previously
! -------- requested point
!
           CALL DIAGI_PURGE_BOT ( DIAGI_S )
           CALL PGUNSA
      END IF
!
! --- The point has been found
!
      CALL PGSAVE
      CALL PGSCI ( 0 )
!
! --- Extinguishing this point at the plot
!
      IF ( DIAGI_S%IPST(IND_CLR) .EQ. 1 ) THEN
           IF ( IND_CLR .EQ. 1 ) THEN
                CALL PGPNTS ( 1, XARR1(IND_PT), YARR1(IND_PT), 1, 1, 1 )
              ELSE IF ( IND_CLR .EQ. 2 ) THEN
                CALL PGPNTS ( 1, XARR2(IND_PT), YARR2(IND_PT), 1, 1, 1 )
              ELSE IF ( IND_CLR .EQ. 3 ) THEN
                CALL PGPNTS ( 1, XARR3(IND_PT), YARR3(IND_PT), 1, 1, 1 )
           END IF
         ELSE IF ( DIAGI_S%IPST(IND_CLR) .EQ. 2  .OR.  &
     &             DIAGI_S%IPST(IND_CLR) .EQ. 3        ) THEN
!
! -------- Outlined circles
!
           CALL PGSFS ( 2  )
           IF ( IND_CLR .EQ. 1 ) THEN
                CALL PGCIRC_PET ( NPTS, XARR1(IND_PT), YARR1(IND_PT), &
     &                            XRAD_WC, YRAD_WC )
              ELSE IF ( IND_CLR .EQ. 2 ) THEN
                CALL PGCIRC_PET ( NPTS, XARR2(IND_PT), YARR2(IND_PT), &
     &                            XRAD_WC, YRAD_WC )
              ELSE IF ( IND_CLR .EQ. 3 ) THEN
                CALL PGCIRC_PET ( NPTS, XARR3(IND_PT), YARR3(IND_PT), &
     &                            XRAD_WC, YRAD_WC )
           END IF  
         ELSE IF ( DIAGI_S%IPST(IND_CLR) .EQ. 4  .OR.  &
     &             DIAGI_S%IPST(IND_CLR) .EQ. 5        ) THEN
!
! --------- Filled circles
!
            CALL PGSFS ( 1  )
            IF ( IND_CLR .EQ. 1 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR1(IND_PT), YARR1(IND_PT), &
     &                             XRAD_WC, YRAD_WC )
              ELSE IF ( IND_CLR .EQ. 2 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR2(IND_PT), YARR2(IND_PT), &
     &                             XRAD_WC, YRAD_WC )
              ELSE IF ( IND_CLR .EQ. 3 ) THEN
                 CALL PGCIRC_PET ( NPTS, XARR3(IND_PT), YARR3(IND_PT), &
     &                             XRAD_WC, YRAD_WC )
            END IF  
      END IF
!
! --- Putting a marker with warning light
!
      CALL PGSCI ( MARK_CLR )
      IF ( IND_CLR .EQ. 1 ) THEN
           CALL PGPT  ( 1, XARR1(IND_PT), YARR1(IND_PT), IMARK )
         ELSE IF ( IND_CLR .EQ. 2 ) THEN
           CALL PGPT  ( 1, XARR2(IND_PT), YARR2(IND_PT), IMARK )
         ELSE IF ( IND_CLR .EQ. 3 ) THEN
           CALL PGPT  ( 1, XARR3(IND_PT), YARR3(IND_PT), IMARK )
      END IF
      CALL PGUNSA
!
! --- Formatting description line
!
      CALL REPA_INQLINE ( REP, IND_BAS, IND_CLR, IND_PT, DIAGI_S%MESS_BOT )
!
! --- .. and printing information message on the screen
!
      CALL PGSAVE
      CALL PGSCF  ( 2 )
      CALL PGSCH  ( DIAGI_S%SCH_LAB  )
      CALL PGSLW  ( DIAGI_S%ISLW_LAB )
      IL = I_LEN(DIAGI_S%MESS_BOT)
      CALL CLRCH ( STR )
      CALL PGPTXT ( XMIN, YMIN + (YMAX-YMIN)*DIAGI_S%YSH_LAB, &
     &              0.0, 0.0, STR(1:IL) )
      CALL PGPTXT ( XMIN, YMIN + (YMAX-YMIN)*DIAGI_S%YSH_LAB, &
     &              0.0, 0.0, DIAGI_S%MESS_BOT(1:IL) )
      CALL PGUNSA
!
! --- ... and into the information file
!
      WRITE ( UNIT=REP%CNF%LUN_REPI, FMT='(A)' ) &
     &                               DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT))
      CALL FLUSH ( REP%CNF%LUN_REPI )
!
      RETURN
      END  SUBROUTINE REPA_DOINQ
