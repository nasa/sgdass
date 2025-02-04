      FUNCTION   DIAGI_INQ ( DIAGI_S, N, XARR4, YARR4, EARR4, XC, YC, &
     &                       IPQ )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAGI_INQ  is generating report about the point of the  *
! *   current colour to be the nearest point at the DiaGI current        *
! *   plotting area to the current cursor position. The marker is        *
! *   setting to that point and some information is printed at the       *
! *   bottom line: [CLR=cc, ] ii(tt)[vv] where cc -- index of the        *
! *   current colour, ii -- the index of the current point, tt -- total  *
! *   number of points, vv -- total number of visible points at the      *
! *   current plotting are.                                              *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *           N ( INTEGER*4 ) -- the number of points.                   *
! *       XARR4 ( INTEGER*4 ) -- Array (dimension N) of the arguments.   *
! *       YARR4 ( INTEGER*4 ) -- Array (dimension N) of the values.      *
! *       EARR4 ( INTEGER*4 ) -- Array (dimension N) of the errors of    *
! *                              the function under investigation.       *
! *          XC ( REAL*4    ) -- Current cursor positions ( x-world      *
! *                              coordinate ).                           *
! *          YC ( REAL*4    ) -- Current cursor positions ( y-world      *
! *                              coordinate ).                           *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <DIAGI_INQ> ( INTEGER*4 ) -- The same as IPQ.                        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI        *
! *                              internal parameters.                    *
! *         IPQ ( INTEGER*4 ) -- Index of the point under investigation. *
! *                              Input value: the index of the previous  *
! *                              requiested point ( or 0 of no).         *
! *                              Output value: the index of the current  *
! *                              point under investigation. 0 if there   *
! *                              is no points in the plotting area.      *
! *                                                                      *
! *  ###  15-OCT-97    DIAGI_INQ   v1.4  (c)  L. Petrov 21-APR-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  DIAGI_INQ, IPQ, N
      CHARACTER  STR*20, MESQ*128
      REAL*4     XC, YC
      REAL*4     XARR4(N), YARR4(N), EARR4(N)
      REAL*4     XMIN4, XMAX4, YMIN4, YMAX4, DIST, DIST_MIN, RAD_MM, &
     &           XRAD_WC, YRAD_WC
      INTEGER*4  IUSE, J1, NPTS, ICLR, ILST, IPST, IL, IMARK
      LOGICAL*4  LER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      PARAMETER  ( IMARK = 12 )
!
      XMIN4 = DIAGI_S%XMIN
      XMAX4 = DIAGI_S%XMAX
      YMIN4 = DIAGI_S%YMIN
      YMAX4 = DIAGI_S%YMAX
!
      ICLR  = DIAGI_S%ICLR
      ILST  = DIAGI_S%ILST(ICLR)
      IPST  = DIAGI_S%IPST(ICLR)
      LER   = DIAGI_S%LER(ICLR)
!
! --- Determine the radii of the circle
!
      IF ( IPST .EQ. 2  .OR. IPST .EQ. 4 ) THEN
           RAD_MM = DIAGI_S%RAD_SMALL
           NPTS   = NPTS_SMALL
         ELSE IF ( IPST .EQ. 3 .OR. IPST .EQ. 5 ) THEN
           RAD_MM = DIAGI_S%RAD_LARGE
           NPTS   = NPTS_LARGE
      END IF
      IF ( DIAGI_S%XRIGHT > 600.0 ) RAD_MM = 1.6*RAD_MM
!
      IF ( IPST .GE. 2  .AND.  IPST .LE. 5 ) THEN
           XRAD_WC = RAD_MM*(XMAX4-XMIN4)/(DIAGI_S%XRIGHT - DIAGI_S%XLEFT )
           YRAD_WC = RAD_MM*(YMAX4-YMIN4)/(DIAGI_S%YTOP - DIAGI_S%YBOT )
      END IF
!
      CALL PGBBUF  ! starting bufferization
      IF ( IPQ .EQ. 0 ) THEN
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
           CALL PGPT   ( 1, XARR4(IPQ), YARR4(IPQ), IMARK )
!
! -------- Lighting again the point previously requested since marker spoiled
! -------- point representation
!
           CALL PGSCI  ( ITAB_CLR(ICLR,1) )
           IF ( IPST .EQ. 1 ) THEN
                CALL PGPNTS ( 1, XARR4(IPQ), YARR4(IPQ), 1, 1, 1 )
             ELSE IF ( IPST .EQ. 2  .OR.  IPST .EQ. 3  ) THEN
!
! -------------- Outlined circles
!
                 CALL PGSLW ( 1 )
                 CALL PGSFS ( 1 )
                 CALL PGSCI ( 0 )
                 CALL PGCIRC_PET ( NPTS, XARR4(IPQ), YARR4(IPQ), &
     &                             XRAD_WC, YRAD_WC )
                 CALL PGSFS ( 2                )
                 CALL PGSCI ( ITAB_CLR(ICLR,1) )
                 CALL PGCIRC_PET ( NPTS, XARR4(IPQ), YARR4(IPQ), &
     &                             XRAD_WC, YRAD_WC )
               ELSE IF ( IPST .EQ. 4  .OR.  IPST .EQ. 5 ) THEN
                 CALL PGSLW ( 1                )
                 CALL PGSFS ( 1                )
                 CALL PGSCI ( ITAB_CLR(ICLR,1) )
                 CALL PGCIRC_PET ( NPTS, XARR4(IPQ), YARR4(IPQ), &
     &                             XRAD_WC, YRAD_WC )
           END IF
!
! -------- Now we shoud extinguish information message about the previously
! -------- requested point
!
           CALL DIAGI_PURGE_BOT ( DIAGI_S )
           CALL PGUNSA
      END IF
!
! --- Scanning all points and searching the point to be at the minimal distance
! --- from the current cursor coordinates
!
      IUSE = 0
      DIST_MIN = 2.0+0.01  ! Square of the maximal distance
      IPQ = 0
      DO 410 J1=1,N
         IF ( XARR4(J1) .GE. XMIN4 .AND. XARR4(J1) .LE. XMAX4  .AND. &
     &        YARR4(J1) .GE. YMIN4 .AND. YARR4(J1) .LE. YMAX4        ) THEN
!
! ----------- Only points within plotting area are being taken into account
!
              IUSE = IUSE + 1
              DIST = ( (XARR4(J1) - XC)/(XMAX4 - XMIN4) )**2 + &
     &               ( (YARR4(J1) - YC)/(YMAX4 - YMIN4) )**2
              IF ( DIST .LT. DIST_MIN ) THEN
                   IPQ = J1
                   DIST_MIN = DIST
              END IF
         END IF
 410  CONTINUE
!
      IF ( IPQ .GT. 0 ) THEN
!
! -------- The point has been found
!
           CALL PGSAVE
           CALL PGSCI ( 0 )
!
! -------- Extinguishing this point at the plot
!
           IF ( IPST .EQ. 1 ) THEN
                CALL PGPNTS ( 1, XARR4(IPQ), YARR4(IPQ), 1, 1, 1 )
             ELSE IF ( IPST .EQ. 2  .OR.  IPST .EQ. 3  ) THEN
!
! -------------- Outlined circles
!
                 CALL PGSFS ( 2  )
                 CALL PGCIRC_PET ( NPTS, XARR4(IPQ), YARR4(IPQ), &
     &                             XRAD_WC, YRAD_WC )
               ELSE IF ( IPST .EQ. 4  .OR.  IPST .EQ. 5 ) THEN
                 CALL PGSFS ( 1  )
                 CALL PGCIRC_PET ( NPTS, XARR4(IPQ), YARR4(IPQ), &
     &                             XRAD_WC, YRAD_WC )
           END IF
!
! -------- Putting a marker with warning light
!
           CALL PGSCI ( 2 )
           CALL PGPT  ( 1, XARR4(IPQ), YARR4(IPQ), IMARK )
           CALL PGUNSA
!
! -------- Formatting description line
!
           CALL CLRCH ( MESQ )
           IF ( DIAGI_S%NCLR .GT. 1 ) THEN
!
! ------------- Colour index...
!
                CALL CLRCH ( STR )
                CALL INCH  ( ICLR, STR )
                MESQ = 'CLR='//STR(1:I_LEN(STR))//','
                IL = ILEN(MESQ) + 2
             ELSE
                IL = 1
           END IF
!
! -------- ... the index of the point
!
           CALL INCH  ( IPQ, STR )
           MESQ(IL:)='Point: '//STR
!
! -------- ... total number of points
!
           CALL INCH  ( N, STR )
           MESQ(ILEN(MESQ)+1:)='('//STR(1:I_LEN(STR))//')'
!
! -------- ... visible number of points
!
           CALL INCH  ( IUSE, STR )
           MESQ(ILEN(MESQ)+1:)='['//STR(1:I_LEN(STR))//']'
!
! -------- X-cordinate of the point
!
           CALL CLRCH  ( STR )
           WRITE ( UNIT=STR(1:15), FMT='(1PG15.8)' ) XARR4(IPQ)
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '.'  ) STR='0.'//STR(2:)
           IF ( STR(1:2) .EQ. '-.' ) STR='-0.'//STR(3:)
           MESQ(ILEN(MESQ)+1:)=' X='//STR
!
! -------- Y-coordinate of the point
!
           CALL CLRCH  ( STR )
           WRITE ( UNIT=STR(1:15), FMT='(1PG15.8)' ) YARR4(IPQ)
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '.'  ) STR='0.'//STR(2:)
           IF ( STR(1:2) .EQ. '-.' ) STR='-0.'//STR(3:)
           MESQ(ILEN(MESQ)+1:)=' Y='//STR
!
           IF ( LER ) THEN
!
! ------------- Error of the point
!
                CALL CLRCH  ( STR )
                WRITE ( UNIT=STR(1:15), FMT='(1PG15.8)' ) EARR4(IPQ)
                CALL CHASHL ( STR )
                IF ( STR(1:1) .EQ.  '.' ) STR='0.'//STR(2:)
                IF ( STR(1:2) .EQ. '-.' ) STR='-0.'//STR(3:)
                MESQ(ILEN(MESQ)+1:)=' E='//STR
           END IF
           DIAGI_S%MESS_BOT = MESQ
!
! -------- .. and printing information message
!
           CALL PGSAVE
           CALL PGSCF  ( 2              )
           CALL PGSCH  ( DIAGI_S%SCH_LAB  )
           CALL PGSLW  ( DIAGI_S%ISLW_LAB )
           CALL PGPTXT ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, &
     &                   0.0, 0.0, &
     &                   DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
           CALL PGUNSA
        ELSE
!
! -------- No points in the plotting area have been found.
! -------- Putting DiaGI_label at the screen again
!
           CALL PGSAVE
           CALL PGSCH  ( DIAGI_S%SCH_LAB  )
           CALL PGSLW  ( DIAGI_S%ISLW_LAB )
           CALL PGPTXT ( XMIN4, YMIN4 + (YMAX4-YMIN4)*DIAGI_S%YSH_LAB, &
     &                   0.0, 0.0, &
     &                   DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
           CALL PGUNSA
      END IF
!
! --- Flushing the output
!
      CALL PGEBUF
      CALL PGUPDT
!
      DIAGI_INQ = IPQ
!
      RETURN
      END  !#!  DIAGI_INQ  #!#
