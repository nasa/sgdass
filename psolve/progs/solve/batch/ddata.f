      SUBROUTINE DDATA ( ISTAD, NUM_STAINC, LIST_STAINC, NUM_STAEXC, LIST_STAEXC, &
     &                   NUM_SOUEXC, LIST_SOUEXC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DDATA PROGRAM SPECIFICATION
!
! 1.1 Delete station and source that were de-selected.
!
! 1.2 REFERENCES:
!
! 2.  DDATA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
      INTEGER*4   NUM_STAINC               ! number of stations to be included
      CHARACTER*8 LIST_STAINC(MAX_ARC_STA) ! List of 8-character names of
      INTEGER*4   NUM_STAEXC               ! number of stations to be excluded
      CHARACTER*8 LIST_STAEXC(MAX_ARC_STA) ! List of 8-character names of
!                                          ! stations to be included
      INTEGER*4   NUM_SOUEXC               ! number of sources to be excluded
      CHARACTER*8 LIST_SOUEXC(MAX_ARC_SRC) ! List of 8-character names of
!                                          ! sources to be excluded
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 ISTAD(*)
!
! ISTAD - Station data flag
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bdata.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  I, J, NUMBL
      CHARACTER  STA_NAM*8
      LOGICAL*2    FALSE_L2
      PARAMETER  ( FALSE_L2 = .FALSE. )
      LOGICAL*2    KSET, FL_STA_USE(MAX_ARC_STA), FL_FOUND
      LOGICAL*2    EQUAL, KBIT
      INTEGER*4    J1, J2, J3, J4, IUER
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   09-DEC-97   Added logic for supporting deslection status for stations
!   pet   2002.03.18  Removed unnecessary argument KFAIL from the arguments
!                     and from call to desta
!   pet   2004.03.16  Added a feature: to exclude sources as a session-line option
!   pet   2014.04.19  Change the logic: if the source has been deselected &
!                     in the database, it remains delected. The workaround is to use
!                     SOU_USE_DB_IGNORE option in the arc-line. That option lifts
!                     deselection flag stored in the database
!   pet   2020.06.24  Added support of NUM_STAINC, LIST_STAINC. Include has a preference &
!                     with respect to exclude
!   pet   2024.11.13  Added a check whether a selected/deselected station observed or not
!                     out for a long time
!   pet   2024.11.13  Implemented suppoert of phase delay solutions that was commented &
!                     out for a long time
!
! 5.  DDATA PROGRAM STRUCTURE
!
!
      IF ( NUM_STAINC .GT. 0 ) THEN
           FL_STA_USE = .FALSE.
           DO 410 J1=1,NUM_STAINC
              FL_FOUND = .FALSE.
              DO 420 J2=1,NUMSTA
                 IF ( LIST_STAINC(J1) == ISITN_CHR(J2) ) THEN
                      FL_STA_USE(J2) = .TRUE.
                      FL_FOUND = .TRUE.
                 END IF
 420          CONTINUE 
              IF ( .NOT. FL_FOUND ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6709, IUER, 'DDATA', 'Station '//LIST_STAINC(J1)// &
     &                 ' set to selection/deselection did not observe'// &
     &                 ' in experiment '//DBNAME_CH )
                   CALL EXIT ( 1 )
              END IF
 410       CONTINUE 
         ELSE
           FL_STA_USE = .TRUE.
      END IF
      IF ( NUM_STAEXC .GT. 0 ) THEN
           DO 430 J3=1,NUM_STAEXC
              DO 440 J4=1,NUMSTA
                 IF ( LIST_STAEXC(J3) == ISITN_CHR(J4) ) THEN
                      FL_STA_USE(J4) = .FALSE.
                 END IF
 440          CONTINUE 
 430       CONTINUE 
      END IF
!
      DO I=1,NUMSTA
         KSET=KDSTA
         DO J=1,NDSTA
            CALL MEMCPY ( STA_NAM, IDELAR(IDSTA+(J-1)*4) )
!!            IF ( EQUAL( IDELAR(IDSTA+(J-1)*4), INT2(1), ISITN(1,I), INT2(1), INT2(8)) ) THEN
            IF ( STA_NAM == ISITN_CHR(I) ) THEN
                 KSET = .NOT. KSET
                 GOTO 810
            ENDIF
         ENDDO
 810     CONTINUE
!
         IF ( .NOT. FL_STA_USE(I) ) KSET = FL_STA_USE(I) 
         CALL KSBIT ( ISTAD, I, KSET )
!
! ------ Turn off all parameters associated with this station
! ------ if they were deselected
!
         IF ( .NOT. KSET ) THEN
              CALL DESTA ( I )
              DO J=1,NUMSTA
                 IF ( IDATYP .EQ. PX__DTP .OR. IDATYP .EQ. PS__DTP ) THEN
                      CALL SBIT ( IBLSEL_P(1,J), I, INT2(0) ) ! There may be a problem because selection
                      CALL SBIT ( IBLSEL_P(1,I), J, INT2(0) ) ! status may be different for PX and PS
                   ELSE
                      CALL SBIT ( IBLSEL_G(1,J), I, INT2(0) )
                      CALL SBIT ( IBLSEL_G(1,I), J, INT2(0) )
                 END IF
              ENDDO
         ENDIF
      ENDDO
      NUMBL=0
      DO I=1,NUMSTA-1
         DO J=I+1,NUMSTA
            IF ( KBIT ( IBLSEL_G(1,I),J) ) NUMBL = NUMBL+1
         ENDDO
      ENDDO
      IF ( NUMBL .EQ. 1 ) THEN
          DO I=1,NUMSTA
             DO J=1,NUMSTA
                CALL SBIT( ICLOCK(1,I), J, INT2(0) )
                CALL SBIT( ICLOCK(1,J), I, INT2(0) )
             ENDDO
          ENDDO
      ENDIF
!
! --- Check for de-selected sources
!
      DO I=1,NUMSTR
         KSET=KDSRC
         DO J1=1,NDSRC
            IF ( EQUAL ( IDELAR(IDSRC+(J1-1)*4), INT2(1), ISTRN(1,I), INT2(1), INT2(8)) ) THEN
                 KSET=.NOT. KSET
                 GOTO 830
            ENDIF
         ENDDO
 830     CONTINUE
         IF ( NUM_SOUEXC .GT. 0 ) THEN
              DO J1=1,NUM_SOUEXC 
                 IF ( ISTRN_CHR(I) .EQ. LIST_SOUEXC(J1) ) THEN
                      KSET = .FALSE.
                 END IF
              END DO
         END IF
         IF ( KBIT( ISRSEL(1), INT2(I) ) ) THEN
              CALL KSBIT ( ISRSEL(1) , I, KSET )
            ELSE
              CALL KSBIT ( ISRSEL(1) , I, FALSE_L2 )
         END IF
      ENDDO
!
      RETURN
      END  SUBROUTINE  DDATA  !#!#
