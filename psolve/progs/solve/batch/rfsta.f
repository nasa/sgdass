      SUBROUTINE RFSTA ( FIXNAM_CHR, FIXSTA_CHR, ISTAD, ISITN_CHR, NUMSTA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RFSTA PROGRAM SPECIFICATION
!
! 1.1 Pick a reference station.
!
! 1.2 REFERENCES:
!
! 2.  RFSTA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTAD(*),NUMSTA
!
! FIXSTA - Specified reference station (or PICK)
! ISITN - Array of station names
! ISTAD - Station data flag
! NUMSTA - Total number of stations
!
! 2.3 OUTPUT Variables:
!
!
! FIXNAM - Name of the reference station
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER MASTER*8, FINAM*128
      INTEGER*2 IMASTR(4),IDUM,ICHMV,IFIX,ISTA,I,ICHCM
      CHARACTER ISITN_CHR(NUMSTA)*(*), FIXNAM_CHR*(*), FIXSTA_CHR*(*)
      CHARACTER  STA_NAM*8
      INTEGER*4 IOS
      LOGICAL*2 KBIT, EQUAL
      LOGICAL*4 CHECK_STABIT
!
      EQUIVALENCE (MASTER,IMASTR(1))
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   MWH  930712      Put list of top choices in a file
!   pet  2000.07.27  Prohibited taking as a reference station which has been
!                    deselected
!   pet  2005.03.18  Reaplced archaic calls of ICHCM, ICHMV with modern string &
!                    manipulation operators
!
! 5.  RFSTA PROGRAM STRUCTURE
!
! If a station is specified, then that's what we'll use
!
      CALL CLRCH ( FIXNAM_CHR )
      IF ( FIXSTA_CHR == 'PICK    ' ) THEN
!
! -------- Otherwise, try to pick one, first from choices listed in file
!
           FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//STATION_PICK_FILE
           OPEN ( UNIT=67, FILE=FINAM, IOSTAT=IOS )
           CALL FERR ( INT2(IOS), " BATCH(rfsts) Opening Station PICK file "// &
     &                 FINAM, INT2(0), INT2(0) )
           DO WHILE ( .TRUE. )
              READ ( 67, '(A8)', END=50, IOSTAT=IOS ) STA_NAM
              CALL FERR ( INT2(IOS), "BATCH(rfsts) Reading Station PICK list "// &
     &                    FINAM, INT2(0), INT2(0) )
!
              DO ISTA=1,NUMSTA
                 IF ( STA_NAM == ISITN_CHR(ISTA) .AND. &
     &                KBIT(ISTAD,ISTA)           .AND. &
     &                CHECK_STABIT( ISTA )             ) THEN
!
                      FIXNAM_CHR = STA_NAM 
                      GOTO 100
                 ENDIF
              ENDDO
           ENDDO
!
! -------- If none was found, fick the first station not deleted
!
50        CONTINUE
          DO ISTA=1,NUMSTA
             IF ( KBIT(ISTAD,ISTA)  .AND.  CHECK_STABIT ( ISTA ) ) THEN
                  FIXNAM_CHR = ISITN_CHR(ISTA)
                  GOTO 100
             ENDIF
          ENDDO
      ENDIF
100   CONTINUE
      CLOSE ( UNIT=67 )
!
      RETURN
      END  !#!  RFSTA  #!#
