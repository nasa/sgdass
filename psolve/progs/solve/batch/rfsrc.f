      SUBROUTINE RFSRC ( FIXNAM_CHR, FIXSRC_CHR, ISRSEL, ISTRN_CHR, NUMSTR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RFSRC PROGRAM SPECIFICATION
!
! 1.1 Select reference source.
!
! 1.2 REFERENCES:
!
! 2.  RFSRC INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISRSEL(*), NUMSTR
!
! FIXSRC - Specified reference source name (or PICK)
! ISRSEL - source selection flag
! ISTRN - Array of source names
! NUMSTR - Total number of sources
!
! 2.3 OUTPUT Variables:
!
      CHARACTER  FIXNAM_CHR*(*), FIXSRC_CHR*(*), ISTRN_CHR(NUMSTR)*(*)
!
! FIXNAM - Reference source name determined in this routine
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 SRCTER(2)
      INTEGER*2 IDUM, IFIX, ISRC, I
      LOGICAL*2 KBIT,EQUAL
!
! First choices if PICK is indicated
!
      DATA SRCTER/'3C273B  ','OJ287   '/
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet  2005.03.18  Reaplced archaic calls of ICHCM, ICHMV with modern string &
!                    manipulation operators
!
! 5.  RFSRC PROGRAM STRUCTURE
!
! If a source name is specified, that's what we'll use
!
      IF ( FIXNAM_CHR == 'PICK    ' ) THEN
!
! -------- Otherwise, find a reference source, first from top choices 
! -------- listed above
!
           DO IFIX=1,2
              DO ISRC=1,NUMSTR
                 IF ( SRCTER(IFIX) == ISTRN_CHR(ISRC) .AND. &
     &                KBIT(ISRSEL,ISRC)                     ) THEN
                      GOTO 200
                 ENDIF
              ENDDO
          ENDDO
!
! ------- Since topo choices weren't available, take the first that is
!
          DO ISRC=1,NUMSTR
             IF ( KBIT(ISRSEL,ISRC) ) THEN
                  FIXNAM_CHR = ISTRN_CHR(ISRC)
                  GOTO 200
             ENDIF
          ENDDO
      ENDIF
200   CONTINUE
!
      RETURN
      END  SUBROUTINE  RFSRC 
