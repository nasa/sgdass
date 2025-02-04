      SUBROUTINE CLATM(ISITE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CLATM PROGRAM SPECIFICATION
!
! 1.1 Clear out all the atmosphere epochs for this site, except
!     for the first one.
!
! 1.2 REFERENCES:
!
! 2.  CLATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   ISITE
!
! ISITE - Site number of the station being processed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setep_sta
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   J,KBITN,OLDPOS,NEWPOS,K
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CLATM PROGRAM STRUCTURE
!
      IF(NUMATM(ISITE).GT.1) THEN
!
!   Clear the epochs and the flags for the atms succeeding
!   start with the SECOND epoc
!
          DO J=IATSTR(ISITE)+2, &
     &         IATSTR(ISITE)+NUMATM(ISITE)
              TATM(J)=0.0
              CALL SBIT( LATM(1,1), J, INT2(0) )
              CALL SBIT( LATM(1,2), J, INT2(0) )
              CALL SBIT( LATM(1,3), J, INT2(0) )
          ENDDO
!
!   Decrement the counters for the succeeding stations
!
          IF(ISITE.LT.NUMSTA) THEN
              DO J=ISITE+1,NUMSTA
                   IATSTR(J)=IATSTR(J)-NUMATM(ISITE)+1
                  DO K= 1,NUMATM(J)
                      OLDPOS=IATSTR(J)+K+NUMATM(ISITE)-1
                      NEWPOS=IATSTR(J)+K
                      TATM(NEWPOS)=TATM(OLDPOS)
                      TATM(OLDPOS)=0.0
                      CALL SBIT(LATM(1,1),NEWPOS, &
     &                          KBITN(LATM(1,1),OLDPOS) )
                      CALL SBIT(LATM(1,2),NEWPOS, &
     &                          KBITN(LATM(1,2),OLDPOS) )
                      CALL SBIT(LATM(1,3),NEWPOS, &
     &                          KBITN(LATM(1,3), OLDPOS) )
                      CALL SBIT( LATM(1,1), OLDPOS, INT2(0) )
                      CALL SBIT( LATM(1,2), OLDPOS, INT2(0) )
                      CALL SBIT( LATM(1,3), OLDPOS, INT2(0) )
                  ENDDO
              ENDDO
          ENDIF
!
!   Reset the atmosphere counter for this station
!
          NUMATM(ISITE)=1
      ENDIF
      RETURN
      END
