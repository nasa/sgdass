      SUBROUTINE SETNAM ( FL_NOCAL, FL_NOCONT, FL_NOMAP, IONCTL, DBNAME_MES, &
     &                    FL_GVF, GVF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
!
! 1.  SETNAM PROGRAM SPECIFICATION
!
! 1.1 Set up the parts of NAMFIL which handle calibrations and contributions
!      and the ionosphere.
!
! 1.2 REFERENCES:
!
! 2.  SETNAM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER IONCTL*(*), DBNAME_MES*(*)
!
! IONCTL - Ion control variable
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: gcalnm,docali,docont,
!                           putcard,put_cln_card,ferr,fatal
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 MAX_CALS,MAX_CONS,MAX_FCALS
      PARAMETER (MAX_CALS=32, &
     &           MAX_FCALS=112, &
     &           MAX_CONS=16)
      LOGICAL*4  FL_NOCAL, FL_NOCONT, FL_NOMAP, FL_GVF
      INTEGER*4  GVF
!
      CHARACTER*8  NAMCAL(MAX_CALS), NAMFCAL(MAX_FCALS), NAMCON(MAX_CONS), &
     &             NAMMCAL(M_CLM)
      INTEGER*2    ICALS, IFCALS, ICONS, IERR, IFCALS_SAVE, IZCALS, L_CLM
      CHARACTER*4 LCARD
      CHARACTER*70 JBUF
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   1999.11.18  Added support of mode calibrations
!   pet   2006.02.08  Added support of a case when no calibrations and/or &
!                     contributions and/or mapping are to be applied
!   pet   2006.07.06  Fixed a bug: FL_NOCAL flag was not honored correctly
!
!
! 5.  SETNAM PROGRAM STRUCTURE
!
!
! --- Get contributions and calibrations names and counts from NAMFIL
!
      CALL GCALNM ( FL_NOCAL, FL_NOCONT, FL_NOMAP, NAMCAL, NAMFCAL, NAMCON, &
     &              NAMMCAL, ICALS, IFCALS, ICONS, L_CLM, &
     &              MAX_CALS, MAX_FCALS, MAX_CONS, M_CLM, IZCALS )
!
! -------- Save off number of flyby calibrations in case some are added
!
      IFCALS_SAVE = IFCALS
!
! --- Set up station dependent calibrations
!
      CALL DOCALI ( FL_NOCAL, NAMCAL, NAMFCAL, ICALS, IFCALS, IONCTL, &
     &              DBNAME_MES )
!
! --- Set up contributions and mode calibrations
!
      CALL DOCONT_MCAL ( FL_NOCAL, FL_NOCONT, FL_NOMAP, ICONS, NAMCON, &
     &                   L_CLM, NAMMCAL )
!
! --- If any calibrations were added to the local list of flyby names,
! --- write them out to the namfil for this data base
!
      IF ( IFCALS .GT. IFCALS_SAVE  .AND.  .NOT. FL_NOCAL ) THEN
           LCARD = 'FCLN'
           CALL PUT_CLN_CARD ( INT2(1), LCARD, INT2(1), IFCALS, NAMFCAL, IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL FERR ( IERR, 'IN SETNAM WRITING NEW NAMFIL LIST OF '// &
     &              'FLYBY NAMES', INT2(0), INT2(0) )
                CALL FATAL ( 'IN SETNAM WRITING NEW NAMFIL LIST OF FLYBY NAMES' )
           END IF
!
           LCARD = 'CLCT'
           WRITE ( JBUF, "(A4,5(1X,I3),46X)" ) LCARD, ICALS, IFCALS, IZCALS, &
     &                                         ICONS, L_CLM
           CALL PUTCARD( INT2(1), LCARD, INT2(1), JBUF, IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL FERR ( IERR, 'IN SETNAM WRITING NEW NAMFIL COUNT CARD', &
     &               INT2(0), INT2(0) )
                CALL FATAL ( 'IN SETNAM WRITING NEW NAMFIL COUNT CARD' )
           END IF
      END IF
!
      RETURN
      END  !#!  SETNAM  #!#
