      SUBROUTINE DOCONT_MCAL ( FL_NOCAL, FL_NOCONT, FL_NOMAP, ICONS, NAMCON, &
     &                         L_CLM, NAMMCAL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOCONT_MCAL PROGRAM SPECIFICATION
!
! 1.1 Set up contributions.
!
! 1.2 REFERENCES:
!
! 2.  DOCONT_MCAL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICONS, L_CLM
      LOGICAL*4 FL_NOCAL, FL_NOCONT, FL_NOMAP
      CHARACTER NAMCON(ICONS)*(*), NAMMCAL(L_CLM)*(*)
!
! ICONS - Number of contributions from NAMFIL
! NAMCOM - Contribution names from NAMFIL
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setnam
!       CALLED SUBROUTINES: doobcont
!
! 3.  LOCAL VARIABLES
!
      CHARACTER LCARD*4, JBUF*70, DBNAME*15
      INTEGER*2 NUMDB, ICONT, IERR, OBCAPL, OBCAVL, MCAVL, MCAPL
      INTEGER*4 IOS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   2006.02.08  Added support of a case when no calibrations and/or &
!                     contributions and/or mapping are to be applied
!
! 5.  DOCONT_MCAL PROGRAM STRUCTURE
!
      NUMDB = 1
      LCARD = 'INIT'
      ICONT = 1
      CALL GETCARD(NUMDB,LCARD,ICONT,JBUF,IERR )
!
      DBNAME = JBUF(11:20)//' <'//JBUF(23:24)//'>'
!
      NUMDB = 1
      LCARD = 'CONT'
      ICONT = 1
      CALL GETCARD(NUMDB,LCARD,ICONT,JBUF,IERR )
!
      ICONT=0
      DO WHILE(IERR.EQ.0)
         READ ( JBUF, FMT='(A4,1X,4I7,37X)', IOSTAT=IOS ) LCARD, OBCAVL, &
     &                                              OBCAPL, MCAVL, MCAPL
         CALL FERR ( INT2(IOS), "BATCH(docont_mcal) Reading CONT card", INT2(0), &
     &               INT2(0) )
!
         CALL DOOBCONT_MCAL ( FL_NOCAL, FL_NOCONT, FL_NOMAP, DBNAME, ICONS, &
     &                        NAMCON, OBCAVL, OBCAPL, L_CLM, NAMMCAL, MCAVL, &
     &                        MCAPL )
!
         WRITE ( JBUF, FMT='(A4,1X,4I7,37X)', IOSTAT=IOS ) LCARD, OBCAVL, &
     &                                               OBCAPL, MCAVL, MCAPL
!
         CALL PUTCARD ( NUMDB, LCARD, INT2(4), JBUF, IERR )
         CALL FERR ( IERR, "BATCH(docont_mcal) Putting namfil card", INT2(0), &
     &        INT2(0) )
!
         CALL GETCARD ( NUMDB, LCARD, ICONT, JBUF, IERR )
         CALL FERR ( IERR, 'BATCH(docont_mcal) Getting namfil card', INT2(1), &
     &        INT2(0) )
      ENDDO
!
      RETURN
      END  !#!  DOCONT_MCAL  #!#
