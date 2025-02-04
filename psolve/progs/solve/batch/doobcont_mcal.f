      SUBROUTINE DOOBCONT_MCAL ( FL_NOCAL, FL_NOCONT, FL_NOMAP, &
     &                           DBNAME, ICONS, NAMCON, OBCAVL, OBCAPL, &
     &                           L_CLMR, NAMMCAL, MCAVL, MCAPL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOOBCONT_MCAL PROGRAM SPECIFICATION
!
! 1.1 Change status of observation dependent contributions and mode calibrations
!
! 1.2 REFERENCES:
!
! 2.  DOOBCONT_MCAL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICONS, L_CLMR, OBCAVL, MCAVL
      CHARACTER DBNAME*(*), NAMCON(ICONS)*(*), NAMMCAL(L_CLMR)*(*)
!
! ICONS - Number of contributions from NAMFIL
! NAMCON - Contribution names from NAMFIL
! OBCAVL -
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 OBCAPL, MCAPL
      LOGICAL*4 FL_NOCAL, FL_NOCONT, FL_NOMAP
!
! OBCAPL -
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcont.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: docont
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, J, J2, J3
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   2006.02.08  Added support of a case when no calibrations and/or &
!                     contributions and/or mapping are to be applied
!
! 5.  DOOBCONT_MCAL PROGRAM STRUCTURE
!
      IF ( CONTRB(1) .EQ. 'DEFAULT'  .OR. FL_NOCONT ) GOTO 810
!
      OBCAPL=0
      IF ( CONTRB(1) .EQ. 'NONE' .OR. FL_NOCONT ) GOTO 810
!
      DO 410 I=1,ICONTR
         DO J=1,ICONS
            IF ( CONTRB(I) .EQ. NAMCON(J) .AND. KBIT(OBCAVL,J) ) THEN
                 CALL SBIT( OBCAPL, J, INT2(1) )
                 GOTO 410
            ENDIF
         ENDDO
!
         CALL FERR ( INT2(1910), 'BATCH(doobcont_mcal): contribution :'// &
     &        CONTRB(I)//': specified in the control file is missing in the '// &
     &       'superfile '//DBNAME, INT2(0), INT2(0) )
 410  CONTINUE
!
 810  CONTINUE
      IF ( L_CLM .GT. 0 ) THEN
           IF ( MCAL(1)(1:8) .EQ. 'IN      ' ) GOTO 820
           MCAPL = 0
           IF ( MCAL(1)      .EQ. 'NONE    ' ) GOTO 820
           DO 420 J2=1,L_CLM
              DO 430 J3=1,L_CLMR
                 IF ( NAMMCAL (J3) .EQ. MCAL(J2) .AND. KBIT ( MCAVL, J3 ) ) THEN
                      CALL SBIT ( MCAPL, J3, INT2(1) )
                      GOTO 420
                 END IF
 430          CONTINUE
!
              CALL FERR ( INT2(1920), &
     &            'BATCH(doobcont_mcal): mode calibration :'//MCAL(J2)// &
     &            ': specified in the control file is missing '// &
     &            'in the superfile '//DBNAME, INT2(0), INT2(0) )
420       CONTINUE
      END IF
 820  CONTINUE
!
      RETURN
      END  !#!  DOOBCONT_MCAL  #!#
