      SUBROUTINE GVELTIE ( NUMGRP, VELTIES, STASUP_CHR, ISTASP, TOKEN, STRING, &
     &                     DEFVEL, VELSUP, DEFCMP, CMPSUP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GVELTIE PROGRAM SPECIFICATION
!
! 1.1 Parse VELOCITY_TIE line of $SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GVELTIE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER string*(*)
      INTEGER*2 DEFVEL(2), DEFCMP
      INTEGER*2 VELSUP(STA_BIT_WORDS,*), CMPSUP(STA_BIT_WORDS,*)
!
! STRING - Remainder of VELOCITY_TIE line
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2     NUMGRP, VELTIES(*), ISTASP
      CHARACTER     STASUP_CHR(MAX_STA)*(*), TOKEN*(*)
!
! ISTASP - Number of stations in suppression list
! NUMGRP - Number of distinct groups of velocity-tied stations
! STASUP - List of station names for suppression
! TOKEN - Single token from STRING
! VELTIES - Group number to which each station belongs (0 = no group)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gsuprs
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF
      CHARACTER TEMP*8, STRING_ORIG*128
      INTEGER*2 I, LENGTH, CFREAD, IDUM, IPOS, IL
      CHARACTER    BSLASH*1
      PARAMETER  ( BSLASH = '\' )
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR  921215  replaced '\' with BSLASH
!   mwh  910318  created
!   kdb  961125  declare cmpsup,velsup with *
!   pet  2000.05.10  Added support of the qualifiers YES and NO
!   pet  2000.08.01  Imrpoved comments
!   pet  2004.03.15  Added a diagnostic of a siyutaiton when a user tried
!                    to use the same station for velocity ties constraints
!                    more than once. Internal logic of applying constraints
!                    does not allow it.
!
! 5.  GVELTIE PROGRAM STRUCTURE
!
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'NO' ) THEN
           NUMGRP = 0
           RETURN
        ELSE IF ( TOKEN .EQ. 'YES' ) THEN
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF
!
      NUMGRP=1
      DO WHILE (TOKEN.NE.' ')
         IF ( TOKEN .EQ. 'AND' ) THEN
              NUMGRP=NUMGRP+1
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
           ELSE
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH=CFREAD(STRING)
                   IF ( STRING(1:1) .EQ. '$'  .OR. CFEOF(IDUM) ) THEN
                        CALL FERR ( INT2(9020), 'GVELTIE(BATCH) Illegal '// &
     &                      'continuation line in VELOCITY_TIE section '// &
     &                       STRING(1:10), INT2(0), INT2(0) )
                   END IF
                   STRING_ORIG = STRING
                   IL = I_LEN(STRING_ORIG)
                   IF ( STRING_ORIG(IL:IL) .EQ. '/' ) THEN
                        STRING_ORIG(IL:IL) = ' '
                   END IF
                ELSE
!
! ---------------- Check: whether this station has already been specified
!
                   DO I=1,ISTASP
                      IPOS=I
                      IF ( TOKEN .EQ. STASUP_CHR(I) ) THEN
                           IF ( VELTIES(I) .NE. 0  .AND.  &
     &                          VELTIES(I) .NE. NUMGRP    ) THEN
!
                                CALL FERR ( INT2(9030), 'GVELTIE(BATCH) '// &
     &                              'Error in parsing string '// &
     &                               STRING_ORIG(1:IL)// &
     &                              ' in $SUPPRESSION section of the'// &
     &                              ' control file: the station '// &
     &                              STASUP_CHR(I)//' has been already '// &
     &                              'designated for participation in '// &
     &                              'velocity ties constraint. Refer to '// &
     &                              'Solve user guide.', INT2(0), INT2(0) )
                           END IF
                           GOTO 90
                      END IF
                   ENDDO
                   ISTASP=ISTASP+1
                   IF ( ISTASP .GT. MAX_STA ) THEN
                        CALL FERR ( INT2(9040), 'GVELTIE(BATCH) too many '// &
     &                      'velocities in VELOCITY_TIE '//TOKEN(1:10), &
     &                       INT2(0), INT2(0) )
                   END IF
                   STASUP_CHR(ISTASP) = TOKEN
                   IPOS=ISTASP
90                 CONTINUE
                   VELTIES(IPOS)=NUMGRP
              ENDIF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  GVELTIE  #!#
