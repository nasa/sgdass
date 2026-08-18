      SUBROUTINE REPPARM ( FILE_NAME, M_CLR, COL_KEY, COL_ATR, COL_NAM, &
     &                     STY_KEY, PNT_STY, BAD_KEY, SHOW_BAD, &
     &                     NUM_KEY, PPPL_M, PPPL_MAX, PAG_KEY, PAGE_UPDN, &
     &                     REPA__LABEL, IUER )
!
! *
! *   PURPOSE: read REPA parameter file
! *            If the parameter file is not available or the values are
! *            not o.k. or a keyword can not be found REPPARM creates a new file
! *            (REPAXY - XY are the user initials) with default parameter values
! *            in the work directory.
! *            The user can edit the file $WORK_DIR/REPAXY.
! *            If the keywords are not found, defaults are used (see repa.i).
! *
! *   called subroutines: CLRCH, CHASHL, GET_UNIT
! *   calling program: REPA
! *
! *   2003-01-10 Volkmar Thorandt
! *
!
      IMPLICIT   NONE
!
      INCLUDE       'diagi.i'
!
      INTEGER*4     M_CLR, PPPL_MAX
      CHARACTER     FILE_NAME*(*)        ! name of REPA parameter file
      CHARACTER     PAR_REC*80           ! temporary string
      INTEGER*4     GET_UNIT, LUN        ! unit
      INTEGER*4     J0, J1, J2, J3, J4   ! loop variables
      INTEGER*4     IPOS1, IPOS2, IPOS3  ! positions in PAR_REC
      INTEGER*4     IPOS4, IPOS5         ! positions in PAR_REC
      INTEGER*4     ITMP                 ! temporary integer value
      INTEGER*4     COL_ATR(M_CLR)       ! colour attributes
      CHARACTER     COL_NAM(M_CLR)*10    ! colour names
      INTEGER*4     PNT_STY(M_CLR)       ! point styles
      LOGICAL*4     SHOW_BAD             ! flag for bad observations display (yes=.TRUE.)
      CHARACTER     COL_KEY(M_CLR)*11    ! colour keywords
      CHARACTER     STY_KEY(M_CLR)*11    ! point style keywords for parameter file
      CHARACTER     BAD_KEY*11           ! show "bad" observations keyword for parameter file
      CHARACTER     NUM_KEY*11           ! max. # of plots per page (MultiDiaGi)
      INTEGER*4     PPPL_M               ! max. # of plots per page (MultiDiaGi)
      CHARACTER     PAG_KEY(2)*11        ! PgUp/PgDn keywords for parameter file
      INTEGER*4     PAGE_UPDN(2)         ! decimal codes of PgUp/PgDn keybord buttons
      INTEGER*4     ILEN                 ! length of character string
      LOGICAL*4     EX                   ! .TRUE. if file exists
      INTEGER*4     IFOUND               ! # of found parameters
      INTEGER*4     LENPAR               ! length of parameter
      INTEGER*4     IUER 
      INTEGER*4     IOS
      CHARACTER     REPA__LABEL*(*)
      CHARACTER  STR*80
      INTEGER*4  I_LEN
!
      LUN = GET_UNIT()
!
! --- Open parameter file REPAxx
!
      IOS = 0
      OPEN ( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN', IOSTAT=IOS )
      READ ( LUN, '(A80)', IOSTAT=IOS ) STR
      IF ( IOS .NE. 0 ) GOTO 460
      IF ( STR(1:LEN(REPA__LABEL)) .NE. REPA__LABEL ) THEN
           CALL ERR_LOG ( 7811, IUER, 'REPPARM', 'Error in processing '// &
     &         'REPA configuration file '//FILE_NAME(1:I_LEN(FILE_NAME))// &
     &         ' -- the first line is '//STR(1:I_LEN(STR))//' while '// &
     &         REPA__LABEL//' and the format date was expected' )
           RETURN 
      END IF
!
      IF ( STR(LEN(REPA__LABEL)+1:LEN(REPA__LABEL)+10) .LT. '2003.08.01' ) THEN
           CALL ERR_LOG ( 7812, IUER, 'REPPARM', 'Your REPA configuration '// &
     &         'file '//FILE_NAME(1:I_LEN(FILE_NAME))//' is too old. '// &
     &         'Please update it. Refer to REPA documentation for more '// &
     &         'information' )
           RETURN 
      END IF
!
! --- colours
!
      IFOUND = 0
      LENPAR = 2
      DO J0=1,M_CLR
            DO J1=1,10000
               CALL CLRCH ( PAR_REC )
               READ ( LUN, '(A80)', IOSTAT=IOS ) PAR_REC
               IF ( IOS .NE. 0 ) GOTO 460
               CALL CHASHL ( PAR_REC )
               IPOS1 = INDEX ( PAR_REC, COL_KEY(J0) )
               IF ( IPOS1 .GT. 0 .AND. PAR_REC(1:1) .NE. '#' ) THEN     ! keyword found
!C                write(6,*) 'REPPARM: PAR_REC(1:12)=',PAR_REC(1:12)
                  DO J2 = IPOS1+12,78
                     IF ( PAR_REC(J2:J2) .NE. ' ' ) THEN
                        IPOS2 = J2
                        DO J3=IPOS2,80-LENPAR+1
                           IF ( PAR_REC(J3:J3) .EQ. ' ' ) THEN
                              IPOS3 = J3 - 1
                              IF ( IPOS3 .LE. IPOS2+LENPAR-1 ) THEN
                                 DO J4=IPOS2,IPOS3
                                    IF ( ICHAR(PAR_REC(J4:J4)) .LT. 48   & ! ask for ASCII area
     &                                  .OR. ICHAR(PAR_REC(J4:J4)) .GT. 57 ) THEN
                                       WRITE ( 6, '(A)' ) 'Error 1'
                                       GOTO 460
                                    END IF
                                 END DO
                                 READ ( PAR_REC(IPOS2:IPOS3), '(I3)' ) ITMP
                                 IF ( ITMP .GT. MCLR .OR. ITMP .LT. 1) THEN
                                      WRITE ( 6, '(A)' ) 'Error 2'
                                      GOTO 460                           ! parameter value not o.k.
                                 ELSE
                                    COL_ATR(J0) = ITMP                 ! get colour name
                                    IFOUND = IFOUND + 1
                                    IPOS4 = INDEX ( PAR_REC, '"' )
                                    IF ( IPOS4 .GT. 0 ) THEN
                                       PAR_REC(IPOS4:IPOS4) = ' '
                                       IPOS5 = INDEX ( PAR_REC, '"' )
                                       IF ( IPOS5 .GT. 0 .AND. IPOS5-IPOS4+1 .LE. 10 ) THEN
                                          COL_NAM(J0) = PAR_REC(IPOS4+1:IPOS5-1)            ! get colour name
                                       ELSE
                                          WRITE ( 6, '(A)' ) 'Error 3'
                                          GOTO 460                      ! parameter value not o.k.
                                       END IF
                                    END IF
!C                                  write(6,*) 'REPPARM: COL_ATR(J0)=',COL_ATR(J0)
!C                                  write(6,*) 'REPPARM: COL_NAM(J0)=',COL_NAM(J0)
                                    GOTO 430
                                 END IF
                              END IF
                           END IF
                        END DO
                     END IF
                  END DO
               END IF
            END DO
 430        CONTINUE
            REWIND ( LUN )
            READ ( LUN, '(A80)', IOSTAT=IOS ) STR
         END DO
         IF ( IFOUND .NE. M_CLR ) THEN
              WRITE ( 6, * ) 'Error 4  ifound =',ifound,' m_clr=',m_clr
              GOTO 460                               ! wrong # of parameters found
         END IF
         REWIND ( LUN )
         READ ( LUN, '(A80)', IOSTAT=IOS ) STR
!
! ------ point styles
!
         IFOUND = 0
         LENPAR = 1
         DO J0=1,M_CLR
            DO J1=1,10000
               CALL CLRCH ( PAR_REC )
               READ ( LUN, '(A80)', IOSTAT=IOS ) PAR_REC
               IF ( IOS .NE. 0 ) GOTO 460
               CALL CHASHL ( PAR_REC )
               IPOS1 = INDEX ( PAR_REC, STY_KEY(J0) )
               IF ( IPOS1 .GT. 0 .AND. PAR_REC(1:1) .NE. '#' ) THEN     ! keyword found
!C                write(6,*) 'REPPARM: PAR_REC(1:12)=',PAR_REC(1:12)
                  DO J2 = IPOS1+12,80-LENPAR+1
                     IF ( PAR_REC(J2:J2) .NE. ' ' ) THEN
                        IPOS2 = J2
                        DO J3=IPOS2,80-LENPAR+1
                           IF ( PAR_REC(J3:J3) .EQ. ' ' ) THEN
                              IPOS3 = J3 - 1
                              IF ( IPOS3 .LE. IPOS2+LENPAR-1 ) THEN
                                 DO J4=IPOS2,IPOS3
                                    IF ( ICHAR(PAR_REC(J4:J4)) .LT. 48   & ! ask for ASCII area
     &                                  .OR. ICHAR(PAR_REC(J4:J4)) .GT. 57 ) THEN
                                       WRITE ( 6, '(A)' ) 'Error 5'
                                       GOTO 460
                                    END IF
                                 END DO
                                 READ ( PAR_REC(IPOS2:IPOS3), '(I3)' ) ITMP
                                 IF ( ITMP .GT. MPST .OR. ITMP .LT. 1 ) THEN
                                       WRITE ( 6, '(A)' ) 'Error 6'
                                     GOTO 460                           ! parameter value not o.k.
                                 ELSE
                                    PNT_STY(J0) = ITMP                  ! get point style
                                    IFOUND = IFOUND + 1
!C                                  write(6,*) 'REPPARM: PNT_STY(J0)=',PNT_STY(J0)
                                    GOTO 440
                                 END IF
                              END IF
                           END IF
                        END DO
                     END IF
                  END DO
               END IF
            END DO
 440        CONTINUE
            REWIND ( LUN )
            READ ( LUN, '(A80)', IOSTAT=IOS ) STR
         END DO
         IF ( IFOUND .NE. M_CLR ) THEN
              WRITE ( 6, '(A)' ) 'Error 7'
              GOTO 460                              ! wrong # of parameters found
         END IF
         REWIND ( LUN )
         READ ( LUN, '(A80)', IOSTAT=IOS ) STR
!
! --- show bad parameter
!
         DO J1=1,10000
            CALL CLRCH ( PAR_REC )
            READ ( LUN, '(A80)', IOSTAT=IOS ) PAR_REC
            IF ( IOS .NE. 0 ) GOTO 460
            CALL CHASHL ( PAR_REC )
            IPOS1 = INDEX ( PAR_REC, BAD_KEY )
            IF ( IPOS1 .GT. 0 .AND. BAD_KEY .NE. '#' ) THEN             ! keyword found
!C             write(6,*) 'REPPARM: PAR_REC(1:12)=',PAR_REC(1:12)
               DO J2 = IPOS1+12,80
                  IF ( PAR_REC(J2:J2) .NE. ' ' ) THEN
! ------------------ ask whether the value is 'yes' or 'no'
                     IF ( PAR_REC(J2:J2) .EQ. 'Y' .OR. PAR_REC(J2:J2) .EQ. 'y' ) THEN
                        SHOW_BAD = .TRUE.                               ! get show bad parameter
!C                      write(6,*) 'REPPARM: SHOW_BAD=',SHOW_BAD
                        GOTO 450
                     ELSE IF ( PAR_REC(J2:J2) .EQ. 'N' .OR. PAR_REC(J2:J2) .EQ. 'n' ) THEN
                        SHOW_BAD = .FALSE.                              ! get show bad parameter
!C                      write(6,*) 'REPPARM: SHOW_BAD=',SHOW_BAD
                        GOTO 450
                     ELSE
                        WRITE ( 6, '(A)' ) 'Error 8'
                        GOTO 460                                        ! parameter value not o.k.
                     END IF
                  END IF
               END DO
               WRITE ( 6, '(A)' ) 'Error 9'
               GOTO 460
            END IF
         END DO
         WRITE ( 6, '(A)' ) 'Error 10'
         GOTO 460
 450     CONTINUE
         REWIND ( LUN )
         READ ( LUN, '(A80)', IOSTAT=IOS ) STR
!
! ------ max. # of plots per MultiDiaGi page (not greater than PPPL_MAX !!)
!
         LENPAR = 3
         DO J1=1,10000
            CALL CLRCH ( PAR_REC )
            READ ( LUN, '(A80)', IOSTAT=IOS ) PAR_REC
            IF ( IOS .NE. 0 ) GOTO 460
            CALL CHASHL ( PAR_REC )
            IPOS1 = INDEX ( PAR_REC, NUM_KEY )
            IF ( IPOS1 .GT. 0 .AND. PAR_REC(1:1) .NE. '#' ) THEN        ! keyword found
!C             write(6,*) 'REPPARM: PAR_REC(1:12)=',PAR_REC(1:12)
               DO J2 = IPOS1+12,80-LENPAR+1
                  IF ( PAR_REC(J2:J2) .NE. ' ' ) THEN
                     IPOS2 = J2
                     DO J3=IPOS2,80-LENPAR+1
                        IF ( PAR_REC(J3:J3) .EQ. ' ' ) THEN
                           IPOS3 = J3 - 1
                           IF ( IPOS3 .LE. IPOS2+LENPAR-1 ) THEN
                              DO J4=IPOS2,IPOS3
                                 IF ( ICHAR(PAR_REC(J4:J4)) .LT. 48      & ! ask for ASCII area
     &                                .OR. ICHAR(PAR_REC(J4:J4)) .GT. 57 ) THEN
                                    WRITE ( 6, '(A)' ) 'Error 11'
                                    GOTO 460                            ! parameter value not o.k.
                                 END IF
                              END DO
                              READ ( PAR_REC(IPOS2:IPOS3), '(I3)' ) ITMP
                              IF ( ITMP .GT. PPPL_MAX .OR. ITMP .LT. 1 ) THEN
                                  WRITE ( 6, '(A)' ) 'Error 12'
                                  GOTO 460                              ! parameter value not o.k.
                              ELSE
                                 PPPL_M = ITMP                          ! get max. # of plots
!C                               write(6,*) 'REPPARM: PPPL_M=',PPPL_M
                                 GOTO 455
                              ENDIF
                           END IF
                        END IF
                     END DO
                  END IF
               END DO
            END IF
         END DO
         WRITE ( 6, '(A)' ) 'Error 13'
         GOTO 460
 455     CONTINUE
         REWIND ( LUN )
         READ ( LUN, '(A80)', IOSTAT=IOS ) STR
!
! ------ decimal codes of PgUp/PgDn keybord keys
!
         IFOUND = 0
         LENPAR = 3
         DO J0=1,2
            DO J1=1,10000
               CALL CLRCH ( PAR_REC )
               READ ( LUN, '(A80)', IOSTAT=IOS ) PAR_REC
               IF ( IOS .NE. 0 ) GOTO 460
               CALL CHASHL ( PAR_REC )
               IPOS1 = INDEX ( PAR_REC, PAG_KEY(J0) )
               IF ( IPOS1 .GT. 0 .AND. PAR_REC(1:1) .NE. '#' ) THEN     ! keyword found
!C                write(6,*) 'REPPARM: PAR_REC(1:12)=',PAR_REC(1:12)
                  DO J2 = IPOS1+12,80-LENPAR+1
                     IF ( PAR_REC(J2:J2) .NE. ' ' ) THEN
                        IPOS2 = J2
                        DO J3=IPOS2,80-LENPAR+1
                           IF ( PAR_REC(J3:J3) .EQ. ' ' ) THEN
                              IPOS3 = J3 - 1
                              IF ( IPOS3 .LE. IPOS2+LENPAR-1 ) THEN
                                 DO J4=IPOS2,IPOS3
                                    IF ( ICHAR(PAR_REC(J4:J4)) .LT. 48   & ! ask for ASCII area
     &                                  .OR. ICHAR(PAR_REC(J4:J4)) .GT. 57 ) THEN
                                       WRITE ( 6, '(A)' ) 'Error 14'
                                       GOTO 460
                                    END IF
                                 END DO
                                 READ ( PAR_REC(IPOS2:IPOS3), '(I3)' ) ITMP
                                 PAGE_UPDN(J0) = ITMP                  ! get PgUp/PgDn code
                                 IFOUND = IFOUND + 1
!C                               write(6,*) 'REPPARM: PAGE_UPDN(J0)=',PAGE_UPDN(J0)
                                 GOTO 457
                              END IF
                           END IF
                        END DO
                     END IF
                  END DO
               END IF
            END DO
 457        CONTINUE
            REWIND ( LUN )
            READ ( LUN, '(A80)', IOSTAT=IOS ) STR
         END DO
         IF ( IFOUND .NE. 2 ) THEN
              CALL ERR_LOG ( 7813, IUER, 'REPAPARM', 'Wrong number of '// &
     &            'parameters in REPA configuration file '//FILE_NAME )
              CLOSE ( UNIT=LUN )
              RETURN 
         END IF
!
         GOTO 500
!
! ------ user parameter file does not exist or parameter not o.k. or keyword not found
! ------ --> create default parameter file
!
 460  CONTINUE
      CLOSE ( UNIT=LUN )
      CALL CLRCH  ( STR )
      CALL INCH   ( IOS, STR ) 
      CALL ERR_LOG ( 7814, IUER, 'REPAPARM', 'Error '//STR(1:I_LEN(STR))// &
     &    ' in reading REPA configuration file '//FILE_NAME )
      RETURN
!@      REWIND ( LUN )
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '##  REPA parameter file                                                       ##'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '# colour attributes from DiaGi colour set                                     ##'
!@      PAR_REC =  '# range: [1,MCLR] (s. diagi.i, currently MCLR=32)                             ##'
!@      WRITE ( PAR_REC(47:48), '(I2)' ) MCLR
!@      WRITE ( LUN, '(A80)/$' ) PAR_REC
!@      WRITE ( LUN, '(A80)/$' ) '# Call diagi_dec to display colours!                                          ##'
!@      WRITE ( LUN, '(A80)/$' ) '# The colour names are displayed in some cases (i.e. for met. data).,         ##'
!@      WRITE ( LUN, '(A80)/$' ) '# the user can create them including name in "".                              ##'
!@      WRITE ( LUN, '(A80)/$' ) '# In case of wrong or missing parameters defaults are used.                   ##'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A57)/$' ) '# colours of "good", "recoverable" and "bad" observations'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@! --- colours
!@      DO J1=1,M_CLR
!@         IF ( J1 .EQ. 4 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A40)/$' ) '# colours of "zeroline" and "sigmalines"'
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         IF ( J1 .EQ. 7 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A68)/$' ) '# colour of connecting lines if displayed observations with the same'
!@            WRITE ( LUN, '(A26)/$' ) '# source will be connected '
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         IF ( J1 .EQ. 8 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A59)/$' ) '# colours for display of meteorological data (two stations)'
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         CALL CLRCH ( PAR_REC )
!@         PAR_REC(1:12) = COL_KEY(J1)//':'
!@         WRITE ( PAR_REC(14:15), '(I2)' ) COL_ATR(J1)
!@         PAR_REC(17:16+ILEN(COL_NAM(J1))+2) = '"'//COL_NAM(J1)(1:ILEN(COL_NAM(J1)))//'"'
!@         WRITE ( LUN, '(A80)/$' ) PAR_REC
!@      END DO
!@      WRITE ( LUN, '(A1)' ) '#'
!@! --- point styles
!@      WRITE ( LUN, '(A1)/$' ) '#'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '# point styles from DiaGi point style set                                     ##'
!@      WRITE ( LUN, '(A80)/$' ) '# range: [1,5]                                                                ##'
!@      WRITE ( LUN, '(A80)/$' ) '# More information in DiaGi users guide.                                      ##'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A62)/$' ) '# point styles of "good", "recoverable" and "bad" observations'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      DO J1=1,M_CLR
!@         IF ( J1 .EQ. 4 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A45)/$' ) '# point styles of "zeroline" and "sigmalines"'
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         IF ( J1 .EQ. 7 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A73)/$' ) '# point style of connecting lines if displayed observations with the same'
!@            WRITE ( LUN, '(A26)/$' ) '# source will be connected '
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         IF ( J1 .EQ. 8 ) THEN
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@            WRITE ( LUN, '(A64)/$' ) '# point styles for display of meteorological data (two stations)'
!@            WRITE ( LUN, '(A1)/$' )  '#'
!@         END IF
!@         CALL CLRCH ( PAR_REC )
!@         PAR_REC(1:12) = STY_KEY(J1)//':'
!@         WRITE ( PAR_REC(14:15), '(I2)' ) PNT_STY(J1)
!@         WRITE ( LUN, '(A80)/$' ) PAR_REC
!@      END DO
!@      WRITE ( LUN, '(A1)' ) '#'
!@! --- show bad parameter
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '# flag for displaying "bad" obsevations                                       ##'
!@      WRITE ( LUN, '(A80)/$' ) '# values: yes/no                                                              ##'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      CALL CLRCH ( PAR_REC )
!@      PAR_REC(1:12) = BAD_KEY//':'
!@      PAR_REC(14:16) = 'yes'
!@      WRITE ( LUN, '(A80)/$' ) PAR_REC
!@! --- max. # of plots per MultiDiaGi page
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '# max. # of plots per MultiDiaGi page (prefere square numbers!)               ##'
!@      PAR_REC = '# values: not greater than 036                                                ##'
!@      WRITE ( PAR_REC(28:30), '(I2)' ) PPPL_MAX
!@      WRITE ( LUN, '(A80)/$' ) PAR_REC
!@      WRITE ( LUN, '(A80)/$' ) '# For greater value increase PPPL_MAX in repa.i and recompile/link REPA.      ##'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      CALL CLRCH ( PAR_REC )
!@      PAR_REC(1:12) = NUM_KEY//':'
!@      WRITE ( PAR_REC(14:16), '(I3)' ) PPPL_M
!@      WRITE ( LUN, '(A80)/$' ) PAR_REC
!@! --- decimal codes of PgUp/PgDn keybord keys
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A80)/$' ) '# decimal codes for PgUp (next baseline) and PgDn (prev. bl.) key on keybord   #'
!@      WRITE ( LUN, '(A80)/$' ) '# Call diagi_key to find the codes!                                            #'
!@      WRITE ( LUN, '(A80)/$' ) '################################################################################'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      DO J1=1,2
!@         CALL CLRCH ( PAR_REC )
!@         PAR_REC(1:12) = PAG_KEY(J1)//':'
!@         WRITE ( PAR_REC(14:16), '(I3)' ) PAGE_UPDN(J1)
!@         WRITE ( LUN, '(A80)/$' ) PAR_REC
!@      END DO
!@! --- end of file
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@      WRITE ( LUN, '(A13)/$' ) '# end of file'
!@      WRITE ( LUN, '(A1)/$' )  '#'
!@!
  500 CONTINUE 
      CLOSE ( LUN )
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  !#!  REPPARM  #!#
