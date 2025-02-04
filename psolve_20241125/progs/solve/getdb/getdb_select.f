      SUBROUTINE GETDB_SELECT ( GVF_ENV_DIR, FILENV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GETDB_SELECT 
! *                                                                      *
! *  ### 08-DEC-2005   GETDB_SELECT  v2.1 (c) L. Petrov  22-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  GVF_ENV_DIR*(*), FILENV*(*)
      INTEGER*4  IUER
      INTEGER*4  M_ENV
      PARAMETER  ( M_ENV = 32*1024 )
      CHARACTER, ALLOCATABLE :: F_ENV(:)*128, F_ALT(:)*128
      CHARACTER  STR*128, STR1*128, FINAM*128
      CHARACTER  KEY_CH*4, PATTERN*128
      INTEGER*4  NS
      PARAMETER  ( NS = 20 )
      INTEGER*4  LEV, DIR_DESC(32), J1, J2, J3, J4, IP, L_ENV, L_ALT, KEY, &
     &           IX, IY, IB, IC, NB, NE, NC, LE, IER
      EQUIVALENCE ( KEY, KEY_CH )
      LOGICAL*4  FL_ALL
      LOGICAL*4, EXTERNAL :: MATCH_WILD 
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, LINDEX 
!
      ALLOCATE ( F_ENV(M_ENV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_ENV*128, STR )
           CALL ERR_LOG ( 8781, IUER, 'GETDB_SELECT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynbamic memory' )
           RETURN 
      END IF
!
      ALLOCATE ( F_ALT(M_ENV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_ENV*128, STR )
           CALL ERR_LOG ( 8782, IUER, 'GETDB_SELECT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynbamic memory' )
           RETURN 
      END IF
      LEV = 0
!
      L_ENV = 0
      DO 410 J1=1,M_ENV
         IP = GET_FILE_FROM_DIR ( LEV, DIR_DESC, GVF_ENV_DIR, FINAM )
         IF ( IP .NE. 0 ) THEN
              CALL ERR_LOG ( 8781, IUER, 'GETDB_SELECT', 'Error in '// &
     &            'examining contents of directory '//GVF_ENV_DIR )
              RETURN 
         END IF
         IF ( LEV .EQ. 0 ) GOTO 810 ! No more files? exit
!
         IF ( ILEN(FINAM) .GT. LEN('.env') ) THEN
              IF ( FINAM(ILEN(FINAM)-LEN('.env')+1:ILEN(FINAM)) == '.env' ) THEN
                   IB = LINDEX ( FINAM, '/' ) + 1
                   L_ENV = L_ENV + 1
                   F_ENV(L_ENV) = FINAM(IB:)
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_ENV .LE. 0 ) THEN
           CALL ERR_LOG ( 8782, IUER, 'GETDB_SELECT', 'Direcotry '// &
     &         'with database envelops '//GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))// &
     &         ' is empty. Nothing to select' )
           RETURN 
      END IF
!
      CALL SORT_CH ( L_ENV, F_ENV )
!
      CALL START_MN()
!
      FL_ALL = .TRUE.
 900  CONTINUE 
      CALL CLRCH ( STR )
      STR = 'Please select a database         -       (     )'
      WRITE ( UNIT=STR(28:32), FMT='(I5)' ) 1
      IF ( FL_ALL ) THEN
           WRITE ( UNIT=STR(36:40), FMT='(I5)' ) MIN( NS, L_ENV )
           WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ENV 
         ELSE 
           WRITE ( UNIT=STR(36:40), FMT='(I5)' ) MIN( NS, L_ALT )
           WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ALT
      END IF
      CALL SETCR_MN ( 0,  0 ) 
      CALL ADDSTR_F ( STR(1:80) )
      CALL SETCR_MN ( 0,  1 ) 
      CALL ADDSTR_F ( '----------------------------------------'// &
     &                '----------------------------------------' )
      CALL SETCR_MN ( 0, 22 ) 
      CALL ADDSTR_F ( '----------------------------------------'// &
     &                '----------------------------------------' )
      CALL SETCR_MN ( 0, 23 ) 
      CALL ADDSTR_F ( 'Select database using cursor or (T)ype the '// &
     &                'database name   or hit ESC to quit' )
      NB = 1
      IF ( FL_ALL ) THEN
           LE = L_ENV 
           NE = MIN ( NS, L_ENV )
         ELSE 
           LE = L_ALT
           NE = MIN ( NS, L_ALT )
      END IF
      NC = NE + 2
      IC = NE
 910  CONTINUE 
      IB = 1
      DO 420 J2=NB,NB+NS-1
         IB = IB + 1
         CALL SETCR_MN ( 0, IB ) 
         IF ( J2 .LE. NE ) THEN
              IF ( FL_ALL ) THEN
                   STR = F_ENV(J2)
                 ELSE 
                   STR = F_ALT(J2)
              END IF
            ELSE
              CALL CLRCH ( STR )  
         END IF
         CALL ADDSTR_F ( '  '//STR(1:78) )
 420  CONTINUE 
 920  CONTINUE 
      CALL SETCR_MN ( 0, MIN(NE+1,NC-1) ) 
      KEY = 0
      CALL SENKRS_MN ( IX, IY, KEY )
      IF ( KEY_CH(4:4) == CHAR(13)  .OR.  KEY_CH(4:4) == ' ' ) THEN
           IC = NB + (IY-2)
           IF ( FL_ALL ) THEN
                FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//F_ENV(IC)
              ELSE 
                FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//F_ALT(IC)
           END IF
           GOTO 820
         ELSE IF ( KEY_CH(4:4) == CHAR(18) .AND. LE > NS ) THEN
           NB = NB - NS 
           IF ( NB < 0 ) NB = 1
           NE = NB + NS-1
           CALL CLRCH ( STR )
!
           STR = 'Please select a database         -       (     )'
           WRITE ( UNIT=STR(28:32), FMT='(I5)' ) NB
           IF ( FL_ALL ) THEN
                WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ENV 
              ELSE 
                WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ALT 
           END IF
           CALL SETCR_MN ( 0,  0 ) 
           CALL ADDSTR_F ( STR(1:80) )
           GOTO 910
         ELSE IF ( KEY_CH(4:4) == CHAR(19) .AND. LE > NS ) THEN
           NE = NE + NS 
           IF ( FL_ALL ) THEN
                IF ( NE > L_ENV ) NE = L_ENV
              ELSE 
                IF ( NE > L_ALT ) NE = L_ALT
           END IF
           NB = NE - (NS-1)
!
           STR = 'Please select a database         -       (     )'
           WRITE ( UNIT=STR(28:32), FMT='(I5)' ) NB
           IF ( FL_ALL ) THEN
                WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ENV 
              ELSE 
                WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ALT 
           END IF
           CALL SETCR_MN ( 0,  0 ) 
           CALL ADDSTR_F ( STR(1:80) )
           GOTO 910
         ELSE IF ( KEY_CH(4:4) == '-' ) THEN
           IF ( NC .LE. 3 ) THEN
                IF ( NB > 1 ) THEN
                     NB = NB - 1
                     NE = NE - 1
                     STR = 'Please select a database         -       (     )'
                     WRITE ( UNIT=STR(28:32), FMT='(I5)' ) NB
                     WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                     WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ENV 
                     CALL SETCR_MN ( 0,  0 ) 
                     CALL ADDSTR_F ( STR(1:48) )
                     IC = NB
                     GOTO 910
                END IF
              ELSE
                IC = IC - 1
                NC = NC - 1
                GOTO 920
           END IF
         ELSE IF ( KEY_CH(4:4) == '+' ) THEN
           IF ( NC .GE. NS+2 ) THEN
                IF ( NE < L_ENV ) THEN
                     NB = NB + 1
                     NE = NE + 1
                     STR = 'Please select a database         -       (     )'
                     WRITE ( UNIT=STR(28:32), FMT='(I5)' ) NB
                     WRITE ( UNIT=STR(36:40), FMT='(I5)' ) NE
                     WRITE ( UNIT=STR(43:47), FMT='(I5)' ) L_ENV 
                     CALL SETCR_MN ( 0,  0 ) 
                     CALL ADDSTR_F ( STR(1:48) )
                     IC = NE
                     GOTO 910
                END IF
              ELSE
                IC = IC + 1
                NC = NC + 1
                GOTO 920
           END IF
         ELSE IF ( KEY_CH(4:4) == CHAR(27) ) THEN
           GOTO 820
         ELSE IF ( KEY_CH(4:4) == 'T' ) THEN
           CALL SETCR_MN ( 0, 23 ) 
           CALL CLRCH ( STR ) 
           CALL ADDSTR_F ( STR(1:79) )
           CALL SETCR_MN ( 0, 23 ) 
           CALL ADDSTR_F ( 'Database name >> ' )
           CALL CLRCH ( STR ) 
           CALL GETSTR_MN ( STR )
           CALL CHASHL    ( STR )
           IF ( ILEN(STR) == 0 ) THEN
                GOTO 900
             ELSE IF ( ILEN(STR) < 10 ) THEN
                STR = STR(1:I_LEN(STR))//'*'
           END IF
           IF ( INDEX ( STR, '*' ) > 0 .OR. &
     &          INDEX ( STR, '?' ) > 0 .OR. &
     &          INDEX ( STR, '[' ) > 0 .OR. &
     &          INDEX ( STR, ']' ) > 0      ) THEN
                L_ALT = 0
                DO 430 J3=1,L_ENV
                   IF ( MATCH_WILD ( F_ENV(J3)(1:I_LEN(F_ENV(J3))), &
     &                               STR(1:I_LEN(STR)) ) ) THEN
                        L_ALT = L_ALT + 1
                        F_ALT(L_ALT) = F_ENV(J3)
                   END IF
 430            CONTINUE 
                IF ( L_ALT == 0 ) THEN
                     CALL SETCR_MN ( 0, 23 ) 
                     STR1 = STR
                     CALL CLRCH ( STR )
                     STR = STR1(1:I_LEN(STR1))//' not found! '// &
     &                     'Please try again'
                     CALL ADDSTR_F ( STR(1:79) )
                     CALL SETCR_MN ( 0, 22 ) 
                     GOTO 920
                   ELSE 
                     FL_ALL = .FALSE.
                     CALL CLEAR_MN()
                     GOTO 900
                END IF 
           END IF
           IF ( ILEN(STR) .GE. 19 ) THEN
                FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//STR
                GOTO 820
              ELSE IF ( ILEN(STR) == 16 ) THEN
                FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//STR(1:16)// &
     &                  'env'
                GOTO 820
              ELSE IF ( ILEN(STR) == 15 ) THEN
                FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//STR(1:15)// &
     &                  '.env'
                GOTO 820
              ELSE IF ( ILEN(STR) == 10 ) THEN
                PATTERN = STR(1:10)//'_v*.env'
                CALL CLRCH ( FILENV )
                DO 440 J4=1,L_ENV
                   IF ( MATCH_WILD ( F_ENV(J4)(1:I_LEN(F_ENV(J4))), &
     &                               PATTERN(1:I_LEN(PATTERN)) ) ) THEN
                        FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))// &
     &                           '/'//F_ENV(J4)
                   END IF
 440            CONTINUE 
                IF ( ILEN(FILENV) > 0 ) THEN
                     GOTO 820
                   ELSE 
                     CALL SETCR_MN ( 0, 22 ) 
                     STR1 = STR
                     CALL CLRCH ( STR )
                     STR = STR1(1:I_LEN(STR1))//' not found! '// &
     &                     'Please try again'
                     CALL ADDSTR_F ( STR(1:79) )
                     CALL SETCR_MN ( 0, 22 ) 
                     GOTO 920
                END IF 
           END IF
      END IF
      GOTO 920
 820  CONTINUE 
      CALL END_MN()
!
      DEALLOCATE ( F_ENV )
      DEALLOCATE ( F_ALT )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_SELECT  !#!#
