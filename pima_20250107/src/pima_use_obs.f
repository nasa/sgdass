      SUBROUTINE PIMA_USE_OBS ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_USE_OBS
! *                                                                      *
! *  ### 03-AUG-2006  PIMA_USE_OBS  v1.0 (c)  L. Petrov 03-AUG-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      LOGICAL*1  INC_OBS(PIM__MOBS), EXC_OBS(PIM__MOBS)
      CHARACTER  FILNAM(2)*128, FILTYP(2)*8
      INTEGER*4    MBUF, LB, MIND
      PARAMETER  ( MBUF = PIM__MOBS + 256 )
      PARAMETER  ( LB   = 32 )
      PARAMETER  ( MIND = 32 )
      CHARACTER*(LB), ALLOCATABLE :: BUF(:)
      CHARACTER  STR*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  J1, J2, J3, J4, J5, NBUF, LIND, IND(2,MIND), &
     &           OBS_MIN, OBS_MAX, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- Initialization
!
      DO 410 J1=1,PIM%NOBS
         IF ( PIM%CONF%INCLUDE_OBS_FILE == 'NO' ) THEN
!
! ----------- Special case: if no include file is specified, then all
! ----------- observations are to be included
!
              INC_OBS(J1) = .TRUE.
            ELSE
              INC_OBS(J1) = .FALSE.
         END IF
         EXC_OBS(J1) = .FALSE.
 410  CONTINUE 
!
      FILNAM(1) = PIM%CONF%INCLUDE_OBS_FILE 
      FILTYP(1) = 'INCLUDE '
!
      FILNAM(2) = PIM%CONF%EXCLUDE_OBS_FILE 
      FILTYP(2) = 'EXCLUDE '
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LB, BUF )
           CALL ERR_LOG ( 7681, IUER, 'PIMA_USE_OBS', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for temporary '// &
     &         'buffers' )
           RETURN 
      END IF
!
      DO 420 J2=1,2
         IF ( FILNAM(J2) .EQ. 'NO' ) THEN
              GOTO 420
            ELSE
              CALL ERR_PASS ( IUER, IER )
              CALL RD_TEXT  ( FILNAM(J2), MBUF, BUF, NBUF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7682, IUER, 'PIMA_USE_OBS', 'Error '// &
     &                 'in an attempt to read '//FILTYP(J2)// &
     &                 ' observations file '//FILNAM(J2) )
                   RETURN 
              END IF
         END IF
!
         DO 430 J3=1,NBUF
            IF ( BUF(J3)(1:1)  == '#' ) GOTO 430
            IF ( BUF(J3)(1:1)  == '*' ) GOTO 430
            IF ( ILEN(BUF(J3)) ==  0  ) GOTO 430
!
            CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, IER )
            IF ( LIND .LE. 1 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J3, STR )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7683, IUER, 'PIMA_USE_OBS', 'Error '// &
     &                    'in processing line '//STR(1:I_LEN(STR))// &
     &                    ' of the '//FILTYP(J2)//' observations file '// &
     &                     FILNAM(J2)(1:I_LEN(FILNAM(J2)))//' "'// &
     &                     BUF(J3)(1:I_LEN(BUF(J3)))//'" the number of '// &
     &                    'words should be two' )
                   RETURN 
                END IF
            END IF
!
            CALL CHIN ( BUF(J3)(IND(1,1):IND(2,1)), OBS_MIN )
            IF ( OBS_MIN .LE. 0  .OR. OBS_MIN > 1000000000 ) THEN
                 CALL ERR_LOG ( 7684, IUER, 'PIMA_USE_OBS', 'Error '// &
     &               'in processing line '//STR(1:I_LEN(STR))// &
     &               ' of the '//FILTYP(J2)//' observations file '// &
     &               FILNAM(J2)(1:I_LEN(FILNAM(J2)))//' "'// &
     &               BUF(J3)(1:I_LEN(BUF(J3)))//'" the first word should '// &
     &              'be a positive integer' )
                 RETURN 
            END IF
!
            IF ( LIND > 1 ) THEN
                 CALL CHIN ( BUF(J3)(IND(1,2):IND(2,2)), OBS_MAX )
                 IF ( BUF(J3)(IND(1,2):IND(1,2)) .EQ. '#' ) THEN
                      OBS_MAX = OBS_MIN
                    ELSE
                      IF ( OBS_MAX .LE. 0  .OR. OBS_MIN > 1000000000 ) THEN
                           CALL ERR_LOG ( 7685, IUER, 'PIMA_USE_OBS', 'Error '// &
     &                         'in processing line '//STR(1:I_LEN(STR))// &
     &                         ' of the '//FILTYP(J2)//' observations file '// &
     &                          FILNAM(J2)(1:I_LEN(FILNAM(J2)))//' "'// &
     &                          BUF(J3)(1:I_LEN(BUF(J3)))//'" the second word '// &
     &                         'should be a positive integer' )
                           RETURN 
                      END IF
                 END IF
               ELSE 
                 OBS_MAX = OBS_MIN
            END IF
!
            IF ( OBS_MAX < OBS_MIN ) THEN
                 CALL ERR_LOG ( 7686, IUER, 'PIMA_USE_OBS', 'Error '// &
     &               'in processing line '//STR(1:I_LEN(STR))// &
     &               ' of the '//FILTYP(J2)//' observations file '// &
     &               FILNAM(J2)(1:I_LEN(FILNAM(J2)))//' "'// &
     &               BUF(J3)(1:I_LEN(BUF(J3)))//'" the second word should '// &
     &              'be equal or greater than the first word' )
                 RETURN 
            END IF
!
            DO 440 J4=1,PIM%NOBS
               IF ( J2 == 1 ) THEN
                    IF ( J4 .GE. OBS_MIN  .AND. J4 .LE. OBS_MAX ) THEN
                         INC_OBS(J4) = .TRUE.
                    END IF
                  ELSE IF ( J2 == 2 ) THEN
                    IF ( J4 .GE. OBS_MIN  .AND. J4 .LE. OBS_MAX ) THEN
                         EXC_OBS(J4) = .TRUE.
                    END IF
               END IF
 440        CONTINUE 
 430     CONTINUE 
 420  CONTINUE 
!
      DO 450 J5=1,PIM%NOBS
         PIM%USE_OBS(J5) = .FALSE.
         IF ( INC_OBS(J5) ) PIM%USE_OBS(J5) = .TRUE.
         IF ( EXC_OBS(J5) ) PIM%USE_OBS(J5) = .FALSE.
 450  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_USE_OBS !#!#
