      SUBROUTINE PIMA_GET_EVN_GAIN ( PIM, EVN_GAIN_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_EVN_GAIN parses gains in EVN format and inserts   *
! *   them into appropriate slots of PIMA internal data structures.      *
! *                                                                      *
! * ### 09-APR-2012 PIMA_GET_EVN_GAIN v1.1 (c) L. Petrov 25-DEC-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER  EVN_GAIN_FILE*(*)
      INTEGER*4  MP, MIND
      PARAMETER  ( MP = 1024 )
      PARAMETER  ( MIND = 128 )
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      LOGICAL*1  LEX
      CHARACTER  REG*5, STA_CODE*16, STR*128
      PARAMETER  ( REG = CHAR(9)//CHAR(0)//CHAR(32)//'='//',' )
      INTEGER*4  J1, J2, J3, J4, J5, J6, NP, LIND, IND(2,MIND), &
     &           POLY_TYP, IND_STA, NPOLY, IER
      REAL*8     DPFU(2), GAIN_POLY(0:PIM__MGPL), FREQS(2)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=EVN_GAIN_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6711, IUER, 'PIMA_GET_EVN_GAIN', 'Cannot '// &
     &         'find gain file '//EVN_GAIN_FILE )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MP) )
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( EVN_GAIN_FILE, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6712, IUER, 'PIMA_GET_EVN_GAIN', 'Failure in '// &
     &         'an attempt to read the gain file '//EVN_GAIN_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 410 J1=1,NP-1
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN' ) THEN
              IF ( LIND < 9 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6713, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Line '//STR(1:I_LEN(STR))//' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' has too few words, while at least 9 words '// &
     &                 ' were expected' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              STA_CODE = BUF(J1)(IND(1,2):IND(2,2)) 
              IND_STA = 0
              DO 420 J2=1,PIM%NSTA
                 IF ( PIM%STA(J2)%ORIG_NAME(1:ILEN(STA_CODE)) == &
     &                STA_CODE(1:I_LEN(STA_CODE)) ) THEN
                      IND_STA = J2
                 END IF
 420          CONTINUE 
              IF ( IND_STA == 0 ) GOTO 410
!
              POLY_TYP = 0 
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'ELEV' .OR. &
     &             BUF(J1)(IND(1,3):IND(2,3)) == 'ALTAZ'     ) THEN
                   POLY_TYP = 1
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'ZEN' ) THEN
                   POLY_TYP = 2
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6714, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'unrecoginzed gain curve type in parsing '// &
     &                 'line '//STR(1:I_LEN(STR))//' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' -- '//BUF(J1)(IND(1,3):IND(2,3))// &
     &                 ' while ELEV or ZEN were expected' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F14.5)', IOSTAT=IER ) DPFU(1)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6715, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' cannot decode  DPFU(RR) field '// &
     &                 BUF(J1)(IND(1,5):IND(2,5)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F14.5)', IOSTAT=IER ) DPFU(2)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6716, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' cannot decode  DPFU(LL) field '// &
     &                 BUF(J1)(IND(1,6):IND(2,6)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              STR = BUF(J1)(IND(1,8):IND(2,8))
              IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.0'
              READ ( UNIT=STR, FMT='(F8.5)', IOSTAT=IER ) FREQS(1)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6717, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' cannot decode  Freq_min field '// &
     &                 BUF(J1)(IND(1,8):IND(2,8)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              FREQS(1) = FREQS(1)*1.0D6
!
              STR = BUF(J1)(IND(1,9):IND(2,9))
              IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.0'
              READ ( UNIT=STR, FMT='(F8.5)', IOSTAT=IER ) FREQS(2)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6718, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' cannot decode  Freq_max field '// &
     &                 BUF(J1)(IND(1,9):IND(2,9)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              FREQS(2) = FREQS(2)*1.0D6
!
              CALL EXWORD ( BUF(J1+1), MIND, LIND, IND, REG, IER )
              IF ( BUF(J1+1)(IND(1,1):IND(2,1)) .NE. 'POLY' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1+1, STR )
                   CALL ERR_LOG ( 6719, IUER, 'PIMA_GET_EVN_GAIN', &
     &                 'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of gain file '// &
     &                 EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                 ' -- the first word POLY was expected, but '// &
     &                 ' got '//BUF(J1+1)(IND(1,1):IND(2,1)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              NPOLY = LIND - 2
              IF ( BUF(J1+1)(IND(1,LIND):IND(2,LIND)) == '/' ) NPOLY = NPOLY - 1
!
              DO 430 J3=2,LIND
                 IF ( BUF(J1+1)(IND(1,J3):IND(2,J3)) == '/' ) GOTO 430
                 READ ( UNIT=BUF(J1+1)(IND(1,J3):IND(2,J3)), FMT='(F14.5)', IOSTAT=IER ) GAIN_POLY(J3-2)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1+1, STR )
                      CALL ERR_LOG ( 6720, IUER, 'PIMA_GET_EVN_GAIN', &
     &                    'Error in parsing line '//STR(1:I_LEN(STR))// &
     &                    ' of gain file '// &
     &                    EVN_GAIN_FILE(1:I_LEN(EVN_GAIN_FILE))// &
     &                    ' cannot decode  polynomial coeeficient '// &
     &                    BUF(J1+1)(IND(1,J3):IND(2,J3)) )
                      DEALLOCATE ( BUF )
                      RETURN 
                 END IF
 430          CONTINUE 
!
! ----------- Now insert gain
!
              IF ( .NOT. PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
                   IF ( ASSOCIATED ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP ) ) THEN
!
! --------------------- Deallocate memory, if it has already been allocated
!
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP   )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN  )
                        DEALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS  )
                   END IF
!
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL = .TRUE.
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ  = PIM%NFRQ
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL  = PIM%NPOL
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB  = NPOLY
!
! ---------------- and allocate memory
!
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP  (PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL(0:PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
          &                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
          &                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
                   ALLOCATE ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL) )
!
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM = 0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB  = 0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP   = 0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL = 0.0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL = 0.0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN  = 0.0
                   PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS  = 0.0
              END IF
!
! ----------- Set DPFU and GAIN
!
              DO 440 J4=1,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NFRQ
                 IF ( PIM%FREQ_ARR(1,J4,PIM%CONF%FRQ_GRP) .GE. FREQS(1) .AND. &
     &                PIM%FREQ_ARR(1,J4,PIM%CONF%FRQ_GRP) .LE. FREQS(2) ) THEN
!
                      DO 450 J5=1,PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J4,J5) = DPFU(J5)
!
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP(J4,J5) = PIMA__GA_ELEV
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_TYP(J4,J5) = 1
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_TYP(J4,J5) = 1
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%X_VAL(J4,J5) = 0.0D0
!
! ---------------------- The caveat: indexing of arrays GAIN%Y_VAL and GAIN%GAIN
! ---------------------- starts from 1
!
                         DO 460 J6=0,NPOLY
                            PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J6,J4,J5)  = GAIN_POLY(J6)
                            PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%Y_VAL(J6,J4,J5) = GAIN_POLY(J6)
 460                     CONTINUE
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J4,J5) = NPOLY
                         PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB         = NPOLY
 450                  CONTINUE
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           IF ( PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NPOL == 1 ) THEN
                                WRITE ( 6, 110 ) PIM%C_STA(IND_STA), &
     &                                      PIM%FREQ_ARR(1,J4,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP(J4,1), &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J4,1), DPFU(2), &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:NPOLY,J4,1) 
                             ELSE 
                                WRITE ( 6, 110 ) PIM%C_STA(IND_STA), &
     &                                      PIM%FREQ_ARR(1,J4,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%TYP(J4,1), &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%NTAB, &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J4,1:2) , &
     &                                      PIM%STA(IND_STA)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(0:NPOLY,J4,1) 
                           END IF
 110                       FORMAT ( 'PIMA_GET_EVN_GAIN Sta: ', A, &
     &                              ' Freq: ', F9.2, ' MHz ', &
     &                              ' Typ: ', I1, ' Npoly: ', I1, &
     &                              ' Sens: ', 2(F7.5,1X), &
     &                              ' Poly: ', 6(F11.8,1X) )
                      END IF
                 END IF
 440         CONTINUE
             IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                  WRITE ( 6, '(A)' ) ' '
             END IF
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_EVN_GAIN  !#!#
