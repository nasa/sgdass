      SUBROUTINE PIMA_UV_EXCLUDE ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_UV_EXCLUDE 
! *                                                                      *
! * ## 07-MAY-2006  PIMA_UV_EXCLUDE   v1.0 (c) L. Petrov 07-MAY-2006 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IUER
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*8  SIZE_I8, SIZE__EXTRA
      PARAMETER  ( SIZE__EXTRA = 8192 )
      CHARACTER  STR*128, STR1*32, FINAM*128
      INTEGER*1, ALLOCATABLE :: BUF(:)
      LOGICAL*4  LEX
      INTEGER*4  UNIX_DATE, IS, J1, J2, LUN, IB, IL, LN, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, READ
!
      PIM%NUV_EXC = 0
      IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_NO ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_AUTO ) THEN
           FINAM = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &             PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_uv.exc'
!
! -------- In AUTO mode we check whether the input file exists
!
           INQUIRE ( FILE=FINAM, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
!
! ------------- Nothing to do
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
         ELSE 
           FINAM = PIM%CONF%EXCLUDE_UV_FINAM
      END IF
!
! --- Get information about file, in partucular its size in bytes
!
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7621, IUER, 'PIMA_UV_EXCLUDE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in attempt to inquire UV exclusion '// &
     &              'file '//FINAM )
                RETURN 
           END IF
      END IF
!
      ALLOCATE ( BUF(SIZE_I8+SIZE__EXTRA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( SIZE_I8+SIZE__EXTRA, STR )
           CALL ERR_LOG ( 7622, IUER, 'PIMA_UV_EXCLUDE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for the buffer '// &
     &          'for the UV exclusion file '//FINAM )
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7623, IUER, 'PIMA_UV_EXCLUDE', 'Failre to open input '// &
     &         'UV exclusion file '//FINAM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! ---- Read contents of the entire file into BUF
!
      IS = READ ( %VAL(LUN), %VAL(LOC(BUF)), %VAL(SIZE_I8) )
      IER = -1
      CALL BINF_CLOSE ( LUN, IER )
!
      IB = 1
      DO 410 J1=1,SIZE_I8 
         IF ( BUF(J1) == 10 ) THEN
              IF ( J1 - IB > 0 ) THEN
                   IF ( BUF(IB) == ICHAR('#') .OR. &
     &                  BUF(IB) == ICHAR('!') .OR. &
     &                  BUF(IB) == ICHAR('*')      ) THEN
                      ELSE
                        PIM%NUV_EXC = PIM%NUV_EXC + 1
                   END IF
              END IF
              IB = J1+1
         END IF
 410  CONTINUE 
!
      ALLOCATE ( PIM%UV_EXC(PIM%NUV_EXC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NUV_EXC, STR )
           CALL ERR_LOG ( 7624, IUER, 'PIMA_UV_EXCLUDE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for the buffer '// &
     &          'for the UV exclusion file '//FINAM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IB = 1
      IL = 0
      PIM%NUV_EXC = 0
      DO 420 J2=1,SIZE_I8 
         IF ( BUF(J2) == 10 ) THEN
              IL = IL + 1
              IF ( J2 - IB > 0 ) THEN
                   IF ( BUF(IB) == ICHAR('#') .OR. &
     &                  BUF(IB) == ICHAR('!') .OR. &
     &                  BUF(IB) == ICHAR('*')      ) THEN
                      ELSE
                        PIM%NUV_EXC = PIM%NUV_EXC + 1
                        LN = J2-IB
                        CALL CLRCH ( STR ) 
                        CALL LIB$MOVC3 ( MIN(LEN(STR),LN), BUF(IB), STR )
                        CALL CHASHL ( STR )
                        READ ( UNIT=STR(1:I_LEN(STR)), FMT='(I11)', &
     &                         IOSTAT=IER ) PIM%UV_EXC(PIM%NUV_EXC)
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR1 )
                             CALL INCH  ( IL, STR1 )
                             CALL ERR_LOG ( 7625, IUER, 'PIMA_UV_EXCLUDE', &
     &                           'Failure in decoding the '//STR1(1:I_LEN(STR1))// &
     &                           'th line "'//STR(1:I_LEN(STR))// &
     &                           '" of the UV exclusion file '//FINAM )
                             PIM%NUV_EXC = 0
                             DEALLOCATE ( PIM%UV_EXC )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
                   END IF
              END IF
              IB = J2+1
         END IF
 420  CONTINUE 
      DEALLOCATE ( BUF )
!
      IF ( PIM%NUV_EXC > 0 ) THEN
           CALL SORT_FAST_I4 ( PIM%NUV_EXC, PIM%UV_EXC )
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                WRITE ( 6, 110 ) PIM%NUV_EXC, FINAM(1:I_LEN(FINAM))
 110            FORMAT ( 'PIMA_UV_EXLUDE: ',I8, ' UV points were excluded ', &
     &                   'according to file ', A )
           END IF
      END IF
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_UV_EXCLUDE !#!#
