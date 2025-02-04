      SUBROUTINE HARPOS_KEEP_S1_ONLY ( FILHAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HARPOS_KEEP_S1_ONLY
! *                                                                      *
! * ## 06-JUN-2017 HARPOS_KEEP_S1_ONLY v1.0 (c) L. Petrov 06-JUN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  FILHAR*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128, OUT(:)*128
      CHARACTER  STR*128
      INTEGER*4  IUER
      INTEGER*8  SIZE_I8
      INTEGER*4  J1, J2, J3, J4, IS, UNIX_DATE, M_BUF, L_BUF, NOUT, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN
!
      IS = FILE_INFO ( TRIM(FILHAR)//CHAR(0), UNIX_DATE, SIZE_I8 )
      M_BUF = SIZE_I8/32
      ALLOCATE ( BUF(M_BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_BUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4511, IUER, 'HARPOS_KEEP_S1_ONLY', 'Failure in '// &
     &         'an attempt to allocate '//TRIM(STR)//' bytes for array BUF' )
           RETURN 
      END IF
      ALLOCATE ( OUT(M_BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_BUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4512, IUER, 'HARPOS_KEEP_S1_ONLY', 'Failure in '// &
     &         'an attempt to allocate '//TRIM(STR)//' bytes for array OUT' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILHAR, M_BUF, BUF, L_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_BUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4513, IUER, 'HARPOS_KEEP_S1_ONLY', 'Error in '// &
     &         'reading input harpos file '//FILHAR )
           RETURN 
      END IF
!
      NOUT = 0
      DO 410 J1=1,L_BUF
         IF ( BUF(J1)(1:2) == 'H ' ) THEN
              IF ( BUF(J1)(4:6) .NE. 'S1 ' ) GOTO 410
           ELSE IF ( BUF(J1)(1:2) == 'D ' ) THEN
              IF ( BUF(J1)(4:6) .NE. 'S1 ' ) GOTO 410
         END IF
         NOUT = NOUT + 1
         OUT(NOUT) = BUF(J1)
         IF ( J1 < L_BUF - 8 ) THEN
              IF ( BUF(J1+3)(1:80) == '#============================ End of comments: =================================' ) THEN
                   NOUT = NOUT + 1
                   CALL CLRCH ( OUT(NOUT) )
                   NOUT = NOUT + 1
                   OUT(NOUT)= '#    Only terms of S1 harmonic (diurnal position variations) retained' 
              END IF
         END IF
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NOUT, OUT, FILHAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4514, IUER, 'HARPOS_KEEP_S1_ONLY', 'Error in '// &
     &         'writing into the output harpos file '//FILHAR )
           RETURN 
      END IF
      DEALLOCATE ( BUF )
      DEALLOCATE ( OUT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HARPOS_KEEP_S1_ONLY  !#!#
