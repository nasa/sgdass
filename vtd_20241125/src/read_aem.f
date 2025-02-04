      SUBROUTINE READ_AEM ( FILE_AEM, AEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_AEM 
! *                                                                      *
! *  ### 31-OCT-2006    READ_AEM   v1.1 (c)  L. Petrov  08-AUG-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'aem.i'
      TYPE       ( AEM__TYPE ) :: AEM
      CHARACTER  FILE_AEM*(*)
      INTEGER*4  IUER
      LOGICAL*4  LEX
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 16*1024, MIND = 32 )
      CHARACTER  STR*128, REG*6
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(5)//'():' )
      INTEGER*4  J1, J2, J3, NBUF, LIND, IND(2,MIND), IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=FILE_AEM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2611, IUER, 'READ_AEM', 'Cannot find '// &
     &         'a priori Earth rotation model file '//FILE_AEM )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 128*MBUF, STR )
           CALL ERR_LOG ( 2612, IUER, 'READ_AEM', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILE_AEM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2613, IUER, 'READ_AEM', 'Error in an attempt '// &
     &         'to read an a priori Earth rotation model file '//FILE_AEM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1) .NE. AEM__LABEL ) THEN
           CALL ERR_LOG ( 2614, IUER, 'READ_AEM', 'Wrong format of the '// &
     &         'a priori Earth rotation model file '// &
     &          FILE_AEM(1:I_LEN(FILE_AEM))//' -- first line is '// &
     &          BUF(1)(1:I_LEN(BUF(1)))//' while the format label "'// &
     &          AEM__LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 2615, IUER, 'READ_AEM', 'Cannot find the trailer '// &
     &         'label of the a priori Earth rotation model file '//FILE_AEM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT( SIZEOF(AEM), AEM ) 
!
      DO 410 J1=2,NBUF-1
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -3 )
         IF ( LIND < 2 ) GOTO 410
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MODEL_NAME' ) THEN
              AEM%MODEL_NAME = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MODEL_DATE' ) THEN
              AEM%MODEL_DATE = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_COM' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), AEM%N_COM )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_PRC' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), AEM%N_PRC )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_NUT' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), AEM%N_NUT )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_E3P' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), AEM%N_E3P )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_E3H' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), AEM%N_E3H )
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MODEL_COMMENT' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_COM ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2616, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              AEM%MODEL_COMMENT(IP) = BUF(J1)(IND(1,3):IND(2,LIND)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DZETA' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_PRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2617, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%DZETA(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2618, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TETA' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_PRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2619, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%TETA(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2620, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Z' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_PRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2621, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%Z(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2622, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EPS0' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_PRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2623, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%EPS0(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2624, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CHI' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_PRC ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2625, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%CHI(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2626, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_PHS' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2627, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_PHS(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2628, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_FRQ' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2629, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_FRQ(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2630, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_ACC' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2631, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_ACC(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2632, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_PSI_IN' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2633, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_PSI_IN(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2634, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_EPS_IN' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2635, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_EPS_IN(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2636, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_PSI_OUT' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2637, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_PSI_OUT(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2638, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_EPS_OUT' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2639, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_EPS_OUT(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2640, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_PSI_IN_RATE' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2641, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_PSI_IN_RATE(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2642, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_EPS_IN_RATE' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2643, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_EPS_IN_RATE(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2644, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_PSI_OUT_RATE' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2645, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_PSI_OUT_RATE(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2646, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUT_EPS_OUT_RATE' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 1  .OR.  IP > AEM%N_NUT ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2647, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%NUT_EPS_OUT_RATE(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2648, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'S0' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT=*, IOSTAT=IER ) &
     &               AEM%S0
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2649, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OMEGA_N' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT=*, IOSTAT=IER ) &
     &               AEM%OMEGA_N
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2650, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'E3_POL' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_E3P ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2651, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%E3_POL(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2652, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'E3_FRQ' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_E3H ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2653, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%E3_FRQ(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2654, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'E3_COS' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_E3H ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2655, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%E3_COS(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2656, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'E3_SIN' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IP )
              IF ( IP .LT. 0  .OR.  IP > AEM%N_E3H ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2657, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding '//STR(1:I_LEN(STR))//'-th line of '// &
     &                 'the a priori Earth rotation model file '// &
     &                  FILE_AEM(1:I_LEN(FILE_AEM))// &
     &                 ' -- index is out of range' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) &
     &               AEM%E3_SIN(IP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2658, IUER, 'READ_AEM', 'Error in '// &
     &                 'decoding REAL*8 value at '//STR(1:I_LEN(STR))// &
     &                 '-th line of the a priori Earth rotation '// &
     &                 'model file '//FILE_AEM )
                   RETURN
              END IF
            ELSE 
              CALL CLRCH  ( STR )
              CALL INCH   ( J1, STR )
              CALL ERR_LOG ( 2659, IUER, 'READ_AEM', 'Uknown keyword '// &
     &             BUF(J1)(IND(1,1):IND(2,1))//' at line '// &
     &             STR(1:I_LEN(STR))//' of the a priori Earth rotation '// &
     &            'model file '//FILE_AEM )
              RETURN
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_AEM  !#!#
