      SUBROUTINE GAXOF ( GAXOF_FINAM, L_STA, C_STA, AXOF, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine GAXOF  reads and prases the file with antenna global       *
! *   offsets GAXOF_FINAM. It extracts the list of station names and     *
! *   new value of the antenna axis offsets.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * GAXOF_FINAM ( CHARACTER ) -- Name of the antenna axis offsets file.  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       L_STA ( INTEGER*4 ) -- The total number of stations for        *
! *                              which antenna axis offset information   *
! *                              is provided                             *
! *       C_STA ( CHARACTER ) -- Array of IVS station names for which    *
! *                              antenna axis offset information is      *
! *                              provided. Dimension: MAX_STA (solve.i). *
! *                              Only L_STA elements are defined.        *
! *        ASOF ( REAL*8    ) -- Array of atenna axis offsets for the    *
! *                              stations in C_STA. Units: meters.       *
! *                              Dimension: MAX_STA (solve.i). Only      *
! *                              L_STA elements are defined.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 12-OCT-2004     GAXOF     v2.0 (c)  L. Petrov  24-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      CHARACTER  GAXOF_FINAM*(*), C_STA(MAX_STA)*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      REAL*8     AXOF(MAX_STA)
      INTEGER*4  L_STA, IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 4096, MIND = 64 )
      CHARACTER    GAXOF__LABEL*28, GAXOF__LABEL2*46, REG*3, STR*256
      PARAMETER  ( GAXOF__LABEL  = '# AXOF Format of 2004.10.12 ' )
      PARAMETER  ( GAXOF__LABEL2 = 'VLBI_AXIS_OFFSET  Format version of 2004.05.18' )
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32) )
      REAL*8     AXOF__MIN, AXOF__MAX
      PARAMETER  ( AXOF__MIN = -200.0D0, AXOF__MAX = 15000.0D0 ) 
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, LIND, IND(2,MIND), NBUF, IER
      EXTERNAL   I_LEN
      INTEGER*4  I_LEN
!
! --- Check whether the global axis offsets file exists
!
      L_STA = 0
      INQUIRE ( FILE=GAXOF_FINAM, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4811, IUER, 'GAXOF', 'Mapping file for antenna '// &
     &         'axis offsets '//GAXOF_FINAM(1:I_LEN(GAXOF_FINAM))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Allocate memory for the buffer
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4812, IUER, 'GAXOF', 'Failure to allocate '// &
     &         'a buffer for axis offsets' )
           RETURN 
      END IF
!
! --- Read the file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( GAXOF_FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4813, IUER, 'GAXOF', 'Error in an attempt to '// &
     &         'read the file with axis offset '//GAXOF_FINAM )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the fitst line -- it should contain a label in post 2004 format
!
      IF ( BUF(1)(1:LEN(GAXOF__LABEL)) .EQ. GAXOF__LABEL ) THEN
!
! -------- Yes, it is post-2004 format
!
! -------- Check the trailer
!
           IF ( BUF(NBUF)(1:LEN(GAXOF__LABEL)) .NE. GAXOF__LABEL ) THEN
                CALL ERR_LOG ( 4814, IUER, 'GAXOF', 'Trap of internal '// &
     &              'control: no trailer was found in the mapping file '// &
     &              'with antenna axis offsets '// &
     &              GAXOF_FINAM(1:I_LEN(GAXOF_FINAM))//' -- the file '// &
     &              'either was not read to the end, or it does not '// &
     &              'follow specifications' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Decode the buffer line by line, bypassing comments
!
           DO 410 J1=2,NBUF-1
              IF ( BUF(J1)(1:11) .EQ. 'Axis offset' ) THEN
                   L_STA = L_STA + 1
                   C_STA(L_STA) = BUF(J1)(14:21)
!U@                   CALL UNDSCR ( C_STA(L_STA) )
                   CALL EXWORD ( BUF(J1)(22:), MIND, LIND, IND, REG, IER )
                   READ ( UNIT=BUF(J1)(IND(1,1)+21:IND(2,1)+21), &
     &                    FMT='(F9.2)', IOSTAT=IER ) AXOF(L_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 4815, IUER, 'GAXOF', 'Error in '// &
     &                      'decoding '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'of the mapping global antenna axisf offsets '// &
     &                      'file '//GAXOF_FINAM )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
                   IF ( AXOF(L_STA) .LT. AXOF__MIN  .OR. &
     &                  AXOF(L_STA) .GT. AXOF__MAX       ) THEN
                        WRITE ( STR, 110 ) GAXOF_FINAM, C_STA(L_STA), &
     &                                     AXOF(L_STA), &
     &                                     AXOF__MIN, AXOF__MAX       
 110                    FORMAT ( 'Error in parsing antenna axis offset '// &
     &                           'mapping file: offset for station ',A, &
     &                           ' is ',1PD18.10,' mm which is out of range '// &
     &                           '[',0PF7.1, ', ', 0PF7.1,'] mm' )
                        CALL ERR_LOG ( 4816, IUER, 'GAXOF', STR )
                        RETURN 
                   END IF
              END IF
 410       CONTINUE 
         ELSE IF ( BUF(1)(1:LEN(GAXOF__LABEL2)) .EQ. GAXOF__LABEL2 ) THEN
           DO 420 J2=2,NBUF-1
              IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
              L_STA = L_STA + 1
              C_STA(L_STA) = BUF(J2)(1:8)
!@U              CALL UNDSCR ( C_STA(L_STA) )
              READ ( UNIT=BUF(J2)(22:28), FMT='(F7.4)', IOSTAT=IER ) AXOF(L_STA)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4815, IUER, 'GAXOF', 'Error in '// &
     &                      'decoding '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'of the mapping global antenna axisf offsets '// &
     &                      'file '//GAXOF_FINAM )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              AXOF(L_STA) = AXOF(L_STA)*1.D3
              IF ( AXOF(L_STA) .LT. AXOF__MIN  .OR. &
     &             AXOF(L_STA) .GT. AXOF__MAX       ) THEN
                   WRITE ( STR, 110 ) GAXOF_FINAM, C_STA(L_STA), &
     &                                AXOF(L_STA), &
     &                                AXOF__MIN, AXOF__MAX       
                   CALL ERR_LOG ( 4816, IUER, 'GAXOF', STR )
                   RETURN 
              END IF
 420       CONTINUE 
         ELSE 
!
! -------- This is is pre-2004 format
!
           DO 430 J3=1,NBUF
              IF ( BUF(J3)(1:2) .EQ. '//' ) GOTO 430
              CALL EXWORD ( BUF(J3)(9:), MIND, LIND, IND, REG, IER )
              IF ( LIND .LE. 1 ) GOTO 430
              L_STA = L_STA + 1
              C_STA(L_STA) = BUF(J3)(1:8)
              CALL UNDSCR ( C_STA(L_STA) )
              READ ( UNIT=BUF(J3)(IND(1,1)+8:IND(2,1)+8), FMT='(F16.8)', &
     &               IOSTAT=IER ) AXOF(L_STA)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 4817, IUER, 'GAXOF', 'Error in '// &
     &                 'decoding the '//STR(1:I_LEN(STR))//'-th line '// &
     &                 'of the mapping global antenna axisf offsets '// &
     &                 'file '//GAXOF_FINAM )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
 430       CONTINUE 
      END IF
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GAXOF
