      SUBROUTINE GET_SOURCE_TABLE ( MASTER_DIR, M_SRC, L_SRC, IVS_NAME, &
     &           B1950_NAME, ICRF_NAME, J2000_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_SOURCE_TABLE reads the source table and returns arrays *
! *   of IVS, IERS, ICRF and IAU J2000.0 names of all known geodetic     *
! *   VLBI sources. The current version of this routine looks for the    *
! *   file with source names in $SAVE_DIR directory.                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * MASTER_DIR ( CHARACTER ) -- Directory where the local copy of        *
! *                             VLBI master schedule file and networking *
! *                             station file are located. In current     *
! *                             version it should be SAVE_DIR !!!        *
! *      M_SRC ( INTEGER*4 ) -- Maximal number of sources in the source  *
! *                             name file. (4096 was enough on           *
! *                             2002.09.30).                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      L_SRC ( INTEGER*4 ) -- The number of sources in the source      *
! *                             file.                                    *
! *   IVS_NAME ( CHARACTER ) -- Array of  8-characters long IVS source   *
! *                             names. Dimension: L_SRC.                 *
! *  B1950_NAME ( CHARACTER ) -- Array of  8-characters long IERS source  *
! *                             names. Dimension: L_SRC.                 *
! *  ICRF_NAME ( CHARACTER ) -- Array of 16-characters long ICRF source  *
! *                             designators. Dimension: L_SRC.           *
! * J2000_NAME ( CHARACTER ) -- Array of 10-characters long IAU source   *
! *                             names in J2000.0 format.                 *
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
! * ### 30-SEP-2002 GET_SOURCE_TABLE v3.0 (c)  L. Petrov 05-OCT-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  M_SRC, L_SRC, IUER
      CHARACTER  MASTER_DIR*(*), IVS_NAME(M_SRC)*(*), B1950_NAME(M_SRC)*(*), &
     &           ICRF_NAME(M_SRC)*(*), J2000_NAME(M_SRC)*(*)
      INTEGER*4  M_BUF
      PARAMETER  ( M_BUF = 2*MAX_SRC )
      CHARACTER  FINAM*128, STR*128
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  FORMAT_LABEL1*32, FORMAT_LABEL2*32, FORMAT_LABEL3*32
      PARAMETER  ( FORMAT_LABEL1 = '# SOURCE-NAMES  v 0.1 2001.04.17' )
      PARAMETER  ( FORMAT_LABEL2 = '# SOURCE-NAMES  v 1.0 2004.04.08' )
      PARAMETER  ( FORMAT_LABEL3 = '# SOURCE-NAMES  v 2.0 2005.09.06' )
      LOGICAL*4  LEX
      INTEGER*4  N_BUF, J1, I_SRC, IFMT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST
!
      IF ( M_SRC .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_SRC, STR )
           CALL ERR_LOG ( 2821, IUER, 'GET_SOURCE_TABLE', 'Parameter M_SRC '// &
     &         'is too small: '//STR )
           RETURN
      END IF
!
! --- Build the name of the source file
!
      FINAM = MASTER_DIR(1:I_LEN(MASTER_DIR))//NAMSRC_FILE
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX) THEN
           CALL ERR_LOG ( 2822, IUER, 'GET_SOURCE_TABLE', 'File with source '// &
     &                   'names '//FINAM(1:I_LEN(FINAM))//' was not found. '// &
     &                   'You should get it from '// &
     &                'http://gemini.gsfc.nasa.gov/solve_save/source.names '// &
     &                'and put in '//MASTER_DIR(1:I_LEN(MASTER_DIR))// &
     &                'directory' )
           RETURN
      END IF
!
      ALLOCATE ( BUF(M_BUF), STAT=IER )
      IF ( .NOT. LEX) THEN
           CALL ERR_LOG ( 2823, IUER, 'GET_SOURCE_TABLE', 'Error in an '// &
     &                   'attempt to allocate memory for a temporary '// &
     &                   'buffer for the file with source names '//FINAM )
           RETURN
      END IF
!
! --- Read the networking source file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, M_BUF, BUF, N_BUF, IER )
      IF ( .NOT. LEX) THEN
           CALL ERR_LOG ( 2824, IUER, 'GET_SOURCE_TABLE', 'Error in an '// &
     &                   'attempt to read the file with source names '//FINAM )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Check the file label
!
      IFMT = 0 
      IF ( BUF(1) .EQ. FORMAT_LABEL1 ) THEN
           IFMT = 1
         ELSE IF ( BUF(1) .EQ. FORMAT_LABEL2 ) THEN
           IFMT = 2
         ELSE IF ( BUF(1) .EQ. FORMAT_LABEL3 ) THEN
           IFMT = 3
         ELSE 
           CALL ERR_LOG ( 2825, IUER, 'GET_SOURCE_TABLE', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' was not recognized as a valid '// &
     &         'source names file: it has a first line >'// &
     &          BUF(1)(1:I_LEN(BUF(1)))//'< while a format label was expected' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Parse the file and extract approriate fields
!
      L_SRC = 0
      DO 410 J1=1,N_BUF
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
!
         IF ( L_SRC .EQ. M_SRC ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_SRC, STR )
              CALL ERR_LOG ( 2826, IUER, 'GET_SOURCE_TABLE', 'Parameter '// &
     &            'M_SRC turned out to be too small: '//STR )
           DEALLOCATE ( BUF )
              RETURN
         END IF
!
!
         IF ( IFMT .EQ. 1 ) THEN
              I_SRC = ADD_CLIST ( M_SRC, L_SRC, IVS_NAME, BUF(J1)(1:8), -2 )
              J2000_NAME(I_SRC) = BUF(J1)(19:28)
              B1950_NAME(I_SRC) = BUF(J1)(10:17)
              ICRF_NAME(I_SRC)  = BUF(J1)(30:45)
            ELSE IF ( IFMT .EQ. 2 ) THEN
              I_SRC = ADD_CLIST ( M_SRC, L_SRC, IVS_NAME, BUF(J1)(1:8), -2 )
              J2000_NAME(I_SRC) = BUF(J1)(19:28)
              B1950_NAME(I_SRC)  = BUF(J1)(10:17)
!
! ----------- 2004.04.09 format does not have IERS designator. 
! ----------- We create it on the fly.
!
              ICRF_NAME(I_SRC)  = 'J'//BUF(J1)(31:32)//BUF(J1)(34:35)// &
     &                                 BUF(J1)(37:40)//BUF(J1)(46:48)// &
     &                                 BUF(J1)(50:51)//BUF(J1)(53:54)
              IF ( ICRF_NAME(I_SRC)(10:10) .EQ. ' ' ) THEN
                   ICRF_NAME(I_SRC)(10:10) = '+'
              END IF
            ELSE IF ( IFMT .EQ. 3 ) THEN
!
! ----------- 2005.09.06 format does not have IERS designator. 
! ----------- We create it on the fly.
!
              I_SRC = ADD_CLIST ( M_SRC, L_SRC, IVS_NAME, BUF(J1)(1:8), -2 )
              J2000_NAME(I_SRC) = BUF(J1)(11:20)
              B1950_NAME(I_SRC) = BUF(J1)(23:30)
              ICRF_NAME(I_SRC)  = 'J'// &
     &                  BUF(J1)(46:47)//BUF(J1)(49:50)//BUF(J1)(52:55)// &
     &                  BUF(J1)(60:62)//BUF(J1)(64:65)//BUF(J1)(67:70)
         END IF
 410  CONTINUE
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_SOURCE_TABLE  !#!#
