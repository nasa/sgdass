      SUBROUTINE GET_NS_TABLE ( MASTER_DIR, M_STA, L_STA, STA_NAME, STA_CODE, &
     &                          STA_DOME, STA_CDP, STA_DESC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_NS_TABLE reads the networking station table and        *
! *   returns arrays of station names, station two-letter code names,    *
! *   station dome names, station CDP numbere and short test of station  *
! *   description. GET_NS_TABLE assumes that the networking station file *
! *   has already been retrieved in the MASTER_DIR directory. If not, it *
! *   will issue the error message. The networing stations file can be   *
! *   browsed to the MASTER_DIR directory by a subroutine GET_MASTER.    *
! *   Therefore, it is recommended to call this subroutine if you are    *
! *   not sure whether the networking file has been copied to that       *
! *   directory.                                                         *
! *                                                                      *
! *   NB: Blanks in IVS station names are replaced with underscores.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * MASTER_DIR ( CHARACTER ) -- Directory where the local copy of        *
! *                             VLBI master schedule file and networking *
! *                             station file are located.                *
! *      M_STA ( INTEGER*4 ) -- Maximal number of stations in the        *
! *                             networking station file. (256 was enough *
! *                             on 2002.05.03).                          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      L_STA ( INTEGER*4 ) -- The number of stations in the            *
! *                             networking file.                         *
! *   STA_NAME ( CHARACTER ) -- Array of 8-character long IVS station    *
! *                             names. Dimension: L_STA.                 *
! *   STA_CODE ( CHARACTER ) -- Array of 2-character long IVS station    *
! *                             codes. Dimension: L_STA.                 *
! *   STA_DOME ( CHARACTER ) -- Array of 9-character long IERS station   *
! *                             dome names. Dimension: L_STA.            *
! *   STA_CDP  ( INTEGER*4 ) -- Array of integer CDP station codes.      *
! *                             Dimension: L_STA.                        *
! *   STA_DESC ( CHARACTER ) -- Array of a shport station description.   *
! *                             50 character at maximum. Usually         *
! *                             contains full name of the station and    *
! *                             its geographical location. Dimenstion:   *
! *                             L_STA.                                   *
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
! *  ### 03-APR-2002  GET_NS_TABLE  v1.1 (c)  L. Petrov 29-MAY-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*4  M_STA, L_STA, IUER
      CHARACTER  MASTER_DIR*(*), STA_NAME(M_STA)*(*), STA_CODE(M_STA)*(*), &
     &           STA_DOME(M_STA)*(*), STA_DESC(M_STA)*(*)
      INTEGER*4  STA_CDP(M_STA)
      INTEGER*4  M_BUF
      PARAMETER  ( M_BUF = 1024 )
      CHARACTER  FINAM*128, STR*128, BUF(M_BUF)*128, FORMAT_LABEL1*28
      PARAMETER  ( FORMAT_LABEL1 = '* ns-codes.txt 010717 format' )
      LOGICAL*4  LEX
      INTEGER*4  N_BUF, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( M_STA .LT. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_STA, STR )
           CALL ERR_LOG ( 2811, IUER, 'GET_NS_TABLE', 'Parameter M_STA is '// &
     &         'too small: '//STR )
           RETURN
      END IF
!
! --- Build the name of the networking station file
!
      FINAM = MASTER_DIR(1:I_LEN(MASTER_DIR))//NAMSTA_FILE
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX) THEN
           CALL ERR_LOG ( 2812, IUER, 'GET_NS_TABLE', 'File with station '// &
     &                   'codes '//FINAM(1:I_LEN(FINAM))//' was not found. '// &
     &                   'Hint: maybe you forgot to brouwse it from the IVS '// &
     &                   'data center? Subroutine GET_MASTER may do it '// &
     &                   'for you.' )
           RETURN
      END IF
!
! --- Read the networking station file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, M_BUF, BUF, N_BUF, IER )
      IF ( .NOT. LEX) THEN
           CALL ERR_LOG ( 2813, IUER, 'GET_NS_TABLE', 'Error in an attempt '// &
     &                   'to read the file with station codes '//FINAM )
           RETURN
      END IF
!
! --- Check the file label
!
      IF ( BUF(1) .NE. FORMAT_LABEL1 ) THEN
           CALL ERR_LOG ( 2814, IUER, 'GET_NS_TABLE', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' was not recognized as a valid '// &
     &         'station codes file: it has a first line >'// &
     &          BUF(1)(1:I_LEN(BUF(1)))//'< while a format label was expected' )
           RETURN
      END IF
!
! --- Parse the file and extract approriate fields
!
      L_STA = 0
      DO 410 J1=1,N_BUF
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '*' ) GOTO 410
!
         IF ( L_STA .EQ. M_STA ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_STA, STR )
              CALL ERR_LOG ( 2815, IUER, 'GET_NS_TABLE', 'Parameter M_STA '// &
     &            'turned out to be too small: '//STR )
              RETURN
         END IF
!
         L_STA = L_STA + 1
         CALL CLRCH ( STA_NAME(L_STA) )
         CALL CLRCH ( STA_CODE(L_STA) )
         CALL CLRCH ( STA_DOME(L_STA) )
         CALL CLRCH ( STA_DESC(L_STA) )
         STA_NAME(L_STA) = BUF(J1)(5:12)
         CALL VTD_NAME_REPAIR ( STA_NAME(L_STA) )
         STA_CODE(L_STA) = BUF(J1)(2:3)
         STA_DOME(L_STA) = BUF(J1)(14:22)
         STA_DESC(L_STA) = BUF(J1)(29:)
         CALL CHIN ( BUF(J1)(24:27), STA_CDP(L_STA) )
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_NS_TABLE  #!#
