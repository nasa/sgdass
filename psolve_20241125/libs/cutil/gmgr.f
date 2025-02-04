      SUBROUTINE GMGR ( MGRMAP, MGR_WARNING, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GMGR  reads mean troposphere gradient mapping file and    *
! *   fills fields of flyby.i common block. It reads mapping file,       *
! *   parses and puts values of North and East mean gradients in the     *
! *   fields of a common block  flyby.i  for the stations which have     *
! *   participated in the session.                                       *
! *                                                                      *
! *   If MGR_WARNING is TRUE routine checks whether gradients were       *
! *   supplied for each station and issues a warning if a station was    *
! *   missed from the file.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MGRMAP ( CHARACTER ) -- Filename of the mapping file for        *
! *                              mean gradient. File should be in MGR    *
! *                              format.                                 *
! * MGR_WARNING ( LOGICAL*4 ) -- If TRUE a warning will be issued if     *
! *                              there is the station which participated *
! *                              in the session was not found in the     *
! *                              mapping mean gradient file.             *
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
! *  ### 25-SEP-2000      GMGR     v1.0 (c)  L. Petrov  25-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'flyby.i'
!
      CHARACTER  MGRMAP*(*)
      LOGICAL*4  MGR_WARNING
      INTEGER*4  IUER
      INTEGER*4  MBUF, M_STA
      PARAMETER  ( MBUF=1024 )
      PARAMETER  ( M_STA = MAX_ARC_STA )
!
      LOGICAL*4  LEX, FL_MGR(M_STA)
      CHARACTER  BUF(MBUF)*80, STR*72, FINAM*128
      INTEGER*4  NBUF, IP, IOS, L_STA, J1, J2, J3, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Build a file name
!
      IF ( MGRMAP(1:1) .EQ. '/' ) THEN
           FINAM = MGRMAP
         ELSE
           FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//MGRMAP
      END IF
!
! --- Check: whether mapping file really exist
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1801, IUER, 'GMGR', 'Mapping file of mean '// &
     &         'troposphere gradients '//FINAM(1:I_LEN(FINAM))//' was not '// &
     &         'found' )
           RETURN
      END IF
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1802, IUER, 'GMGR', 'Error in attempt to read '// &
     &         'mapping file of mean troposphere gradients '//FINAM )
           RETURN
      END IF
!
! --- Check: whether it has the format identifier
!
      IF ( BUF(1)(1:18) .NE. '# MGR-FORMAT V 1.0' ) THEN
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 1803, IUER, 'GMGR', 'File '//FINAM(1:I_LEN(FINAM))// &
     &         ' is not a mapping file of mean troposphere gradients. '// &
     &         'Its first line is '//STR(1:I_LEN(STR))//'  Any valid MGR '// &
     &         'file should have "# MGR-FORMAT V 1.0" at the beginning of '// &
     &         'the first line' )
           RETURN
      END IF
!
! --- Initialization
!
      L_STA = INT4(NUMSTA)
      DO 410 J1=1,L_STA
         FL_MGR(J1) = .FALSE.
         MGR_NORTH(J1) = 0.0D0
         MGR_EAST(J1) = 0.0D0
 410  CONTINUE
!
! --- Parsing file, line by line
!
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
         IF ( BUF(J2)(1:1) .EQ. '$' ) GOTO 420
         IF ( BUF(J2)(1:1) .EQ. '*' ) GOTO 420
!
! ------ BUF(J2)(1:8) ontains station name. Look this name in the list of
! ------ stations participated in the session ISITN_CHR
!
         IP = LTM_DIF ( 0, L_STA, ISITN_CHR, BUF(J2)(1:8) )
         IF ( IP .GT. 0 ) THEN
!
! ----------- Decode north gradient...
!
              READ ( BUF(J2)(13:20), FMT='(F8.4)', IOSTAT=IOS ) MGR_NORTH(IP)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 1804, IUER, 'GMGR', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in decoding line of mean '// &
     &                 'troposphere gradient file '//FINAM(1:I_LEN(FINAM))// &
     &                 '  "'//BUF(J2)(1:I_LEN(BUF(J2)))//'" ' )
                   RETURN
              END IF
!
! ----------- Decoide east gradient
!
              READ ( BUF(J2)(37:44), FMT='(F8.4)', IOSTAT=IOS ) MGR_EAST(IP)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 1805, IUER, 'GMGR', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in decoding line of mean '// &
     &                 'troposphere gradient file '//FINAM(1:I_LEN(FINAM))// &
     &                 '  "'//BUF(J2)(1:I_LEN(BUF(J2)))//'" ' )
                   RETURN
              END IF
!
! ----------- TRansform gradients to meters
!
              MGR_NORTH(IP) = MGR_NORTH(IP)*1.D-3
              MGR_EAST(IP)  = MGR_EAST(IP)*1.D-3
!
! ----------- and set a flag that IP-th station is OK
!
              FL_MGR(IP) = .TRUE.
         END IF
 420  CONTINUE
!
      IF ( MGR_WARNING ) THEN
!
! -------- Check: whether there are stations missed in the mapping mean
! -------- tropospher gradient file
!
           DO 430 J3=1,L_STA
              IF ( .NOT. FL_MGR(J3) ) THEN
                   WRITE (  *, '(A)' ) 'GMGR Warning: station '//ISITN_CHR(J3)// &
     &                                 ' was not found in mapping mean'// &
     &                                 ' gradient file'
                   WRITE ( 23, '(A)' ) 'GMGR Warning: station '//ISITN_CHR(J3)// &
     &                                 ' was not found in mapping mean'// &
     &                                 ' gradient file'
              END IF
430       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GMGR  #!#
