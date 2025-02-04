      SUBROUTINE VTD_MPL_READ ( VTD, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_MPL_READ  reads the file with the model of secular    *
! *   polar motion, parses it and stores in the internal fields of       *
! *   VTD object.
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! *  ### 15-JUN-2004  VTD_MPL_READ  v1.0 (c)  L. Petrov 15-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 256,  MIND = 32 ) 
      LOGICAL*4  LEX
      CHARACTER  BUF(M_BUF)*128, DELIM*3, STR*80, STR1*80
      PARAMETER  ( DELIM = CHAR(0)//CHAR(9)//CHAR(32) )
      INTEGER*4  LIND, IND(2,MIND), N_BUF, J1, IOS, I_MPL, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=VTD%CONF%MPL_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
            CALL ERR_LOG ( 2381, IUER, 'VTD_MPL_READ', 'File specified in '// &
     &          'MPL_FILE keyword in the control file '// &
     &           VTD%CONF%CONFIG_FINAM(1:I_LEN(VTD%CONF%CONFIG_FINAM))//' -- '// &
     &           VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &          ' was not found' )
            RETURN 
      END IF
!
! --- Read mean pole file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%MPL_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2382, IUER, 'VTD_MPL_READ', 'Error reading '// &
     &         'mean pole file '//VTD%CONF%MPL_FILE )
           RETURN
      END IF
!
! --- Check the header line ( there should be a label )
!
      IF ( BUF(1)(1:LEN(MPL__LABEL)) .NE. MPL__LABEL ) THEN
           CALL ERR_LOG ( 2383, IUER, 'VTD_MPL_READ', 'Wrong format of the '// &
     &         'mean pole file '// &
     &          VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))//' -- '// &
     &         'Unrecognized format label' )
           RETURN
      END IF
!
! --- Check the trailing line
!
      IF ( BUF(N_BUF)(1:LEN(MPL__LABEL)) .NE. BUF(1)(1:LEN(MPL__LABEL)) ) THEN
           CALL ERR_LOG ( 2384, IUER, 'VTD_MPL_READ', 'Mean pole file '// &
     &          VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))//' was '// &
     &         'read not to the end: footer line is missing' )
           RETURN
      END IF
!
! --- Parse the file
!
      I_MPL = 0
      DO 410 J1=2,N_BUF-1
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
!
! ------ Split the line onto words
!
         CALL TRAN   ( 11, BUF(J1), BUF(J1) )
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly two words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 2385, IUER, 'VTD_CONF', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'mean pole file '// &
     &            VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &            ' : too few words' )
              RETURN
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'XPOLE_REF:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%XPOL_REF_SEC
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2386, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'SEC' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'YEAR' ) THEN
                   VTD%CONF%XPOL_REF_SEC = (VTD%CONF%XPOL_REF_SEC - 2000.0D0)* &
     &                                 86400.0D0*365.25D0
                 ELSE 
                   CALL ERR_LOG ( 2387, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'YPOLE_REF:'   ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%YPOL_REF_SEC
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2388, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'SEC' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'YEAR' ) THEN
                   VTD%CONF%YPOL_REF_SEC = (VTD%CONF%YPOL_REF_SEC - 2000.0D0)* &
     &                                 86400.0D0*365.25D0
                 ELSE 
                   CALL ERR_LOG ( 2389, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'XPOLE_MEAN:'  ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%XPOL_MEAN
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2390, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'RAD' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'NRAD' ) THEN
                   VTD%CONF%XPOL_MEAN = 1.D-9*VTD%CONF%XPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'PRAD' ) THEN
                   VTD%CONF%XPOL_MEAN = 1.D-12*VTD%CONF%XPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'ARCSEC' ) THEN
                   VTD%CONF%XPOL_MEAN  = MAS__TO__RAD*1.D3*VTD%CONF%XPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'MAS' ) THEN 
                   VTD%CONF%XPOL_MEAN = MAS__TO__RAD*VTD%CONF%XPOL_MEAN 
                 ELSE 
                   CALL ERR_LOG ( 2391, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'YPOLE_MEAN:'  ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%YPOL_MEAN
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2392, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'RAD' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'NRAD' ) THEN
                   VTD%CONF%YPOL_MEAN = 1.D-9*VTD%CONF%YPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'PRAD' ) THEN
                   VTD%CONF%YPOL_MEAN = 1.D-12*VTD%CONF%YPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'ARCSEC' ) THEN
                   VTD%CONF%YPOL_MEAN  = MAS__TO__RAD*1.D3*VTD%CONF%YPOL_MEAN 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'MAS' ) THEN 
                   VTD%CONF%YPOL_MEAN = MAS__TO__RAD*VTD%CONF%YPOL_MEAN 
                 ELSE 
                   CALL ERR_LOG ( 2393, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'XPOLE_DRIFT:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%XPOL_DRIFT
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2394, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'RAD/SEC' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'NRAD/SEC' ) THEN
                   VTD%CONF%XPOL_DRIFT = 1.D-9*VTD%CONF%XPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'PRAD/SEC' ) THEN
                   VTD%CONF%XPOL_DRIFT = 1.D-12*VTD%CONF%XPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'ARCSEC/YEAR' ) THEN
                   VTD%CONF%XPOL_DRIFT = MAS__TO__RAD*1.D3/86400.0D0/365.25D0* &
     &                                   VTD%CONF%XPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'MAS/YEAR' ) THEN 
                   VTD%CONF%XPOL_DRIFT = MAS__TO__RAD/86400.0D0/365.25D0* &
     &                                   VTD%CONF%XPOL_DRIFT 
                 ELSE 
                   CALL ERR_LOG ( 2395, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) .EQ. 'YPOLE_DRIFT:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.8)', &
     &               IOSTAT=IOS ) VTD%CONF%YPOL_DRIFT
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2396, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- error in format conversion' ) 
                   RETURN
              END IF
!
              IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'RAD/SEC' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'NRAD/SEC' ) THEN
                   VTD%CONF%YPOL_DRIFT = 1.D-9*VTD%CONF%YPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'PRAD/SEC' ) THEN
                   VTD%CONF%YPOL_DRIFT = 1.D-12*VTD%CONF%YPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'ARCSEC/YEAR' ) THEN
                   VTD%CONF%YPOL_DRIFT = MAS__TO__RAD*1.D3/86400.0D0/365.25D0* &
     &                                   VTD%CONF%YPOL_DRIFT
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) .EQ. 'MAS/YEAR' ) THEN 
                   VTD%CONF%YPOL_DRIFT = MAS__TO__RAD/86400.0D0/365.25D0* &
     &                                   VTD%CONF%YPOL_DRIFT 
                 ELSE 
                   CALL ERR_LOG ( 2397, IUER, 'VTD_CONF', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'mean pole file '// &
     &                 VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &                 ' -- unsupported unit: '//BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
              I_MPL = I_MPL + 1
         END IF
 410  CONTINUE 
!
      IF ( I_MPL .LT. M_MPL ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( I_MPL, STR ) 
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( M_MPL, STR1 ) 
           CALL ERR_LOG ( 2399, IUER, 'VTD_CONF', 'Error in '// &
     &         'parsing the mean pole file '// &
     &          VTD%CONF%MPL_FILE(1:I_LEN(VTD%CONF%MPL_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' records were found, while '// &
     &         STR1(1:I_LEN(STR1))//' was expected' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_MPL_READ 
