      SUBROUTINE SOUIVS_TO_SOURFC ( FIL_SOU, N_SOU, SOU_IVS, SOU_RFC,   &
     &                              IUER )
!
! *****************************************************************************
! *                                                                           *
! *  Routine SOUIVS_TO_SOURFC converts a source's IVS name to it's RFC ID.    *
! *  N.B: - The RFC ID == J2000-name                                          *
! *                                                                           *
! *  INPUT:                                                                   *
! *            FIL_SOU  =  Source Position File            { CHAR }           *
! *                        This file contains positions and uncertainties of  *
! *                        over 28k objects, given their RFC ID and IVS name. *
! *                        N.B: - use either glo.src or source.names          *
! *                                                                           *
! *            N_SOU    =  No. of sources                  { INT }            *
! *                                                                           *
! *            SOU_IVS  =  Array of source IVS names       { CHAR }           *
! *                                                                           *
! *            IUER     =  Error Handler                   { INT, OPT }       *
! *                        If IUER=0 no error message will be printed, even   *
! *                        in the event of an error. However, for  other      *
! *                        possible values, i.e. IUER=-1,-2, & -3, the error  *
! *                        message will print to screen. For the latter case, *
! *                        i.e. IUER=-3, after printing the program will      *
! *                        terminate.                                         *
! *                        Default, IUER = -1                                 *
! *                                                                           *
! *  OUTPUT:                                                                  *
! *            SOU_RFC  =  Array of source RFC ID's        { CHAR }           *
! *                                                                           *
! *  ### 17-NOV-2020  SOUIVS_TO_SOURFC  v1.0 (c)  N. Habana  17-NOV-2020 ###  *
! *                                                                           *
! *****************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   N_SOU, IUER, IER
      CHARACTER   FIL_SOU*(*)
      CHARACTER   SOU_RFC(N_SOU)*16, SOU_IVS(N_SOU)*16
      CHARACTER   DELIM*3
      INTEGER*4   MP, MIND
      INTEGER*4   MAXL_STRING
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9) ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)
      CHARACTER   SOU_NAM_IVS*16, SOU_NAM_RFC*16
      INTEGER*4   NP, LIND, IND(2,MIND)
      INTEGER*4   J0, J1, J2, J3, J4
      INTEGER*4,  EXTERNAL :: ILEN
!
! --- Reading the Source name file.
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_SOU, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1201, IUER, 'SOURFC_TO_SOUIVS',               &
     &             'Error in reading input file: '//FIL_SOU )
           RETURN
      END IF
!
! --- Go through the Sources and match the SOU_RFC and SOU_IVS arrays
!
      DO 310 J0 = 1, N_SOU
!
! ------ Go through file to find matching source
!
         DO 410 J1 = 1, NP
! ---------
            IF ( ILEN(BUF(J1)) == 0 )  GOTO 410 ! Bypass empty lines
! ---------
            IF ( BUF(J1)(1:1) == '#' ) GOTO 410 ! Bypass comment lines
!
! --------- Extract words from the read line
!
            CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!            
! --------- Label the extracted words
!
            SOU_NAM_IVS = BUF(J1)(IND(1,1):IND(2,1))
            SOU_NAM_RFC = BUF(J1)(IND(1,2):IND(2,2))
!
! --------- Check if this matches what we are looking for
!
            IF ( SOU_IVS(J0) .EQ. SOU_NAM_IVS ) THEN
               SOU_RFC(J0) = SOU_NAM_RFC
               GO TO 310        ! Move to the next source
            END IF
 410     CONTINUE
 310  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE
