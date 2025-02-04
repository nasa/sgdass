      SUBROUTINE VTD_LOAD_SOUPRL ( VTD, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_LOAD_SOUPRL  reads information from the      *
! *   catalogue catalogue of source proper motion and parallax defined   *
! *   in the internal fields of VTD about the sources defined in the     *
! *   input list. It extracts information from that catalogue and loads  *
! *   it into fields of VTD%SOU.                                         *
! *   
! *   If a source is defined in more than one catalogue or defined more  *
! *   than once in the same catalogue, the latest definition will        *
! *   override the previous one.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      MBUF ( INTEGER*4 ) -- The length of the buffer in lines.        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       BUF ( CHARACTER ) -- The text buffer. Dimension: MBUF.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 11-MAR-2023 VTD_LOAD_SOUPRL  v1.1 (c) L. Petrov 14-MAR-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, L_SOU, IUER
      CHARACTER  BUF(MBUF)*(*)
      CHARACTER  STR*80, ALPHA_STR*17, DELTA_STR*17
      CHARACTER  SOUPRL__LABEL1*64
      PARAMETER  ( SOUPRL__LABEL1 = &
     & '# Source proper motion and parallax. File format  1.0  of 2023.03.11' )
      LOGICAL*4  LEX, FL_FOUND(VTD__M_SOU)
      CHARACTER  V_SOU(VTD__M_SOU)*8
      REAL*8     SEC_REF, TAI_REF
      INTEGER*4  NUM_ERR, I_FMT, IND_SOU, NBUF, IB, J1, J2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_SOUPRL_PRP, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2191, IUER, 'VTD_LOAD_SOUPRL', 'Catalogue of '// &
     &         'source parallaxes and proper motions '// &
     &         TRIM(VTD%CONF%FINAM_SOUPRL_PRP)//' does not exist' )
              RETURN 
      END IF
!
! --- Read the file with source parallaxes and proper motions
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_SOUPRL_PRP, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2192, IUER, 'VTD_LOAD_SOUPRL', 'Error in an '// &
     &         'attempt to read input file with source proper motions '//&
     &         'and parallaxes '//VTD%CONF%FINAM_SOUPRL_PRP )
           RETURN 
      END IF
!
! --- Check the label and determine the format
!
      I_FMT = 0
      IF ( BUF(1)(1:LEN(SOUPRL__LABEL1)) .EQ. SOUPRL__LABEL1 ) THEN
           I_FMT = 1
         ELSE 
           CALL ERR_LOG ( 2193, IUER, 'VTD_LOAD_SOUPRL', 'Error in an '// &
     &         'attempt to parse the input file with source parallaxes and '// &
     &         'proper motions '//TRIM(VTD%CONF%FINAM_SOUPRL_PRP)// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Parse file with source proper motions and parallaxes
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '$' ) GOTO 410
         IF ( I_FMT .EQ. 1 ) THEN
!
! ----------- Search the source BUF(J2)(1:8) among the sources that 
! ----------- have already been found in this catalgoue. 
!
              IND_SOU = 0
              DO 420 J2=1,VTD%L_SOU
                 IF ( BUF(J1)(1:8) == VTD%SOU(J2)%IVS_NAME ) THEN
                      IND_SOU = J2
                 END IF
 420          CONTINUE 
              IF ( IND_SOU < 1 ) GOTO 410
!
              VTD%SOU(IND_SOU)%J2000_NAME = BUF(J1)(11:20)
              READ ( UNIT=BUF(J1)(23:32), FMT='(F10.3)', IOSTAT=IER ) VTD%SOU(IND_SOU)%ALPHA_RATE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2194, IUER, 'VTD_LOAD_SOUPRL', 'Error in an '// &
     &                 'attempt to parse the '//TRIM(STR)//'th line of the '// &
     &                 'input file with source parallaxes and '// &
     &                 'proper motions '//TRIM(VTD%CONF%FINAM_SOUPRL_PRP)// &
     &                 ' -- error in reading proper motion along right ascension '// &
     &                 BUF(J1)(23:20) )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(35:44), FMT='(F10.3)', IOSTAT=IER ) VTD%SOU(IND_SOU)%DELTA_RATE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2195, IUER, 'VTD_LOAD_SOUPRL', 'Error in an '// &
     &                 'attempt to parse the '//TRIM(STR)//'th line of the '// &
     &                 'input file with source parallaxes and '// &
     &                 'proper motions '//TRIM(VTD%CONF%FINAM_SOUPRL_PRP)// &
     &                 ' -- error in reading proper motion along declination '// &
     &                 BUF(J1)(35:44) )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(47:54), FMT='(F10.3)', IOSTAT=IER ) VTD%SOU(IND_SOU)%PRLX
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2196, IUER, 'VTD_LOAD_SOUPRL', 'Error in an '// &
     &                 'attempt to parse the '//TRIM(STR)//'th line of the '// &
     &                 'input file with source parallaxes and '// &
     &                 'proper motions '//TRIM(VTD%CONF%FINAM_SOUPRL_PRP)// &
     &                 ' -- error in reading parallax '//BUF(J1)(47:54) )
                   RETURN 
              END IF
!
! ----------- Transform units to SI
!
              VTD%SOU(IND_SOU)%ALPHA_RATE = VTD%SOU(IND_SOU)%ALPHA_RATE*MAS__TO__RAD/YEAR__TO__SEC
              VTD%SOU(IND_SOU)%DELTA_RATE = VTD%SOU(IND_SOU)%DELTA_RATE*MAS__TO__RAD/YEAR__TO__SEC
              VTD%SOU(IND_SOU)%PRLX = VTD%SOU(IND_SOU)%PRLX*MAS__TO__RAD
              IF ( VTD%SOU(IND_SOU)%PRLX > 1.0D-20 ) THEN
                   VTD%SOU(IND_SOU)%DIST = AU__METERS/VTD%SOU(IND_SOU)%PRLX
                   VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__GAL
              END IF
!
! ----------- Compute the vector of change of source position
!
              VTD%SOU(IND_SOU)%S_CRS_RATE(1) = -DSIN(VTD%SOU(IND_SOU)%DELTA) * DCOS(VTD%SOU(IND_SOU)%ALPHA) * VTD%SOU(IND_SOU)%DELTA_RATE &
     &                                         -DCOS(VTD%SOU(IND_SOU)%DELTA) * DSIN(VTD%SOU(IND_SOU)%ALPHA) * VTD%SOU(IND_SOU)%ALPHA_RATE
              VTD%SOU(IND_SOU)%S_CRS_RATE(2) = -DSIN(VTD%SOU(IND_SOU)%DELTA) * DSIN(VTD%SOU(IND_SOU)%ALPHA) * VTD%SOU(IND_SOU)%DELTA_RATE &
     &                                         +DCOS(VTD%SOU(IND_SOU)%DELTA) * DCOS(VTD%SOU(IND_SOU)%ALPHA) * VTD%SOU(IND_SOU)%ALPHA_RATE
              VTD%SOU(IND_SOU)%S_CRS_RATE(3) =  DCOS(VTD%SOU(IND_SOU)%DELTA) * VTD%SOU(IND_SOU)%DELTA_RATE
          END IF
 410   CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_SOUPRL  !#!#
