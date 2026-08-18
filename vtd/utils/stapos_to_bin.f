      PROGRAM    STAPOS_TO_BIN
! ************************************************************************
! *                                                                      *
! *   Program  STAPOS_TO_BIN process a directory with files in ascii     *
! *   station position evolution format and tranforms each of them       *
! *   to the binary BINDSIP station displacement format. It subtracts    *
! *   the mean position at the reference epoch and mean station velocity *
! *   specifed in the header of data files. Therefore, the output files  *
! *   contina the residual deisplacements after subraction the linear    *
! *   model.                                                             *
! *                                                                      *
! *   Usage: stapos_to_bin dirin dirout [verbosity]                      *
! *                                                                      *
! *   where dirin  --    input directory with data files in the station  *
! *                      position evolution format. One file contains    *
! *                      time series of position of a given station.     *
! *         dirout --    output directory with residual station          *
! *                      displacements in BINDISP format.                *
! *         verbosity -- optional verbosity parameters.                  *
! *                      0 -- silent mode ( defaults ).                  *
! *                      1 -- normal verbosity.                          *
! *                     >1 -- debigging messages are printed.            *
! *                                                                      *
! *   In order the displacements file could be processed by VTD, one     *
! *   needs to create the summary afte rynning staps_to_bin:             *
! *                                                                      *
! *   bds_util summary dirout                                            *
! *                                                                      *
! *   bds_util is a part of MALO package.                                *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 16-JUL-2025  STAPOS_TO_BIN  v1.0 (d) L. Petrov  21-JUL-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4    M_FIL, MP
      PARAMETER  ( M_FIL = 2048 )
      PARAMETER  ( MP   = 128*1024 )
      CHARACTER  DIRIN*128, DIROUT*128, FILS(M_FIL)*128, FINAM*128, &
     &           STA_NAM*8, MOD_NAM*64, FILOUT*128, STR*128
      REAL*8     TAI_REF, COO(3,M_FIL), VEL(3,M_FIL), RES_POS(3,MP), TAI(MP)
      INTEGER*8  DIR_DESC(16)
      INTEGER*4  LEV, IVRB, L_FIL, IL, IS, MJD_REF, MJD(MP), LP, &
     &           J1, J2, J3, IUER
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, GET_FILE_FROM_DIR
!
      IVRB = 1 !  Default verbosity
!
! --- Parse input arguments
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: stapos_to_bin dir_in dir_out [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DIRIN  )
           CALL GETARG ( 2, DIROUT ) 
           IF ( IARGC() .GE.  3 ) THEN
                CALL GETARG ( 3, STR ) 
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
! --- Read directory with input files
!
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FINAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 5401, IUER, 'STAPOS_TO_BIN', 'Error in '// &
     &                 'reading input directory '//DIRIN )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
!
! ------ Bypass temporary or very short files
!
         IF ( INDEX ( FINAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FINAM, '~' ) .GT. 0 ) GOTO 410
         IL = ILEN(FINAM)
         IF ( IL < 4 ) GOTO 410
!
! ------ Include this file
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > M_FIL) THEN
              IUER = -2
              CALL CLRCH ( STR )
              CALL INCH  ( M_FIL, STR )
              CALL ERR_LOG ( 5402, IUER, 'STAPOS_TO_BIN', 'Too many files '// &
     &            'in directory '//DIRIN(1:I_LEN(DIRIN))//' -- '// &
     &            'more than '//STR )
              CALL EXIT ( 1 )
         END IF
         FILS(L_FIL) = FINAM
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5403, IUER, 'STAPOS_TO_BIN', 'No data find in '// &
     &         'the input directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Sort file names
!
      CALL SORT_CH ( L_FIL, FILS ) 
      DO 420 J2=1,L_FIL
!
! ------ Parse the J2-th input file. 
! ------ Get MOD_NAM, STA_NAM, MJD_REF, TAI_REF, MP, LP, MJD, TAI, RES_POS, COO, VEL
!
         IUER = -1
         CALL PARSE_STAPOS ( FILS(J2), MOD_NAM, STA_NAM, &
     &                       MJD_REF, TAI_REF, MP, LP, MJD, TAI, &
     &                       RES_POS, COO(1,J2), VEL(1,J2), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5405, IUER, 'STAPOS_TO_BIN', 'Error in parsing '// &
     &            'station position file '//FILS(J2) )
              CALL EXIT ( 1 )
         END IF
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, 110 ) J2, L_FIL, STA_NAM
 110          FORMAT ( 'Processing station ', I4, ' ( ', I4, ' )  name: ', A ) 
         END IF
!
! ------ Build the output file name
!
         FILOUT = TRIM(DIROUT)//'/'//TRIM(STA_NAM)//'.bds'
!
! ------ Writing STA_NAM, MOD_NAM, LP, MJD, TAI, RES_POS, COO
! ------ in to the output file FILOUT
!
         IUER = -1
         CALL STAPOS_TO_BINDISP ( STA_NAM, MOD_NAM, LP, MJD, TAI, &
     &                            RES_POS, COO(1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5406, IUER, 'STAPOS_TO_BIN', 'Error in writing '// &
     &            'in the station position file '//FILS(J2) )
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
!
      END  PROGRAM  STAPOS_TO_BIN  !#!#
