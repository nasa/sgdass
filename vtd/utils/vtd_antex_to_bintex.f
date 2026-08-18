      PROGRAM  VTD_ANTEX_TO_BINTEX
! ************************************************************************
! *                                                                      *
! *   Program VTD_ANTEX_TO_BINTEX parses a file with phase center        *
! *   offsets for both space-born satellite and ground antennas, mainly  *
! *   for GNSS observations, splits the data, and writes into output     *
! *   files in binary BINTEX format.                                     *
! *                                                                      *
! *   The input file is in the International Global Navigation Satellite *
! *   System Service (IGS) ascii ANTEX format defined in                 *
! *   https://files.igs.org/pub/data/format/antex14.txt . It contains    *
! *   information about both space-born and ground antenna.              *
! *                                                                      *
! *   As of September 2025, the latest IGS file with antenna phase       *
! *   center offset is                                                   *
! *   https://files.igs.org/pub/station/general/igs20.atx                *
! *                                                                      *
! *   An output file has information only about one antenna, either      *
! *   space-born or ground. The output file is in binary BINTEX format.  *
! *                                                                      *
! *   Usage: vtd_antex_to_bintex antex_ascii_file output_directory       *
! *                                                                      *
! *   All files for space-born satellite antennas are written in         *
! *   sub-directory bintex_sat with respect to VTD shared directory      *
! *   defined during installation. Naming convention:                    *
! *   SSS_FFFFFFFF_UUUUUUUU.btx  where                                   *
! *       SSS      -- IGS three-letter GNSS satellite code               *
! *       FFFFFFFF -- "validity from" date in YYYYDDMM format            *
! *       UUUUUUUU -- "validity until" date in YYYYDDMM format           *
! *       .btx     -- extension.                                         *
! *                                                                      *
! *   All files for ground antennas are written in sub-directory         *
! *   bintex_sta with respect to VTD shared directory                    *
! *   defined during installation. Naming convention:                    *
! *   TTTTTTT.btx  where                                                 *
! *       TTTTTTTT -- antenna type defined in the input phase center     *
! *                   offset file in accordance with the IGS convention. *
! *                   The convention allows to use character "/" in the  *
! *                   antenna type. Character "/" is replaced with "@"   *
! *                   in the file name.                                  *
! *       .btx     -- extension.                                         *
! *                                                                      *
! *   If Antenna Serial Number field  is NONE, it is not consider a part *
! *   of antenna type field. If it is not NONE, antenna serial number is *
! *   considered as a part of the antenna type field. All blanks inside  *
! *   are replaced with underscores.                                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 02-SEP-2025 VTD_ANTEX_TO_BINTEX v1.0 (d) L. Petrov 08-SEP-2025 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'vtd_local.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  FIL_ANTEX*128, DIR_OUT*128, DIR_SAT*128, DIR_STA*128
      INTEGER*4  IUER
!
!      FIL_ANTEX = '/apr/sats/igs20.atx'
!      DIR_SAT   = VTD__DATA//'/bintex_sat'
!      DIR_STA   = VTD__DATA//'/bintex_sta'
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: vtd_antex_to_bintex antex_ascii_file output_directory'
           CALL EXIT ( 1 )
        ELSE
           CALL GETARG ( 1, FIL_ANTEX )
           CALL GETARG ( 2, DIR_OUT )
      END IF
!
! --- Directory names with phase center offsets for satellites and for stations
!
      DIR_SAT   = TRIM(DIR_OUT)//'/bintex_sat'
      DIR_STA   = TRIM(DIR_OUT)//'/bintex_sta'
!
! --- Unitialuze VTD object
!
      IUER = -1
      CALL VTD_INIT ( VTD, IUER )
!
! --- Parse satellite section of the file with phase center offsets in
! --- IGS ascii ANTEX format
!
      IUER = -1
      CALL VTD_READ_ANTEX_SAT  ( VTD, FIL_ANTEX, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Write phase center offsets for all satellites into binary files in BINTEX
! --- format, one file per satellite 
!
      IUER = -1
      CALL VTD_WRITE_BINTEX ( VTD, VTD__SPACE, DIR_SAT, FIL_ANTEX, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'VTD_ANTES_TO_BINTEX wrote ', INT2(VTD%L_PCO), ' output '// &
     &               'files with phase center offsets for satellites into '// &
     &               TRIM(DIR_SAT)
!
! --- Parse ground station section of the file with phase center offsets in
! --- IGS ascii ANTEX format
!
      IUER = -1
      CALL VTD_READ_ANTEX_STA  ( VTD, FIL_ANTEX, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Write phase center offsets for all ground stations into binary 
! --- files in BINTEX format, one file per satellite 
!
      IUER = -1
      CALL VTD_WRITE_BINTEX ( VTD, VTD__GROUND, DIR_STA, FIL_ANTEX, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'VTD_ANTES_TO_BINTEX wrote ', INT2(VTD%L_PCO), ' output '// &
     &               'files with phase center offsets for stations into '// &
     &               TRIM(DIR_STA)
!
      END  PROGRAM  VTD_ANTEX_TO_BINTEX  !#!  
