      SUBROUTINE VTD_ANTEX_FREQ ( VTD, FREQ_CHAR, ITYPE, IFRQ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_ANTEX_FREQ assigns a frequency in HZ                  *
! *   from a RINEX frequency string                                      *
! *                                                                      *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.1 (d)  J. Skeens  03-SEP-2025 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      CHARACTER  FREQ_CHAR*3
      LOGICAL*4  LEX, IN_RECORD, IN_TABLE
      INTEGER*4  ITYPE, IFRQ
      REAL*8     FREQ_HZ
!
      IF ( FREQ_CHAR == 'G01' .OR. FREQ_CHAR == 'E01' .OR. FREQ_CHAR == 'J01' &
     &     .OR. FREQ_CHAR == 'S01' .OR. FREQ_CHAR == 'C01' .OR. FREQ_CHAR == 'C02' ) THEN
          FREQ_HZ = 1.57542D9
      ELSE IF ( FREQ_CHAR == 'G02' .OR. FREQ_CHAR == 'J02' ) THEN
          FREQ_HZ = 1.22760D9
      ELSE IF ( FREQ_CHAR == 'G05' .OR. FREQ_CHAR == 'E05' .OR. FREQ_CHAR == 'J05' &
     &     .OR. FREQ_CHAR == 'C05' .OR. FREQ_CHAR == 'S05' .OR. FREQ_CHAR == 'I05' ) THEN
          FREQ_HZ = 1.117645D9 
      ELSE IF ( FREQ_CHAR == 'I09' ) THEN
          FREQ_HZ = 2.492028D9 
      ELSE IF ( FREQ_CHAR == 'R01' .OR. FREQ_CHAR == 'R04' ) THEN
          FREQ_HZ = 1.602D9
      ELSE IF ( FREQ_CHAR == 'R02' .OR. FREQ_CHAR == 'R06' ) THEN
          FREQ_HZ = 1.246D9
      ELSE IF ( FREQ_CHAR == 'E06' .OR. FREQ_CHAR == 'J06' .OR. FREQ_CHAR == 'C06' ) THEN
          FREQ_HZ = 1.27875D9
      ELSE IF ( FREQ_CHAR == 'E07' .OR. FREQ_CHAR == 'C07' ) THEN
          FREQ_HZ = 1.20714D9
      ELSE IF ( FREQ_CHAR == 'E08' .OR. FREQ_CHAR == 'C08' ) THEN
          FREQ_HZ = 1.191795D9
      ELSE IF ( FREQ_CHAR == 'R03' ) THEN
          FREQ_HZ = 1.202025D9
      ELSE
          CALL ERR_LOG ( 2924, IUER, 'VTD_ANTEX_FREQ', 'Error in '//  &
     &        ' assigning RINEX frequency to sky frequency '//  &
     &        ' unknown ANTEX frequency code ' //FREQ_CHAR )
          RETURN 
      END IF
!
      VTD%PCO(ITYPE)%BAND(IFRQ)%FREQ = FREQ_HZ
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_ANTEX_FREQ  !#!#
