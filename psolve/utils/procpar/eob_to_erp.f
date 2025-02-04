      PROGRAM    EOB_TO_ERP
! ************************************************************************
! *                                                                      *
! *   Program EOB_TO_ERP
! *                                                                      *
! *  ### 08-OCT-2023   EOB_TO_ERP  v1.0 (c)  L. Petrov  08-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INTEGER*4    M_HEAD, MP
      PARAMETER  ( M_HEAD =     516 )
      PARAMETER  ( MP     = 64*1024 )
      TYPE ( EOP__STRU  ) :: EOP(MP)
      CHARACTER  FIL_EOB*128, FIL_ERP*128
      CHARACTER  BUFI(MP)*256, BUFO(MP)*75, HEAD_BUF(M_HEAD)*128
      REAL*8     STEP_DAYS
      INTEGER*4  J1, J2, J3, LUN, NHEAD, NP, NO, IUER 
      INTEGER*4, EXTERNAL :: GET_UNIT
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage : eob_to_erp fil_eob fil_erp'
           CALL EXIT ( 1 )
        ELSE 
           CALL GETARG ( 1, FIL_EOB ) 
           CALL GETARG ( 2, FIL_ERP ) 
      END IF
!
      IUER = -1
      CALL READ_EOB ( FIL_EOB, M_HEAD, NHEAD, HEAD_BUF, MP, NP, EOP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3801, IUER, 'EOB_TO_ERP', 'Error in '// &
     &         'reading EOB file '//TRIM(FIL_EOB) )
           CALL EXIT ( 1 )
      END IF
      STEP_DAYS = EOP(2)%MJD_EOP - EOP(1)%MJD_EOP 
      NO = 1
      WRITE ( UNIT=BUFO(NO), FMT=110 ) 2400000.5D0 + EOP(1)%MJD_EOP, STEP_DAYS, NP
 110  FORMAT ( 'EOP-MOD Ver 2.0  ', F11.3, F6.3, I5, '  UT1-TAI   UNDEF                  ' )
      IF ( BUFO(NO)(29:29) == '0'   ) BUFO(NO)(29:29) = ' '
      IF ( BUFO(NO)(28:29) == '0 '  ) BUFO(NO)(28:29) = '  '
      IF ( BUFO(NO)(27:29) == '0  ' ) BUFO(NO)(27:29) = '   '
      IF ( BUFO(NO)(34:34) == '0'   ) BUFO(NO)(34:34) = ' '
      IF ( BUFO(NO)(33:34) == '0 '  ) BUFO(NO)(33:34) = '  '
!
      NO = NO + 1 ; BUFO(NO) = '#' 
      NO = NO + 1 ; BUFO(NO) = '# Transformed from eob fromat using eob_to_erp utility' 
      NO = NO + 1 ; BUFO(NO) = '# eob_to_erp '//TRIM(FIL_EOB)//' '//TRIM(FIL_ERP)
      NO = NO + 1 ; BUFO(NO) = '#' 
!
      DO 410 J1=1,NHEAD
         NO = NO + 1
         BUFO(NO) = HEAD_BUF(J1)
 410  CONTINUE 
!
      DO 420 J2=1,NP
         NO = NO + 1
         WRITE ( UNIT=BUFO(NO), FMT=120 ) 2400000.5D0 + EOP(J2)%MJD_EOP, &
     &                                    0.01D0*EOP(J2)%XPL_V*RAD__TO__MAS, &
     &                                    0.01D0*EOP(J2)%YPL_V*RAD__TO__MAS, &
     &                                    NINT(1.0D6*EOP(J2)%U1_V*RAD__TO__SEC)
 120     FORMAT ( F10.2, F7.4, 1X, F7.4, I10, ' 0.0    0.0         0. 0.000 0.000 0.000' )
         IF ( BUFO(NO)(10:10) == '0' ) BUFO(NO)(10:10) = ' '
 420  CONTINUE 
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FIL_ERP, STATUS='unknown', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3802, IUER, 'EOB_TO_ERP', 'Error in '// &
     &         'opening the ouput ERP file '//TRIM(FIL_ERP) )
           CALL EXIT ( 1 )
      END IF
      DO 430 J3=1,NO
         WRITE ( UNIT=LUN, FMT='(A)' ) BUFO(J3)
 430  CONTINUE 
      CLOSE ( UNIT=LUN )
!
      END  PROGRAM  EOB_TO_ERP  !#!#
