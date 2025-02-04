!
! >>>>> INCLUDE-BLOCK with description of data structures used by
!       Phase_doctor
!
!       delay_transfer.i 11-DEC-2001 v 1.0 Leonid Petrov  11-DEC-2001 13:54:37
!
      TYPE      DT__STRU
          REAL*8     GRDEL_X
          REAL*8     GRDEL_S
!
          REAL*8     GRSIG_X
          REAL*8     GRSIG_S
!
          REAL*8     GRDAMBSP_X
          REAL*8     GRDAMBSP_S
          REAL*8     UTC_TAG
          INTEGER*4  MJD
!
          INTEGER*2  NAMBSP_X
          INTEGER*2  NAMBSP_S
!
          INTEGER*2  UACSUP
          INTEGER*2  QC_X
          INTEGER*2  QC_S
          CHARACTER  SOU_NAME*8
          CHARACTER  BAS_NAME*16
          INTEGER*2  LAST_FIELD
      END TYPE  DT__STRU ! DT__STRU !
!
! <<<<< end of INCLUDE-BLOCK  delay_transfer.i
!
