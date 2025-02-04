      FUNCTION STP_COMPAR_TSYS ( TSYS1, TSYS2 )
!
! ***************************************************************************
! *                                                                         *
! *    Auxiliary routine STP_COMPAR_TSYS is used to compare two system      *
! *    temperature data structurs for sorting.                              *
! *                                                                         *
! *    See also PIMA_COMPAR_SCA in the PIMA library package                 *
! *                                                                         *
! *  ### 03-AUG-2020  STP_COMPAR_TSYS  v1.0 (c)  N. Habana  03-AUG-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'stp.i'  
      TYPE ( TSYS_STP__TYPE ) :: TSYS1, TSYS2
#ifdef GNU
      INTEGER*4   STP_COMPAR_TSYS
#else
      INTEGER*2   STP_COMPAR_TSYS
#endif
!
      IF ( TSYS1%MJD_RANGE(1) > TSYS2%MJD_RANGE(1) ) THEN
           STP_COMPAR_TSYS  =  1
      ELSE IF ( TSYS1%MJD_RANGE(1) < TSYS2%MJD_RANGE(1) ) THEN
           STP_COMPAR_TSYS  = -1
      ELSE
           IF ( TSYS1%TAI_RANGE(1) > TSYS2%TAI_RANGE(1) ) THEN
                STP_COMPAR_TSYS  =  1
           ELSE IF ( TSYS1%TAI_RANGE(1) < TSYS2%TAI_RANGE(1) ) THEN
                STP_COMPAR_TSYS  = -1
           ELSE
                IF ( TSYS1%FRQ_RANGE(1) > TSYS2%FRQ_RANGE(1) ) THEN
                     STP_COMPAR_TSYS  =  1
                ELSE IF ( TSYS1%FRQ_RANGE(1) < TSYS2%FRQ_RANGE(1) ) THEN
                     STP_COMPAR_TSYS  = -1
                ELSE
                     STP_COMPAR_TSYS  = 0
                END IF
           END IF
      END IF

      RETURN
      END FUNCTION  STP_COMPAR_TSYS
!
! --------------------------------------------------------------------------
!
      FUNCTION STP_COMPAR_GAIN ( GAIN1, GAIN2 )
!
! ***************************************************************************
! *                                                                         *
! *    Auxiliary routine STP_COMPAR_GAIN is used to compare two system      *
! *    temperature data structurs for sorting.                              *
! *                                                                         *
! *    See also PIMA_COMPAR_SCA in the PIMA library package                 *
! *                                                                         *
! *  ### 03-AUG-2020  STP_COMPAR_GAIN  v1.0 (c)  N. Habana  03-AUG-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'stp.i'  
      TYPE ( GAIN_STP__TYPE ) :: GAIN1, GAIN2
#ifdef GNU
      INTEGER*4   STP_COMPAR_GAIN
#else
      INTEGER*2   STP_COMPAR_GAIN
#endif
!
      IF ( GAIN1%MJD_RANGE(1) > GAIN2%MJD_RANGE(1) ) THEN
           STP_COMPAR_GAIN  =  1
      ELSE IF ( GAIN1%MJD_RANGE(1) < GAIN2%MJD_RANGE(1) ) THEN
           STP_COMPAR_GAIN  = -1
      ELSE
           IF ( GAIN1%TAI_RANGE(1) > GAIN2%TAI_RANGE(1) ) THEN
                STP_COMPAR_GAIN  =  1
           ELSE IF ( GAIN1%TAI_RANGE(1) < GAIN2%TAI_RANGE(1) ) THEN
                STP_COMPAR_GAIN  = -1
           ELSE
                IF ( GAIN1%FRQ_RANGE(1) > GAIN2%FRQ_RANGE(1) ) THEN
                     STP_COMPAR_GAIN  =  1
                ELSE IF ( GAIN1%FRQ_RANGE(1) < GAIN2%FRQ_RANGE(1) ) THEN
                     STP_COMPAR_GAIN  = -1
                ELSE
                     STP_COMPAR_GAIN  =  0
                END IF
           END IF
      END IF
!
      RETURN
      END FUNCTION  STP_COMPAR_GAIN
!
