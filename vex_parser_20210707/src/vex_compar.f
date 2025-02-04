      FUNCTION VEX_COMPAR_SCA ( SCA1, SCA2 )
!
! ***************************************************************************
! *                                                                         *
! *    Auxiliary routine VEX_COMPAR_SCA is used to compare two schedule     *
! *    data structurs for sorting.                                          *
! *                                                                         *
! *    See also PIMA_COMPAR_SCA in the PIMA library package                 *
! *                                                                         *
! *  ### 17-JUL-2020  VEX_COMPAR_SCA   v1.0 (c)  N. Habana  17-JUL-2020 ### *
! *  ### 20-JUL-2020  VEX_COMPAR_SCA   v1.1 (c)  N. Habana  20-JUL-2020 ### *
! *    - Eliminated the use of MJD and now just comparing based on UTC.     *
! *      We are assuming that each schedule list we compare will always     *
! *      occur on the same MJD. This is not particularly true and is a      *
! *      temporary fix. Will need to get back to this and address it        *
! *      eventually, for now it works.                                      *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'vex.i'
      TYPE  ( VEX__SCA_TYPE ) :: SCA1, SCA2
#ifdef GNU
      INTEGER*4   VEX_COMPAR_SCA
#else
      INTEGER*2   VEX_COMPAR_SCA
#endif
!
! --- The MJD of SCA1 comes after SCA2
!
      IF ( SCA1%MJD > SCA2%MJD ) THEN
           VEX_COMPAR_SCA =  1
!
! --- The MJD of SCA1 comes after SCA2
!
      ELSE IF ( SCA1%MJD < SCA2%MJD ) THEN
           VEX_COMPAR_SCA = -1
!
! --- The schedules occur at the same MJD
!
      ELSE
!
! -------- The UTC of SCA1 comes after SCA2
!
           IF ( SCA1%UTC > SCA2%UTC ) THEN
                VEX_COMPAR_SCA =  1
!
! -------- The UTC of SCA1 comes after SCA2
!
           ELSE IF ( SCA1%UTC < SCA2%UTC ) THEN
                VEX_COMPAR_SCA = -1
!
! -------- The schedules occur at the same time
!
           ELSE
                VEX_COMPAR_SCA =  0
           END IF
      END IF
!
      RETURN
      END  FUNCTION  VEX_COMPAR_SCA  !#!
!
! ----------------------------------------------------------
!
      FUNCTION VEX_COMPAR_SOU ( SOU1, SOU2 )
!
! ***************************************************************************
! *                                                                         *
! *    Auxiliary routine VEX_COMPAR_SOU is used to compare two vex source   *
! *    data structures for sorting alphabetically.                          *
! *                                                                         *
! *    See also PIMA_COMPAR_SCA in the PIMA library package                 *
! *                                                                         *
! *  ### 20-NOV-2020  VEX_COMPAR_SOU   v1.0 (c)  N. Habana  20-NOV-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'vex.i'
      TYPE  ( VEX__SOU_TYPE ) :: SOU1, SOU2
#ifdef GNU
      INTEGER*4   VEX_COMPAR_SOU
#else
      INTEGER*2   VEX_COMPAR_SOU
#endif
!
! --- The Source name of SOU1 comes after SOU2
!
      IF ( SOU1%NAME > SOU2%NAME ) THEN
           VEX_COMPAR_SOU =  1
!
! --- The Source name of SOU1 comes before SOU2
!
      ELSE IF ( SOU1%NAME < SOU2%NAME ) THEN
           VEX_COMPAR_SOU = -1
!
! --- The Sources have the same name
!
      ELSE
           VEX_COMPAR_SOU =  0
      END IF
!
      RETURN
      END  FUNCTION  VEX_COMPAR_SOU  !#!#!
!
! ----------------------------------------------------------
!
      FUNCTION VEX_COMPAR_STA ( STA1, STA2 )
!
! ***************************************************************************
! *                                                                         *
! *    Auxiliary routine VEX_COMPAR_STA is used to compare two vex stations *
! *    data structures for sorting alphabetically.                          *
! *                                                                         *
! *    See also PIMA_COMPAR_SCA in the PIMA library package                 *
! *                                                                         *
! *  ### 03-DEC-2020  VEX_COMPAR_STA   v1.0 (c)  N. Habana  03-DEC-2020 ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'vex.i'
      TYPE  ( VEX__STA_TYPE ) :: STA1, STA2
#ifdef GNU
      INTEGER*4   VEX_COMPAR_STA
#else
      INTEGER*2   VEX_COMPAR_STA
#endif
!
! --- The site name of STA1 comes after STA2
!
      IF ( STA1%SITE_NAME > STA2%SITE_NAME ) THEN
           VEX_COMPAR_STA =  1
!
! --- The site name of STA1 comes before STA2
!
      ELSE IF ( STA1%SITE_NAME < STA2%SITE_NAME ) THEN
           VEX_COMPAR_STA = -1
!
! --- The Stations have the same name
!
      ELSE
           VEX_COMPAR_STA =  0
      END IF
!
      RETURN
      END  FUNCTION  VEX_COMPAR_STA  !#!#!#!

