      SUBROUTINE VTD_INIT ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_INIT  initializes object VTD. It should be called      *
! *   before first used of package VLBI Time Delay (VTD).                *
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
! *  ### 26-JAN-2004    VTD_INIT   v1.2 (c)  L. Petrov  15-NOV-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  LEN_VTD
!      
#ifdef SUN
!
! --- An attempt to circumvent a bug in Sun compiler
!
      LEN_VTD = LOC(VTD%LAST_FIELD) - LOC(VTD) + SIZEOF(VTD%LAST_FIELD) 
#else
      LEN_VTD = SIZEOF ( VTD ) 
#endif
      CALL NOUT ( LEN_VTD, VTD )
!
      CALL NERS_QUIT ( NERS__ALL, VTD%NERS )
      VTD%STATUS = VTD__INIT
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_INIT
!
! ------------------------------------------------------------------------
!
      FUNCTION VTD_SIZE()
! ************************************************************************
! *                                                                      *
! *   Returns the size of VTD in bytes.                                  *
! *                                                                      *
! *  ### 14-JUN-2010    VTD_SIZE   v1.0 (c)  L. Petrov  14-JUN-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE 
      INCLUDE 'vtd.i'
      TYPE    ( VTD__TYPE  ), POINTER :: VTD
      INTEGER*4  VTD_SIZE
      VTD_SIZE = SIZEOF(VTD)
      RETURN
      END  FUNCTION VTD_SIZE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VTD_GET_VERSION ( STR )
! ************************************************************************
! *                                                                      *
! *   Returns the VTD label with version.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       STR ( CHARACTER ) -- Version name of the VTD.                  *
! *                                                                      *
! *  ### 14-JUN-2010    VTD_SIZE   v1.0 (c)  L. Petrov  14-JUN-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE 
      INCLUDE 'vtd.i'
      CHARACTER  STR*(*)
      INTEGER*4  IL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      STR = VTD__LABEL
      IL = ILEN(STR)
      IF ( IL < LEN(STR) ) THEN
           STR(IL+1:IL+1) = CHAR(0)
      END IF
      RETURN
      END  SUBROUTINE  VTD_GET_VERSION  !#!#
