      FUNCTION       ADDRESS_ALIGN ( ADR, ALIGNMENT )
! ************************************************************************
! *                                                                      *
! *   Function  ADDRESSS_ALIGN alignes adress in such a way that         *
! *   the new address is no less than ADR but is a multiple oa ALIGNMENT.*
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        ADR ( ADDRESS__TYPE ) -- input address.                       *
! *  ALIGNMENT ( INTEGER*4     ) -- alignment.                           *
! *                                                                      *
! * _________________________ Ouutput parameters: ______________________ *
! *                                                                      *
! * <ADDRESS_ALIGN> ( ADDRESS__TYPE ) -- alignmed address in the range   *
! *                                      [ADR,ADR+ALIGNMENT-1] which     *
! *                                      is a multiple of ALIGNMENT.     *
! *                                                                      *
! * ### 02-NOV-2005  ADDRESS_ALIGN  v1.0 (c)  L. Petrov  02-NOV-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT       NONE 
      ADDRESS__TYPE  ADDRESS_ALIGN
      ADDRESS__TYPE  ADR
      INTEGER*4      ALIGNMENT
      ADDRESS__TYPE  ADR_ROUND
!
      ADR_ROUND = (ADR/ALIGNMENT)*ALIGNMENT
      IF ( ADR_ROUND == ADR ) THEN
           ADDRESS_ALIGN = ADR
         ELSE IF ( ADR_ROUND > ADR ) THEN
           ADDRESS_ALIGN = ADR_ROUND 
         ELSE 
           ADDRESS_ALIGN = ADR_ROUND + ALIGNMENT
      END IF
      RETURN
      END  FUNCTION  ADDRESS_ALIGN  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   INT4_ALIGN ( INT4_VAL, ALIGNMENT )
! ************************************************************************
! *                                                                      *
! *   INTEGER*4 version of ADDRESS_ALIGN routine.                        *
! *                                                                      *
! *  ### 02-NOV-2005   INT4_ALIGN  v1.0 (c)  L. Petrov  02-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INT4_ALIGN
      INTEGER*4  INT4_VAL
      INTEGER*4  ALIGNMENT
      INTEGER*4  INT4_VAL_ROUND
!
      INT4_VAL_ROUND = (INT4_VAL/ALIGNMENT)*ALIGNMENT
      IF ( INT4_VAL_ROUND == INT4_VAL ) THEN
           INT4_ALIGN = INT4_VAL
         ELSE IF ( INT4_VAL_ROUND > INT4_VAL ) THEN
           INT4_ALIGN = INT4_VAL_ROUND
         ELSE
           INT4_ALIGN = INT4_VAL_ROUND + ALIGNMENT
      END IF
      RETURN
      END  FUNCTION  INT4_ALIGN  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   INT8_ALIGN ( INT8_VAL, ALIGNMENT )
! ************************************************************************
! *                                                                      *
! *   INTEGER*8 version of ADDRESS_ALIGN routine.                        *
! *                                                                      *
! *  ### 18-JAN-2011   INT8_ALIGN  v1.0 (c)  L. Petrov  18-JAN-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  INT8_ALIGN
      INTEGER*8  INT8_VAL
      INTEGER*4  ALIGNMENT
      INTEGER*8  INT8_VAL_ROUND
!
      INT8_VAL_ROUND = (INT8_VAL/ALIGNMENT)*ALIGNMENT
      IF ( INT8_VAL_ROUND == INT8_VAL ) THEN
           INT8_ALIGN = INT8_VAL
         ELSE IF ( INT8_VAL_ROUND > INT8_VAL ) THEN
           INT8_ALIGN = INT8_VAL_ROUND
         ELSE
           INT8_ALIGN = INT8_VAL_ROUND + ALIGNMENT
      END IF
      RETURN
      END  FUNCTION  INT8_ALIGN  !#!#
