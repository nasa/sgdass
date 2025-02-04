      SUBROUTINE VTD_VERSION ( INQ, ANS )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_VERSION receviers an inquire for a given label         *
! *   defined in its internal structure and returns a string with        *
! *   answer.                                                            *
! *                                                                      *
! *  ### 15-AUG-2013  VTD_VERSION  v1.0 (c)  L. Petrov  15-AUG-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      CHARACTER  INQ*(*), ANS*(*)
      IF ( INQ == 'VTD__LABEL' ) THEN
           ANS = VTD__LABEL 
         ELSE IF ( INQ == 'BSPPOS__LABEL' ) THEN
           ANS = BSPPOS__LABEL
         ELSE IF ( INQ == 'SOU_MAP__LABEL' ) THEN
           ANS = SOU_MAP__LABEL
         ELSE IF ( INQ == 'VIONO__LABEL' ) THEN
           ANS = VIONO__LABEL
         ELSE IF ( INQ == 'SPD_3D_PROG__LABEL' ) THEN
           ANS = SPD_3D_PROG__LABEL
         ELSE IF ( INQ == 'DE440_EPH__LABEL' ) THEN
           ANS = DE440_EPH__LABEL
         ELSE 
           ANS = 'Unknown inquire: '//INQ
      END IF
      RETURN
      END  SUBROUTINE VTD_VERSION  !#!  
