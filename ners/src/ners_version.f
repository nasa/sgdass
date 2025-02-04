      SUBROUTINE NERS_VERSION ( INQ, ANS )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_VERSION receviers an inquire for a given label        *
! *   defined in its internal structure and returns a string with        *
! *   answer.                                                            *
! *                                                                      *
! *  ### 04-JAN-2018 NERS_VERSION  v1.0 (c)  L. Petrov  15-AUG-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      CHARACTER  INQ*(*), ANS*(*)
      IF ( INQ == 'NERS__VERSION' ) THEN
           ANS = NERS__VERSION
         ELSE IF ( INQ == 'NERS__LABEL' ) THEN
           ANS = NERS__LABEL
         ELSE 
           ANS = 'Unknown inquire: '//INQ
      END IF
      RETURN
      END  SUBROUTINE NERS_VERSION  !#!#
