!
      SUBROUTINE GMET ( METRIC_MAP )
!
! --- program for transferring METRIC_MAP variable to matric_new element
! --- of a common block flyby.i
!
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE 'bindisp.i'
      INCLUDE   'flyby.i'
      CHARACTER  METRIC_MAP*(*)
!
      METRIC_NEW = METRIC_MAP
!
      RETURN
      END  !#!  GMET  #!#
