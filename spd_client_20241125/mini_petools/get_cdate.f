      FUNCTION   GET_CDATE ( )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_CDATE learns the current system time and transofrms   *
! *   it to internal SOLVE format:                                       *
! *   yyyy.mm.dd-hh:mm:ss  like                                          *
! *   1999.09.02-20:54:45                                                *
! *                                                                      *
! *   The length of the line is 19 symbols.                              *
! *                                                                      *
! * ###  06-SEP-1999   GET_CDATE   v2.0  (c) L. Petrov  21-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  GET_CDATE*19
      CHARACTER  STR*32
      INTEGER*4  TIM_VAL(8)
!
      CALL DATE_AND_TIME ( VALUES=TIM_VAL )
!
      WRITE ( UNIT=GET_CDATE, FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":",I2)' ) &
     &        TIM_VAL(1), TIM_VAL(2), TIM_VAL(3), TIM_VAL(5), TIM_VAL(6), &
     &        TIM_VAL(7)
!
      CALL BLANK_TO_ZERO ( GET_CDATE )
!
      RETURN
      END  !#!  GET_CDATE  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_CDATE_MS ( )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_CDATE_MS learns the current system time and           *
! *   transofrms it to internal SOLVE format:                            *
! *   yyyy.mm.dd-hh:mm:ss.ss  like                                       *
! *   1999.09.02-20:54:27.86                                             *
! *                                                                      *
! *   The length of the line is 23 symbols.                              *
! *                                                                      *
! * ###  06-SEP-1999  GET_CDATE_MS  v2.1 (c) L. Petrov  19-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  GET_CDATE_MS*23
      INTEGER*4  TIM_VAL(8)
      CALL DATE_AND_TIME ( VALUES=TIM_VAL )
!
      WRITE ( UNIT=GET_CDATE_MS, &
     &       FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":",I2,".",I3)' ) &
     &        TIM_VAL(1), TIM_VAL(2), TIM_VAL(3), TIM_VAL(5), TIM_VAL(6), &
     &        TIM_VAL(7), TIM_VAL(8)
      CALL BLANK_TO_ZERO ( GET_CDATE_MS )
!
      RETURN
      END  !#!  GET_CDATE_MS  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_HR_CDATE ( )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_CDATE learns the current system time with high        *
! *   accuracy and transforms it in yyyy.mm.dd-hh:mm:ss.nnnnnnnnn 
! *                                                                      *
! *   The length of the line is 29 symbols.                              *
! *                                                                      *
! * ###  31-AUG-2014  GET_HR_CDATE  v2.0 (c) L. Petrov  31-AUG-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  GET_HR_CDATE*29
      CHARACTER  STR*29
      INTEGER*4  TIM_VAL(7)
!
      CALL GET_HR_TIME ( TIM_VAL )
!
      WRITE ( UNIT=STR, FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":",I2,".",I9)' ) &
     &        TIM_VAL
!
      CALL BLANK_TO_ZERO ( STR )
      GET_HR_CDATE = STR
!
      RETURN
      END  FUNCTION  GET_HR_CDATE  !#!#
