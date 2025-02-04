      FUNCTION   UNIX_DATE_TO_DATE ( UNIX_DATE )
! ************************************************************************
! *                                                                      *
! *   Function UNIX_DATE_TO_DATE  transforms the date in the internal    *
! *   Unix format time_t to Solve format YYYY.MM.DD_hh:mm:ss             *
! *                                                                      *
! * ## 18-OCT-2004 UNIX_DATE_TO_DATE v1.0 (c)  L. Petrov  18-OCT-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  UNIX_DATE_TO_DATE*19
      CHARACTER  DAT*24, MON_ENG(12)*3, MON_NUM(12)*2
      INTEGER*4  UNIX_DATE
      INTEGER*4  NN, IND_MON
      DATA       ( MON_ENG(NN), MON_NUM(NN), NN=1,12 ) &
     &           / &
     &             'Jan', '01', & ! 
     &             'Feb', '02', & ! 
     &             'Mar', '03', & ! 
     &             'Apr', '04', & ! 
     &             'May', '05', & ! 
     &             'Jun', '06', & ! 
     &             'Jul', '07', & ! 
     &             'Aug', '08', & ! 
     &             'Sep', '09', & ! 
     &             'Oct', '10', & ! 
     &             'Nov', '11', & ! 
     &             'Dec', '12'  & ! 
     &           / 
      INTEGER*4  LTM_DIF
      CALL CTIME_R ( UNIX_DATE, DAT  ) 
      IND_MON  = LTM_DIF ( 1, 12, MON_ENG, DAT(5:7) )
      UNIX_DATE_TO_DATE = DAT(21:24)//'.'//MON_NUM(IND_MON)//'.'// &
     &                    DAT(9:10)//'_'//DAT(12:19)
      CALL BLANK_TO_ZERO ( UNIX_DATE_TO_DATE )
      RETURN
      END  FUNCTION  UNIX_DATE_TO_DATE
