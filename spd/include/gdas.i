!
! >>>>> Include block that defines failds of the data structure of
! >>>>> the file with atmospheric data
! >>>>>
! >>>>> 2008.03.14 (c)  L. Petrov  v 1.0  2008.03.14_08:25:05
!
      TYPE       GDAS__LABEL_TYPE
         CHARACTER YEAR*2
         CHARACTER MON*2
         CHARACTER DAY*2
         CHARACTER HOUR*2
         CHARACTER FORECAST*2
         CHARACTER LEV_IND*2
         CHARACTER GRID_ID*2
         CHARACTER VAR_NAME*4
         CHARACTER NEXP*4
         CHARACTER PREC*14
         CHARACTER VAR1*14
      END TYPE   GDAS__LABEL_TYPE
!
      TYPE       GDAS__HEADER_TYPE
         CHARACTER  MODEL*4
         CHARACTER  ICX*3
         CHARACTER  MN*2
         CHARACTER  POLE_LAT*7
         CHARACTER  POLE_LON*7
         CHARACTER  REF_LAT*7
         CHARACTER  REF_LON*7
         CHARACTER  SIZE*7
         CHARACTER  ORIENT*7
         CHARACTER  TANG_LAT*7
         CHARACTER  SYNC_XP*7
         CHARACTER  SYNC_YP*7
         CHARACTER  SYNC_LAT*7
         CHARACTER  SYNC_LON*7
         CHARACTER  DUMMY*7
         CHARACTER  NX*3
         CHARACTER  NY*3
         CHARACTER  NZ*3
         CHARACTER  K_FLAG*2
         CHARACTER  LENH*4
      END TYPE   GDAS__HEADER_TYPE
!
      TYPE       GDAS__IND_ELM_TYPE
         CHARACTER, POINTER :: NAME(:)*4
         INTEGER*4, POINTER :: CHECK_SUM(:)
      END TYPE   GDAS__IND_ELM_TYPE
!
      TYPE       GDAS__INDEX_TYPE
         INTEGER*4  N_LEV
         REAL*8,    POINTER :: GPH_HEI_LEV(:)
         INTEGER*4, POINTER :: N_VAR(:)
         TYPE ( GDAS__IND_ELM_TYPE ), POINTER :: VAR(:)
      END TYPE       GDAS__INDEX_TYPE
!
      TYPE       GDAS__RECORD_TYPE
         TYPE ( GDAS__LABEL_TYPE  ) :: LABEL
         CHARACTER*1, POINTER       :: CPACK(:)
         REAL*4,      POINTER       :: DATA(:,:)
      END TYPE   GDAS__RECORD_TYPE
!
! >>>>> End of include block for package GDAS
