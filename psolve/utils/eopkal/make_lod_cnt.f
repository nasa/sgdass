      SUBROUTINE MAKE_LOD_CNT()
      IMPLICIT   NONE                  ! Added by IMP/jwr
      INTEGER*4  IOS, IP, IU
      CHARACTER  PID_STR*8, TMP_FIL*64
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      INTEGER*4  GETPID, I_LEN
!
!  pet  2004.10.28  Changed file names: added PID and username
!
!
! --- make an LOD control file on /tmp
!
      IP = GETPID()
      CALL INCH      ( IP, PID_STR )
      CALL CHASHR    (     PID_STR ) 
      CALL BLANK_TO_ZERO ( PID_STR )
      CALL GETINFO_USER  ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IU = I_LEN(USER_NAME)
      IF ( IU .EQ. 1 ) USER_NAME = '1'
!
      TMP_FIL = '/tmp/eop_kal_'//USER_NAME(1:IU)//'_'//PID_STR//'.cnt'
!
      OPEN ( 80, FILE=TMP_FIL, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' OPEN:  IOS=',IOS
           CALL ERR_LOG ( 3431, -1, 'MAKE_LOD_CNT', 'EOPKAL (make_lod_cnt) '// &
     &         'error in opening file '//TMP_FIL ) 
           STOP 'EOPKAL(make_lod_cnt ) Abnormal termination'
      END IF
!
      WRITE ( 80, *, IOSTAT=IOS ) "begin"
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' OPEN:  IOS=',IOS
           CALL ERR_LOG ( 3432, -1, 'MAKE_LOD_CNT', 'EOPKAL (make_lod_cnt) '// &
     &         'error in writing into file '//TMP_FIL )
           STOP 'EOPKAL(make_lod_cnt ) Abnormal termination'
      END IF
      write(80,*) "headers 2 "
      write(80,*) "line 1"
      write(80,*) "point -1"
      write(80,*) "y_field 1 10  0  Excess LOD (microseconds)"
      write(80,*) "x_field 0 1 3 Year"
      write(80,*) "charsz 1.5"
      write(80,*) &
     &   "label .175 .95 1  0 LOD Determined from VLBI Data"
      write(80,*) "view 0.1 0.9 0.1 0.9"
      write(80,*) "window 80 1 1 99 12 31 0 3300"
      write(80,*) "axes ABCNST ABCNST"
      write(80,*) "file eop_kal.plt"
      write(80,*) "read"
      write(80,*) "charsz 0.8"
      write(80,*) "draw"
      write(80,*) "end"
      close(80)
      RETURN
      END  !#!  MAKE_LOD_CNT  #!#
