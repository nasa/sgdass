      FUNCTION GET_MEMSIZE ()
! ************************************************************************
! *                                                                      *
! *   Routine GET_MEMSIZE returns size of operative memory in bytes.     *
! *   It works only under Linux. IF it cannot get memory size,           *
! *   it returns -1.                                                     *
! *                                                                      *
! * ---------------------- Output parameters: -------------------------- *
! *                                                                      *
! * <GET_MEMSIZE> ( INTEGER*8 ) -- size of operative memory.             *
! *                                                                      *
! *  ### 12-DEC-2006  GET_MEMSIZE  v1.0 (c)  L. Petrov  12-DEC-2006 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*8  GET_MEMSIZE
      CHARACTER  FIL*128, STR*128
      INTEGER*4, EXTERNAL  :: GET_UNIT, I_LEN, ILEN
      INTEGER*4  LUN, IOS, J1, SIZE_I4, IP
!
! --- Open special file 
!
      FIL = '/proc/meminfo'
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FIL, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           GET_MEMSIZE = 1
           RETURN 
      END IF
!
! --- Read the special file
!
      DO 410 J1=1,128
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .NE. 0 ) THEN
              CLOSE ( UNIT=LUN )
              GET_MEMSIZE = 1
              RETURN 
         END IF
!
         IF ( STR(1:9) == 'MemTotal:' ) THEN
!
! ----------- Found relevant entry
!
              STR(1:9) =  '         '
              CALL CHASHL ( STR )
              IP = INDEX ( STR, 'kB' ) 
              IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
!
! ----------- Decode it
!
              CALL CHIN   ( STR, SIZE_I4 )
              GET_MEMSIZE = INT8(SIZE_I4)*INT8(1024)
              CLOSE ( UNIT=LUN )
              RETURN 
         END IF
 410  CONTINUE 
      CLOSE ( UNIT=LUN )
!
      GET_MEMSIZE = 1
      RETURN 
      END  FUNCTION   GET_MEMSIZE  !#!#
