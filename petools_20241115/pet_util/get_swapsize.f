      FUNCTION GET_SWAPSIZE ()
! ************************************************************************
! *                                                                      *
! *   Routine GET_SWAPSIZE returns size of swapping area in bytes.       *
! *   It works only under Linux. If it cannot get swap area size,        *
! *   it returns -1.                                                     *
! *                                                                      *
! * ---------------------- Output parameters: -------------------------- *
! *                                                                      *
! * <GET_SWAPSIZE> ( INTEGER*8 ) -- size of swapping area.               *
! *                                                                      *
! *  ### 12-DEC-2006  GET_SWAPSIZE  v1.0 (c)  L. Petrov  12-DEC-2006 ### *
! *                                                                      *
! ************************************************************************
      INTEGER*8 GET_SWAPSIZE
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
           GET_SWAPSIZE = 1
           RETURN 
      END IF
!
! --- Read the special file
!
      DO 410 J1=1,128
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .NE. 0 ) THEN
              CLOSE ( UNIT=LUN )
              GET_SWAPSIZE = 1
              RETURN 
         END IF
         IF ( STR(1:10) == 'SwapTotal:' ) THEN
!
! ----------- Found relevant entry
!
              STR(1:10) =  '          '
              CALL CHASHL ( STR )
              IP = INDEX ( STR, 'kB' ) 
              IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
!
! ----------- Decode it
!
              CALL CHIN   ( STR, SIZE_I4 )
              GET_SWAPSIZE = INT8(SIZE_I4)*1024
              CLOSE ( UNIT=LUN )
              RETURN 
         END IF
 410  CONTINUE 
      CLOSE ( UNIT=LUN )
!
      GET_SWAPSIZE = 1
      RETURN 
      END  FUNCTION   GET_SWAPSIZE  !#!#
