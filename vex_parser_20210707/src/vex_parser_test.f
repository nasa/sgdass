      PROGRAM    VEX_PARSER_TEST
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      INTEGER*4  IUER, ISDR
      INTEGER*4  N_STA, N_SOU, N_SCA
      CHARACTER  FIL_VEX*128
      TYPE ( VEX_TYPE ) :: VEX
!
!***!      FIL_VEX = '/vlbi/aua052/aua052.vex' 
      FIL_VEX = '/home/nhabana/data/aua052.vex'

!****!      FIL_VEX = '/s0/temp/vex/apsg44.vex'  ! $PROCEDURES is $PROCS & parameters there missing
!*****!      FIL_VEX = '/s0/temp/vex/bl273ag.vex'      ! iau missing
!*****!      FIL_VEX = '/s0/temp/vex/bp242a.vex'
!*****!      FIL_VEX = '/s0/temp/vex/es087w.vex'
!****!      FIL_VEX = '/s0/temp/vex/rd1909.vex'
!****!      FIL_VEX = '/s0/temp/vex/v515c.vex'

      VEX%STATUS = VEX__UNDF
! 
      CALL VEX_COUNT ( N_STA, N_SOU, N_SCA, FIL_VEX, IUER )

      WRITE (6,*) '______________________________________'
      WRITE (6,*) 'We counted ', N_STA, ' Stations '
      WRITE (6,*) '           ', N_SOU, ' Sources'
      WRITE (6,*) '           ', N_SCA, ' Scans'
      WRITE (6,*) '______________________________________' 


      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  GETTING VEX        |'
      WRITE (6,*) '_______________________'

      IUER = -1
      ISDR = 0
      CALL VEX_PARSER ( VEX, FIL_VEX, ISDR, IUER )

      WRITE (6,*) '_______________________'
      WRITE (6,*) '|   WE HAVE VEX       |'
      WRITE (6,*) '_______________________'
 
      WRITE (6,*) ' STAND ALONE'
      WRITE (6,*) ' '
      WRITE (6,*) 'STATUS:         ', VEX%STATUS
      WRITE (6,*) 'REVISION:       ', VEX%REVISION   ! Commented out
      WRITE (6,*) 'EXPER_NAME      ', VEX%EXPER_NAME 
      WRITE (6,*) 'N_STA in VEX    ', VEX%N_STA
      WRITE (6,*) 'N_SOU in VEX    ', VEX%N_SOU
      WRITE (6,*) 'N_SCA in VEX    ', VEX%N_SCA
      WRITE (6,*) ' '
      WRITE (6,*) ' FROM SOURCE (1 e.g.) '
      WRITE (6,*) 'NAME            ', VEX%SOU(25)%NAME
      WRITE (6,*) 'IAU_NAME        ', VEX%SOU(25)%IAU_NAME
      WRITE (6,*) 'RA              ', VEX%SOU(25)%RA    
      WRITE (6,*) 'DEC             ', VEX%SOU(25)%DEC 
      WRITE (6,*) ' '
      WRITE (6,*) ' FROM SCHEDULE (1 e.g.) '
      WRITE (6,*) 'MODE            ', VEX%SCA(123)%MODE
      WRITE (6,*) 'SOU_NAME        ', VEX%SCA(123)%SOU_NAME
      WRITE (6,*) 'IND_SOU         ', VEX%SCA(123)%IND_SOU
      WRITE (6,*) 'MJD             ', VEX%SCA(123)%MJD
      WRITE (6,*) 'UTC             ', VEX%SCA(123)%UTC
      WRITE (6,*) 'N_STA in SCA    ', VEX%SCA(123)%N_STA
      WRITE (6,*) 'IND_STA         ', VEX%SCA(123)%IND_STA(5)
      WRITE (6,*) 'START OFFSET    ', VEX%SCA(123)%START_OFFSET(5)
      WRITE (6,*) 'SCAN DUR        ', VEX%SCA(123)%SCAN_DUR(5)
      WRITE (6,*) 'WRAP MODE       ', VEX%SCA(123)%WRAP_MODE(5)
      WRITE (6,*) '  '  
      WRITE (6,*) ' FROM STATION (1 e.g.) '
      WRITE (6,*) 'SITE_NAME       ', VEX%STA(7)%SITE_NAME
      WRITE (6,*) 'ANT_NAME        ', VEX%STA(7)%ANT_NAME
      WRITE (6,*) 'SITE_ID         ', VEX%STA(7)%SITE_ID
      WRITE (6,*) 'PROC_NAME_PREF  ', VEX%STA(1)%PROCEDURE_NAME_PREFIX
      WRITE (6,*) 'TAPE_CHANGE     ', VEX%STA(1)%TAPE_CHANGE
      WRITE (6,*) 'HEADSTACK       ', VEX%STA(1)%HEADSTACK_MOTION
      WRITE (6,*) 'NEW_SOU_COM     ', VEX%STA(1)%NEW_SOURCE_COMMAND
      WRITE (6,*) 'NEW TAPE SET    ', VEX%STA(1)%NEW_TAPE_SETUP
      WRITE (6,*) 'SET_ALWAYS      ', VEX%STA(1)%SETUP_ALWAYS
      WRITE (6,*) 'PARITY          ', VEX%STA(1)%PARITY_CHECK
      WRITE (6,*) 'PREPASS         ', VEX%STA(1)%TAPE_PREPASS
      WRITE (6,*) 'PREOB           ', VEX%STA(1)%PREOB_CAL
      WRITE (6,*) 'MIDOB           ', VEX%STA(1)%MIDOB_CAL        
      WRITE (6,*) 'POSTOB          ', VEX%STA(1)%POSTOB_CAL
      WRITE (6,*) ' '
      WRITE (6,*) '_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+'

 
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END  PROGRAM    VEX_PARSER_TEST  !#!#
      
