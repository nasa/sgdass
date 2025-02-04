!
!  modified:
!  JMG 960730  Made so that size of parm_fill is set automatically.
!  pet 990106  Renamed the common block since its old names coincided with
!              the name of subroutine PARMS
!  pet 2017.10.23 made PARM_NUM  INTEGER*4  
!
      INTEGER*4     PARM_NUM
      INTEGER*2     PARM_NUM_I2,              &
     &              PARM_NAMES(10,M_GPA),     &
     &              PARM_FILL(JPLIST_FILL),   &
     &              PARM_LAST_FIELD 
      CHARACTER*20  CPARM_NAMES(M_GPA)
      EQUIVALENCE ( PARM_NAMES,CPARM_NAMES)
      COMMON / PARMS_COMMON / PARM_NUM_I2, &  ! (for compatibility with 32-bit Solve)
     &                        PARM_NAMES,  &  ! Array of parameters name
     &                        PARM_NUM,    &  ! Total number of parameters
     &                        PARM_FILL,   &  ! Filler
     &                        PARM_LAST_FIELD ! Filler
