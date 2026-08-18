!
!  >>>> Include block HARPOS
!  >>>> This block keeps definition of data structure for description
!  >>>> the file with harmonic displacements
!  >>>>
!  >>>> 11-DEC-2002  L. Petrov   2014.07.04_10:16:42
!
        CHARACTER   HARPOS__LABEL*36, HARPOS__LABEL_1*36
        PARAMETER ( HARPOS__LABEL   = 'HARPOS  Format version of 2005.03.28' )
        PARAMETER ( HARPOS__LABEL_1 = 'HARPOS  Format version of 2002.12.12' )
        INTEGER*4   LEN__H_REC, LEN__A_REC, LEN__S_REC, LEN__D_REC
        PARAMETER ( LEN__H_REC = 80 )
        PARAMETER ( LEN__A_REC = 80 )
        PARAMETER ( LEN__S_REC = 80 )
        PARAMETER ( LEN__D_REC = 80 )
!
        INTEGER*4  M__HPSLEN         ! maximal number of lines in a harpos file
        PARAMETER  ( M__HPSLEN = 128*1024 )
!
        TYPE      HARPOS__H_RECORD
            CHARACTER*1   REC_ID    !  1:1   Record Identifier
            CHARACTER*2   FILL_1    !  2:3
            CHARACTER*8   WAVE_ID   !  4:11  Harmonic identifier
            CHARACTER*2   FILL_2    ! 12:13
            CHARACTER*13  PHASE     ! 14:26  Phase of the harmonic in rad
            CHARACTER*2   FILL_3    ! 27:28
            CHARACTER*19  FREQ      ! 29:47  Harmonic frequency in rad/s
            CHARACTER*2   FILL_4    ! 48:49
            CHARACTER*10  ACCEL     ! 50:59  Harmonic acceleration in rad/s**2
            CHARACTER*21  FILL_5    ! 60:80
        END TYPE  HARPOS__H_RECORD  ! HARPOS__H_RECORD !
!
        TYPE      HARPOS__A_RECORD
            CHARACTER*1   REC_ID    !  1:1   Record Identifier
            CHARACTER*2   FILL_1    !  2:3
            CHARACTER*14  AREA_RD   !  4:17  Radius of the applicability area
            CHARACTER*63  FILL_2    ! 18:80
        END TYPE  HARPOS__A_RECORD  ! HARPOS__A_RECORD !
!
        TYPE      HARPOS__S_RECORD
            CHARACTER*1   REC_ID    !  1:1
            CHARACTER*2   FILL_1    !  2:3
            CHARACTER*8   SITE_ID   !  4:11  Site identifier
            CHARACTER*2   FILL_2    ! 12:13
            CHARACTER*13  X_COORD   ! 14:26  Site X coordinate in m
            CHARACTER*1   FILL_3    ! 27:27
            CHARACTER*13  Y_COORD   ! 28:40  Site Y coordinate in m
            CHARACTER*1   FILL_4    ! 41:41
            CHARACTER*13  Z_COORD   ! 42:54  Site Z coordinate in m
            CHARACTER*2   FILL_5    ! 55:56
            CHARACTER*8   GEOC_LAT  ! 57:64  Geocentric latitude in degrees
            CHARACTER*1   FILL_6    ! 65:65
            CHARACTER*8   LONGITUDE ! 66:73  East longitude in degrees
            CHARACTER*1   FILL_7    ! 74:74
            CHARACTER*6   HEIGHT    ! 75:80  Height above the ellipsoid in m
        END TYPE  HARPOS__S_RECORD  ! HARPOS__S_RECORD !
!
        TYPE      HARPOS__D_RECORD
            CHARACTER*1   REC_ID    !  1:1   Record identifier
            CHARACTER*2   FILL_1    !  2:3
            CHARACTER*8   WAVE_ID   !  4:11  Harmonic identifier
            CHARACTER*2   FILL_2    ! 12:13
            CHARACTER*8   SITE_ID   ! 14:21  Site identifier
            CHARACTER*3   FILL_3    ! 22:24
            CHARACTER*8   UP_COS    ! 25:32  Up cosine displacement
            CHARACTER*1   FILL_4    ! 33:33
            CHARACTER*8   EAST_COS  ! 34:41  East cosine displacement
            CHARACTER*1   FILL_5    ! 42:42
            CHARACTER*8   NORTH_COS ! 43:50  North cosine displacement
            CHARACTER*3   FILL_6    ! 51:53
            CHARACTER*8   UP_SIN    ! 54:61  Up sine displacement
            CHARACTER*1   FILL_7    ! 62:62
            CHARACTER*8   EAST_SIN  ! 63:70  East sine displacement
            CHARACTER*1   FILL_8    ! 71:71
            CHARACTER*8   NORTH_SIN ! 72:79  North sine displacement
            CHARACTER*1   FILL_9    ! 80:80
        END TYPE  HARPOS__D_RECORD  ! HARPOS__D_RECORD !
!
	CHARACTER    HARPOS_FORMAT_HELP*17
        PARAMETER  ( HARPOS_FORMAT_HELP = 'harpos_format.txt' )
!
!  >>>> end of include block HARPOS
