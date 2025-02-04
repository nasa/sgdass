      PROGRAM    VEX_PARSER_EXAMPLE
      IMPLICIT   NONE 
      INCLUDE    'vex.i'
      INCLUDE    'astro_constants.i'
      CHARACTER  FIL_VEX*128, DATE_START*30, DATE_STOP*30
      TYPE ( VEX_TYPE ) :: VEX
      TYPE ( VEX__FLG_TYPE ) :: FLG
      INTEGER*4  J0, J1, J2, J3, IUER, ISDR
      INTEGER*4  N_STA, N_SOU, N_SCA, N_FRQ, N_IFS
      INTEGER*4, EXTERNAL :: ILEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
!      FIL_VEX = '/vlbi/aua052/aua052.vex' 
!      FIL_VEX = '/tmp/ngvla_01.vex'
!      FIL_VEX = '/f1/home/nhabana/data/memo53_scans.dat'
!      FIL_VEX = '/f1/home/nhabana/data/vex/memo53_scans_B.dat'
!       FIL_VEX = '/f1/home/nhabana/data/vex/aua052.vex'
       FIL_VEX = '/f1/home/nhabana/data/vex/rv118.vex'
      VEX%STATUS = VEX__UNDF
! 
      IUER = -1
      ISDR = 2
      CALL VEX_PARSER ( VEX, FIL_VEX, ISDR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( (ISDR .EQ. 0) .OR. (ISDR .EQ. 1) ) GO TO 110
!
! --- Flags 
!
      CALL VEX_FLAGS ( FLG, FIL_VEX, IUER )
!
! --- Counter
!
      CALL VEX_COUNT ( N_STA, N_SOU, N_SCA, N_FRQ, N_IFS, FIL_VEX, IUER)
!
      WRITE (6,*) '__________________________'
      WRITE (6,*) '|  Reading from VEX File |'
      WRITE (6,*) '__________________________'
!
      WRITE (6,*) '>>',TRIM(FIL_VEX),'<<'
!
! --- Stand alone
!
      WRITE (6,*) '__________________________'
      WRITE (6,*) '|  Stand Alone Variables |'
      WRITE (6,*) '__________________________'
!
      IF ( FLG%EXP ) THEN
         WRITE ( 6, * ) 'STATUS= ', VEX%STATUS
         WRITE ( 6, * ) 'N_STA= ', VEX%N_STA
         WRITE ( 6, * ) 'N_SOU= ', VEX%N_SOU
         WRITE ( 6, * ) 'N_SCA= ', VEX%N_SCA
         WRITE ( 6, * ) 'N_FRQ= ', VEX%N_FRQ
         WRITE ( 6, * ) 'REVISION= ', VEX%REVISION 
         WRITE ( 6, * ) 'EXPER_NAME= ', VEX%EXPER_NAME 
         WRITE ( 6, * ) 'EXPER_DESCR= ', VEX%EXPER_DESCR
         WRITE ( 6, * ) 'CONTACT_NAME= ', VEX%CONTACT_NAME
         WRITE ( 6, * ) 'SCHEDULER_NAME= ', VEX%SCHEDULER_NAME
         WRITE ( 6, * ) ' SCHEDULER_EMAIL= ', VEX%SCHEDULER_EMAIL
         WRITE ( 6, * ) 'VEX%MJD_START= ', VEX%MJD_START
         WRITE ( 6, * ) 'VEX%UTC_START= ', VEX%UTC_START
         WRITE ( 6, * ) 'VEX%MJD_STOP = ', VEX%MJD_STOP
         WRITE ( 6, * ) 'VEX%UTC_STOP = ', VEX%UTC_STOP
  !       DATE_START = MJDSEC_TO_DATE(57575,64800.D0,-1 )
   !      DATE_STOP  = MJDSEC_TO_DATE(57576,64737.D0,-1 )
    !     WRITE ( 6, * ) 'DATE_START = ', DATE_START
     !    WRITE ( 6, * ) 'DATE_STOP  = ', DATE_STOP
      END IF
!
      IF ( .NOT. FLG%GLO ) THEN
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A GLOBAL BLOCK %%'
      END IF
!
      IF ( .NOT. FLG%EXP ) THEN
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN AN EXPER BLOCK %%'
      END IF
!
! --- From Procedures
!
      WRITE (6,*) '________________________'
      WRITE (6,*) '|  Checking Procedures |'
      WRITE (6,*) '________________________'
!
      IF ( FLG%PRO ) THEN
         WRITE (6,*) '%% VALUES FROM THIS BLOCK HAVE BEEN PARSED TO '   &
     &               //' STATION DERIVED TYPE %%'
      ELSE
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A PROCS BLOCK %%'
      END IF
!
! --- From Station
!
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  Checking Station   |'
      WRITE (6,*) '_______________________'
!
      IF ( FLG%STA ) THEN
         DO 410 J1=1,VEX%N_STA
            WRITE (6,*) ' Station ', J1, VEX%STA(J1)%SITE_NAME,         &
     &                  VEX%STA(J1)%SITE_ID,                            &
     &                  ' ASSOCIATED: ', ASSOCIATED ( VEX%STA ) ;       &
     &                  CALL FLUSH ( 6 )
 410     CONTINUE
      ELSE
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A STATION BLOCK %%'
      END IF
!
! --- From Schedule
!
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  Checking Schedules |'
      WRITE (6,*) '_______________________'
!
      IF ( FLG%SCA ) THEN
         DO 420 J3=1,VEX%N_SCA
            WRITE (6,*) 'Scan ', J3, VEX%SCA(J3)%SOU_NAME,              &
     &                  'ASSOCIATED: ', ASSOCIATED ( VEX%SCA ) ;        &
     &                  CALL FLUSH ( 6 )
            WRITE (6,*) '     ', J3, 'SOU_IDX', VEX%SCA(J3)%IND_SOU
            WRITE (6,*) '     ', J3,                                    &
     &                  'MJD/UTC = ', VEX%SCA(J3)%MJD, VEX%SCA(J3)%UTC
            WRITE (6,*) '     ', J3, 'STA_IDX = ',                      &
     &            (VEX%SCA(J3)%IND_STA(J0), J0 =1,VEX%SCA(J3)%N_STA )
            WRITE (6,*) '     ', J3, 'SCAN_DUR = ',                      &
     &            (VEX%SCA(J3)%SCAN_DUR(J0), J0 =1,VEX%SCA(J3)%N_STA )
            WRITE (6,*) '     '
 420     CONTINUE 
      ELSE
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A SCHED BLOCK %%'
      END IF
!
! --- From Sources
!
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  Checking Sources   |'
      WRITE (6,*) '_______________________'
!
      IF ( FLG%SOU ) THEN
         DO 430 J3=1,VEX%N_SOU
            WRITE (6,*) 'Source ', J3, VEX%SOU(J3)%NAME ,               &
     &                  'ASSOCIATED: ', ASSOCIATED ( VEX%SOU ) ;        &
     &                  CALL FLUSH ( 6 )
            WRITE (6,*) '       ', J3,                                  &
     &                  ' RA/DEC = ', VEX%SOU(J3)%RA/DEG__TO__RAD,      &
     &                  VEX%SOU(J3)%DEC/DEG__TO__RAD
            WRITE (6,*) '     '
 430     CONTINUE 
      ELSE
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A SOURCE BLOCK %%'
      END IF
!
! --- From Frequency
!
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  Checking Frequency |'
      WRITE (6,*) '_______________________'
!
      IF ( FLG%FRQ ) THEN
         DO 440 J3=1,N_FRQ
            WRITE (6,*) 'Freq: ', J3, VEX%FRQ(J3)%FRQ_NAME ,            &
     &                  'ASSOCIATED: ', ASSOCIATED ( VEX%FRQ ) ;        &
     &                  CALL FLUSH ( 6 )
            WRITE (6,*) '      ', J3, 'Sample rate = ',                 &
     &                   VEX%FRQ(J3)%SAMPLE_RATE
            WRITE (6,*) '      ', J3, 'Band ID = ',                     &
     &                  (VEX%FRQ(J3)%BAND_ID(J0), J0=1,16)
            WRITE (6,*) '      ', J3, 'SKY FRQ {GHz} = ',               &
     &                  (VEX%FRQ(J3)%SKY_FRQ(J0)/1.D9, J0=1,16)
            WRITE (6,*) '      ', J3, 'CHA_BW {MHz} = ',                &
     &                  (VEX%FRQ(J3)%CHA_BW(J0)/1.D9, J0=1,16)
            WRITE (6,*) '     '
 440     CONTINUE  
      ELSE
         WRITE (6,*) '%% THIS FILE DOES NOT CONTAIN A FREQ BLOCK %%'
      END IF
!
 110  CONTINUE
!
      END  PROGRAM    VEX_PARSER_EXAMPLE  !#!#
