      SUBROUTINE MALO_WRITE_AOD1B_LOVE ( MALO, MALO_OUTPUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_WRITE_AOD1B_LOVE 
! *                                                                      *
! * # 01-NOV-2013 MALO_WRITE_AOD1B_LOVE v1.0 (c) L. Petrov 01-NOV-2013 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  MALO_OUTPUT*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128, SGN_LAT*1, SHC_FORMAT*10, &
     &           C_TXT(MALO__SHC_LTXT)*128, FILOUT*128, PREF*128, &
     &           INP_EXT*8, OUT_EXT*8
      INTEGER*4  J1, L_TXT, IND_WC, IP, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
!
! --- Check whether the name has astrisk
!
      IND_WC = INDEX ( MALO_OUTPUT, '*' ) 
      IF ( IND_WC > 1 ) THEN
           PREF    = MALO_OUTPUT(1:IND_WC-1)
           OUT_EXT =  MALO_OUTPUT(IND_WC+1:)
         ELSE 
           CALL CLRCH ( PREF )
           CALL CLRCH ( OUT_EXT  )
           IP = LINDEX ( MALO_OUTPUT, '.' ) 
           IF ( IP > 1 ) THEN
                OUT_EXT = MALO_OUTPUT(IP:)
           END IF
           IF ( OUT_EXT == '.shc' ) THEN
                CONTINUE 
              ELSE 
                CALL ERR_LOG ( 6781, IUER, 'MALO_WRITE_AOD1B_LOVE', 'Unsupported '// &
     &              'output file name extension '//OUT_EXT(1:I_LEN(OUT_EXT))// &
     &              ' . Supported extnesions: .shc . Malo output: '// &
     &              MALO_OUTPUT )
                RETURN 
           END IF
      END IF 
      SHC_FORMAT = 'REAL*4'
!
! --- Create comments put into the file
!
      L_TXT    = 1
      C_TXT(L_TXT) = 'Spherical transform of the surface pressure field'
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'with Love numbers multiplied its haramonics'
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'MALO_SURFACE_TYPE   = '//MALO%CONF%SURFACE_TYPE   
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'MALO_FINAM_MODEL    = '//MALO%CONF%FINAM_MODEL
      L_TXT    = L_TXT + 1
      C_TXT(L_TXT) = 'LOVE_FILE           = '//MALO%CONF%LOVE_FILE
!
      DO 410 J1=1,MALO%NTIM
!
! ------ Build the output file name
!
         IF ( IND_WC > 1 ) THEN
              STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(J1), MALO%TAI_ARR(J1), -2 )
              FILOUT = PREF(1:I_LEN(PREF))//STR(1:4)//STR(6:7)//STR(9:10)//'_'// &
     &                 STR(12:13)//STR(15:16)//OUT_EXT
            ELSE 
              CALL ERR_LOG ( 6782, IUER, 'MALO_WRITE_AOD1B_LOVE', 'No wildcard '// &
     &            'was found in the output file name '//FILOUT )
              RETURN 
         END IF 
!
! ------ Write spherical harmonics in the output file
!
         CALL ERR_PASS ( IUER, IER )
         CALL SHC_WRITE ( MALO%ORD_SPHE, SHC_FORMAT, 2, MALO%SPH(1,0,0,1,J1), &
     &                    MALO%MJD_ARR(J1), MALO%TAI_ARR(J1), L_TXT, C_TXT, &
     &                    FILOUT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6783, IUER, 'MALO_WRITE_AOD1B_LOVE', &
     &            'Failure in an attempt to write the spherical harmonics '// &
     &            'into output file '//FILOUT )
              RETURN 
         END IF
 410  CONTINUE 
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_WRITE_AOD1B_LOVE  !#!  
