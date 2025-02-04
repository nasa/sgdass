      PROGRAM    UPDATE_SOURCE_NAMES
! ************************************************************************
! *                                                                      *
! *   Program  UPDATE_SOURCE_NAMES
! *                                                                      *
! * ## 04-JUN-2008 UPDATE_SOURCE_NAMES v1.0 (c) L. Petrov 17-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MM_SOU
      PARAMETER  ( MM_SOU = 2*M_SOU )
      CHARACTER  FILCAT*128, FILSOU*128, FILOUT*128, CAT_LABEL*16
      CHARACTER  C_CAT(MM_SOU)*8, C_SOU(MM_SOU)*8
      CHARACTER  BUFCAT(MM_SOU)*256, BUFSOU(MM_SOU)*256
      TYPE       ( SOURCE_CAT__TYPE ) :: SOU(MM_SOU)
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT(MM_SOU)
      INTEGER*4  NCAT, NSOU, L_CAT, L_SOU, MODE, J1, J2, J3, J4, NU, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!!     FILSOU = '/tmp/old_source.names' ! copy of $SAVE_DIR/source.names
!!     FILSOU = '/vlbi/faps/faps.names'
!!     FILCAT = '/vlbi/solutions/2009c_astro/2009c_astro_cat.txt'
!!     FILCAT = '/vlbi/solutions/rfc_2010c/rfc_2010c_cat.txt'
!!     FILCAT = '/tmp/rfc_2011a_cat.txt'
!!     FILOUT = '/tmp/new_source.names'
!!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: update_source_names {input_src} {cat_file} '// &
     &                        '{cat_label} {out_cat}'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILSOU )
           CALL GETARG ( 2, FILCAT )
           CALL GETARG ( 3, CAT_LABEL )
           CALL GETARG ( 4, FILOUT )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILCAT, MM_SOU, BUFSOU, NCAT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL READ_SOU ( FILCAT, MM_SOU, L_CAT, CAT, C_CAT, MODE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      WRITE ( 6, * ) ' L_CAT = ', L_CAT, ' NCAT = ', NCAT
!
      IUER = -1
      CALL RD_TEXT ( FILSOU, MM_SOU, BUFSOU, NSOU, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL READ_SOU ( FILSOU, MM_SOU, L_SOU, SOU, C_SOU, MODE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
      WRITE ( 6, * ) ' L_SOU = ', L_SOU, ' NSOU = ', NSOU
!
      NU = 0
      DO 410 J1=1,L_SOU
         DO 420 J2=1,L_CAT
!!            IF ( SOU(J1)%J2000_NAME == CAT(J2)%J2000_NAME ) THEN
            IF ( SOU(J1)%IVS_NAME == CAT(J2)%IVS_NAME ) THEN
                 SOU(J1)%CALIB   = CAT(J2)%CALIB
                 SOU(J1)%ALP     = CAT(J2)%ALP
                 SOU(J1)%DEL     = CAT(J2)%DEL
                 SOU(J1)%ALP_ERR = CAT(J2)%ALP
                 SOU(J1)%DEL_ERR = CAT(J2)%DEL
                 SOU(J1)%CORR    = CAT(J2)%CORR
                 SOU(J1)%SOU_ERR = CAT(J2)%SOU_ERR
!
                 IF ( SOU(J1)%CALIB == 0 ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = '-' 
                    ELSE IF ( SOU(J1)%CALIB == 1 ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = 'C' 
                    ELSE IF ( SOU(J1)%CALIB == 2 ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = 'N' 
                    ELSE IF ( SOU(J1)%CALIB == 3 ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = 'U' 
                 END IF
                 CALL RH_TAT ( SOU(J1)%ALP, 4, &
     &                         BUFSOU(SOU(J1)%IND_LINE)(45:58), -2 ) 
                 CALL RG_TAT ( SOU(J1)%DEL, 3, &
     &                         BUFSOU(SOU(J1)%IND_LINE)(60:72), -2 ) 
                 IF ( BUFSOU(SOU(J1)%IND_LINE)(60:60) == ' ' ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(60:60) = '+'
                 END IF
                 WRITE ( UNIT=BUFSOU(SOU(J1)%IND_LINE)(75:80), &
     &                   FMT='(F6.2)' ) SOU(J1)%SOU_ERR*RAD__TO__MAS
                 IF ( SOU(J1)%SOU_ERR*RAD__TO__MAS > 10.0 ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = 'N' 
                    ELSE 
                      BUFSOU(SOU(J1)%IND_LINE)(43:43) = 'C' 
                 END IF
                 IF ( BUFSOU(SOU(J1)%IND_LINE)(75:80) == '******' ) THEN
                      BUFSOU(SOU(J1)%IND_LINE)(75:80) = '999.99'
                 END IF
                 CALL CLRCH ( BUFSOU(SOU(J1)%IND_LINE)(81:) )
                 BUFSOU(SOU(J1)%IND_LINE)(82:) = '! '//CAT_LABEL
                 NU = NU + 1
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NSOU, BUFSOU, FILOUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) 'Filout: '//FILOUT(1:I_LEN(FILOUT))
      WRITE ( 6, '(A,I6)' ) 'Number of updated records: ', NU
!
      END  !#!  
