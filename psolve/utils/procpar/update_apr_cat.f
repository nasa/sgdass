      PROGRAM    UPDATE_APR_CAT
! ************************************************************************
! *                                                                      *
! *   Program UPDATE_APR_CAT
! *                                                                      *
! *  ### 11-MAR-2008  UPDATE_APR_CAT v1.3 (c) L. Petrov  01-MAR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT_REF(M_SOU)
      CHARACTER  FIL_APR*128, FIL_CAT*128, FIL_REF*128, FIL_OUT*128
      CHARACTER  BUFA(M_SOU)*256, BUFC(M_SOU)*256, BUFI(M_SOU)*256, &
     &           CR_SOU(M_SOU)*8, STR*32, CAT_LABEL*16, COMMAND_LINE*512
      REAL*8     SIG1, SIG2, CORR, SIG_MAJ, SIG_MIN, TETA, DEC
      LOGICAL*1  FL_KEEP_ICRFDEF
      INTEGER*4  NA, NC, ND, J1, J2, J3, J4, ID, IFMT, NU, IL, IND_REF, &
     &           LR_SOU, MDC, IUER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      FL_KEEP_ICRFDEF = .FALSE.
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: update_apr_cat {apr_src} {cat_file} '// &
     &                        '{cat_label} {out_cat} [keepICRFdef icrf1_cat|nokeepICRFdef]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GET_COMMAND ( COMMAND_LINE )
           CALL GETARG ( 1, FIL_APR )
           CALL GETARG ( 2, FIL_CAT )
           CALL GETARG ( 3, CAT_LABEL )
           CALL GETARG ( 4, FIL_OUT )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                IF ( STR == 'nokeepICRFdef' ) THEN
                     FL_KEEP_ICRFDEF = .FALSE.
                   ELSE IF ( STR == 'keepICRFdef' ) THEN
                     FL_KEEP_ICRFDEF = .TRUE.
                     IF ( IARGC() < 6 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 8401, IUER, 'UPDATE_APR_CAT', 'Missing the 6th '// &
     &                        'argument: the reference catalogue' )
                          CALL EXIT ( 1 )
                     END IF
                     CALL GETARG ( 6, FIL_REF )
                   ELSE
                     IUER = -1
                     CALL ERR_LOG ( 8402, IUER, 'UPDATE_APR_CAT', 'Wrong 5th '// &
     &                   'argument: '//TRIM(STR)//' -- only keepICRFdef or '// &
     &                   'nokeepICRFdef are supported' )
                     CALL EXIT ( 1 )
                END IF               
           END IF               
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_APR, M_SOU, BUFA, NA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8403, IUER, 'UPDATE_APR_CAT', 'Error in reading '// &
     &         'apriori source position file '//FIL_APR )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_CAT, M_SOU, BUFC, NC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8404, IUER, 'UPDATE_APR_CAT', 'Error in reading '// &
     &         'estimated source position file '//FIL_CAT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_KEEP_ICRFDEF ) THEN
           IUER = -1
           CALL READ_SOU ( FIL_REF, M_SOU, LR_SOU, CAT_REF, CR_SOU, MDC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 8405, IUER, 'UPDATE_APR_CAT', 'Error in reading '// &
     &              'the reference source position catalogue'//FIL_REF )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           LR_SOU = 0
      END IF
!      
      IF ( BUFA(1)(1:31) == '$$  SOU-MODFILE Format pre-2000' ) THEN
           IFMT = 1
         ELSE IF ( BUFA(1)(1:67) == '# CATRES Flux and Spectral index file. Format version of 2004.12.18' ) THEN
           IFMT = 2
         ELSE IF ( BUFA(1)(1:50) == '#  Source position file format  1.0  of 2019.08.08' ) THEN
           IFMT = 3
         ELSE 
           WRITE ( 6, * ) 'Unsupported format of the apriori file '//TRIM(FIL_APR)
           CALL EXIT ( 1 )
      END IF
!
      NU = 0
      ND = 0
      DO 410 J1=1,NA
         IF ( BUFA(J1)(1:1) == '$' ) GOTO 410
         IF ( BUFA(J1)(1:17) == '#  Generated with' ) THEN
              BUFA(J1)(19:) = COMMAND_LINE
         END IF
         IF ( BUFA(J1)(1:15) == '#  Last update:' ) THEN
              BUFA(J1)(17:) = GET_CDATE()
         END IF
         IF ( BUFA(J1)(1:37) == '#  Positions of ICRF defining sources' ) THEN
              IF ( FL_KEEP_ICRFDEF ) THEN
                   BUFA(J1)(39:) = 'are kept unchanged'
                 ELSE 
                   BUFA(J1)(39:) = 'were updated'
              END IF
         END IF
!
         IF ( BUFA(J1)(1:1) == '#' ) GOTO 410
         IF ( BUFA(J1)(73:73) == '!' ) THEN
              IF ( BUFA(J1)(74:74) .NE. ' ' ) BUFA(J1) = BUFA(J1)(1:73)//'  '//BUFA(J1)(74:)
              IF ( BUFA(J1)(75:75) .NE. ' ' ) BUFA(J1) = BUFA(J1)(1:73)//'  '//BUFA(J1)(75:)
         END IF
         IF ( FL_KEEP_ICRFDEF ) THEN
              IF ( IFMT == 1 ) THEN
                   IND_REF = LTM_DIF ( 0, LR_SOU, CR_SOU, BUFA(J1)(5:12) )
                ELSE IF ( IFMT == 2 ) THEN
                   IND_REF = LTM_DIF ( 0, LR_SOU, CR_SOU, BUFA(J1)(81:88) )
                ELSE IF ( IFMT == 3 ) THEN
                   IND_REF = LTM_DIF ( 0, LR_SOU, CR_SOU, BUFA(J1)(4:11) )
              END IF
            ELSE 
              IND_REF = 0
         END IF
!
         DO 420 J2=1,NC
            IF ( BUFC(J2)(1:1) == '#' ) GOTO 420
            IF ( BUFC(J2)(1:1) == '$' ) GOTO 420
            IF ( IFMT == 3 .AND. BUFC(J2)(4:11) == BUFA(J1)(4:11) ) THEN
!
                 BUFA(J1)(26:41) = BUFC(J2)(25:39)//' '
                 BUFA(J1)(43:58) = BUFC(J2)(41:56) 
                 IUER = -1
                 BUFA(J1)(46:46) = ':'
                 BUFA(J1)(49:49) = ':'
                 CALL GR_TAT ( BUFA(J1)(43:58), DEC, IUER )
                 BUFA(J1)(46:46) = ' '
                 BUFA(J1)(49:49) = ' '
                 READ ( BUFC(J2)(58:63), '(F6.2)' ) SIG1
                 READ ( BUFC(J2)(65:70), '(F6.2)' ) SIG2
                 READ ( BUFC(J2)(73:78), '(F6.3)' ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 WRITE ( BUFA(J1)(61:67), FMT='(F7.3)' ) SIG_MAJ
                 BUFA(J1)(75:) = CAT_LABEL
                 IF ( SIG_MAJ < 999.999  .AND.  BUFA(J1)(70:70) == '0' ) THEN
                      BUFA(J1)(70:70) = '1'
                 END IF 
                 NU = NU + 1
              ELSE IF ( IFMT == 3 .AND. BUFC(J2)(17:24) == BUFA(J1)(4:11) ) THEN
                 BUFA(J1)(26:41) = BUFC(J2)(27:41)//' '
                 BUFA(J1)(43:58) = BUFC(J2)(43:57)//' '
                 BUFA(J1)(28:28) = ':'
                 BUFA(J1)(31:31) = ':'
                 BUFA(J1)(46:46) = ':'
                 BUFA(J1)(49:49) = ':'
                 IUER = -1
                 CALL GR_TAT ( BUFA(J1)(43:58), DEC, IUER )
                 READ ( BUFC(J2)(59:65), '(F7.2)' ) SIG1
                 READ ( BUFC(J2)(67:72), '(F6.2)' ) SIG2
                 READ ( BUFC(J2)(76:81), '(F6.3)' ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 BUFA(60:60) = ' '
                 BUFA(68:69) = '  '
                 WRITE ( BUFA(J1)(61:67), FMT='(F7.3)' ) SIG_MAJ
                 IF ( BUFA(J1)(61:67) == '*******' ) BUFA(J1)(61:67) = '999.000'
                 BUFA(J1)(75:) = CAT_LABEL
                 IF ( SIG_MAJ < 999.999  .AND.  BUFA(J1)(70:70) == '0' ) THEN
                      BUFA(J1)(70:70) = '1'
                 END IF 
                 NU = NU + 1
              ELSE IF ( IFMT == 3 .AND. BUFC(J2)(11:18) == BUFA(J1)(4:11) ) THEN
                 BUFA(J1)(26:40) = BUFC(J2)(37:51) 
                 BUFA(J1)(43:57) = BUFC(J2)(74:88) 
                 IUER = -1
                 CALL GR_TAT ( BUFC(J2)(74:88), DEC, IUER )
                 READ ( BUFC(J2)(58:67),   '(F10.4)' ) SIG1
                 READ ( BUFC(J2)(95:104),  '(F10.4)' ) SIG2
                 READ ( BUFC(J2)(111:116), '(F6.3)'  ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 WRITE ( BUFA(J1)(61:67), FMT='(F7.3)' ) SIG_MAJ
                 IF ( BUFA(J1)(61:67) == '*******' ) THEN
                      BUFA(J1)(61:67) = '999.000'
                 END IF
                 BUFA(J1)(70:73) = '1  !'
                 IF ( IND_REF > 0 ) THEN
!
! ------------------- This source is marked as ICRF defining and the used wants 
! ------------------- to overwrite the source position with the position from
! ------------------- the ICRF1
!
                      IUER = -1
                      CALL RH_TAT ( CAT_REF(IND_REF)%ALP, 6, BUFA(J1)(25:40), IUER )
                      IUER = -1
                      CALL RG_TAT ( CAT_REF(IND_REF)%DEL, 5, BUFA(J1)(43:57), IUER )
                      IF ( CAT_REF(IND_REF)%DEL > 0.0D0 ) THEN
                           BUFA(J1)(43:43) = '+'
                      END IF
                      WRITE ( UNIT=BUFA(J1)(61:67), FMT='(F7.3)' ) CAT_REF(IND_REF)%SOU_ERR* &
     &                                                             RAD__TO__MAS
                      CALL CLRCH ( BUFA(J1)(75:) )
                      BUFA(J1)(75:) = 'ICRF_def'
                      ND = ND + 1
                    ELSE
                      CALL CLRCH ( BUFA(J1)(75:) )
                      BUFA(J1)(75:) = CAT_LABEL
                 END IF
                 NU = NU + 1
              ELSE IF ( IFMT == 1 .AND. BUFC(J2)(1:8) == 'SOU_GCO:' .AND. &
     &           BUFC(J2)(11:18)  == BUFA(J1)(5:12) ) THEN
!
                 BUFA(J1)(15:29) = BUFC(J2)(25:39) 
                 BUFA(J1)(35:49) = BUFC(J2)(62:76) 
                 IUER = -1
                 CALL GR_TAT ( BUFC(J2)(62:76), DEC, IUER )
                 READ ( BUFC(J2)(46:55),  '(F10.4)' ) SIG1
                 READ ( BUFC(J2)(83:92),  '(F10.4)' ) SIG2
                 READ ( BUFC(J2)(99:104), '(F6.3)'  ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 WRITE ( BUFA(J1)(53:58), FMT='(F6.2)' ) SIG_MAJ
                 CALL CLRCH ( BUFA(J1)(63:) )
                 BUFA(J1)(63:) = CAT_LABEL
                 NU = NU + 1
              ELSE IF ( IFMT == 1 .AND. BUFC(J2)(4:11) == BUFA(J1)(5:12) ) THEN
                 BUFA(J1)(15:29) = BUFC(J2)(25:39) 
                 BUFA(J1)(35:49) = BUFC(J2)(41:55) 
                 BUFC(J2)(44:44) = '_'
                 BUFC(J2)(47:47) = '_'
                 IUER = -1
                 CALL GR_TAT ( BUFC(J2)(41:55), DEC, IUER )
                 READ ( BUFC(J2)(58:63), '(F6.2)' ) SIG1
                 READ ( BUFC(J2)(65:70), '(F6.2)' ) SIG2
                 READ ( BUFC(J2)(73:78), '(F6.3)' ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 WRITE ( BUFA(J1)(53:58), FMT='(F6.2)' ) SIG_MAJ
                 CALL CLRCH ( BUFA(J1)(63:) )
                 BUFA(J1)(63:) = CAT_LABEL
                 NU = NU + 1
              ELSE IF ( IFMT .NE. 3 .AND. BUFC(J2)(5:12) == BUFA(J1)(5:12) ) THEN
                 BUFA(J1) = BUFC(J2)
                 CALL CLRCH ( BUFA(J1)(63:) )
                 BUFA(J1)(63:) = CAT_LABEL
                 NU = NU + 1
              ELSE IF ( IFMT == 2 .AND. BUFC(J2)(4:11) == BUFA(J1)(81:88) ) THEN
                 BUFA(J1)(13:24) = BUFC(J2)(25:36) 
                 BUFA(J1)(26:37) = BUFC(J2)(41:52) 
                 IUER = -1
                 BUFC(J2)(44:44) = '_'
                 BUFC(J2)(47:47) = '_'
                 CALL GR_TAT ( BUFC(J2)(41:55), DEC, IUER )
                 READ ( BUFC(J2)(58:63), '(F6.2)' ) SIG1
                 READ ( BUFC(J2)(65:70), '(F6.2)' ) SIG2
                 READ ( BUFC(J2)(73:78), '(F6.3)' ) CORR 
                 IF ( CORR < -0.999 ) CORR = -0.999
                 IF ( CORR >  0.999 ) CORR =  0.999
                 CALL ERROR_ELLIPSE ( SIG1*DABS(DCOS(DEC)), SIG2, CORR, &
     &                                SIG_MAJ, SIG_MIN, TETA )
                 IL = INDEX ( BUFA(J1), CAT_LABEL )
                 IF ( IL < 1 ) IL = ILEN(BUFA(J1)) + 2
                 CALL CLRCH ( BUFA(J1)(IL:) )
                 BUFA(J1)(IL:) = CAT_LABEL
                 NU = NU + 1
            END IF
 420     CONTINUE 
         IF ( IFMT == 1 ) THEN
              BUFA(J1)(17:17) = ' '
              BUFA(J1)(20:20) = ' '
              BUFA(J1)(38:38) = ' '
              BUFA(J1)(41:41) = ' '
            ELSE IF ( IFMT == 2 ) THEN
              BUFA(J1)(15:15) = '_'
              BUFA(J1)(18:18) = '_'
              BUFA(J1)(29:29) = '_'
              BUFA(J1)(32:32) = '_'
            ELSE IF ( IFMT == 3 ) THEN
              BUFA(J1)(28:28) = ':'
              BUFA(J1)(31:31) = ':'
              BUFA(J1)(46:46) = ':'
              BUFA(J1)(49:49) = ':'
         END IF
 410  CONTINUE 
      IUER = -1
      CALL WR_TEXT ( NA, BUFA, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) ' Output file: '//FIL_OUT(1:I_LEN(FIL_OUT))
!
      WRITE ( 6, * ) 'Format ofthe apriori file:           ', IFMT
      WRITE ( 6, * ) 'Number of records in apriori file:   ', NA
      WRITE ( 6, * ) 'Number of records in the catalogue:  ', NC
      WRITE ( 6, * ) 'Number of updated records:           ', NU
      WRITE ( 6, * ) 'Number of bypassed defining records: ', ND
!      
      END  !#!  
