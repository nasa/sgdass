      PROGRAM    AXOF_UPDATE
! ************************************************************************
! *                                                                      *
! *   Program AXOF_UPDATE
! *                                                                      *
! *  ### 17-MAY-2004   AXOF_UPDATE  v3.1 (c) L. Petrov  20-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MBUF, MAXF, MSTA
      PARAMETER  ( MBUF = 32*1024*1024 )
      PARAMETER  ( MAXF =         8192 )
      PARAMETER  ( MSTA = 512         )
      CHARACTER  BLOFIL*128, SOLFIL*128, AXOFIL*128, FILOUT*128
      CHARACTER, ALLOCATABLE :: SOLBUF(:)*256
      CHARACTER  AXOBUF(MAXF)*256, &
     &           C_STA(MSTA)*8, C_STA_SORTED(MSTA)*8, SOL_ID*64, SOL_DATE*32, &
     &           AXTY_STA(MSTA)*4, TECP_STA(MSTA)*4, ANT_DIAM(MSTA)*5, &
     &           AXO_COM(MSTA)*40
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, AXOF_EXE*128
      CHARACTER  AXOF_FORMAT__LABEL*52, AXOF_FORMAT__LABEL2*28, AXOF_FORMAT__LABEL3*28
      PARAMETER  ( AXOF_FORMAT__LABEL  = '# STATION DESCRIPTION   Format version of 2023.02.18' )
      PARAMETER  ( AXOF_FORMAT__LABEL2 = 'VLBI_AXIS_OFFSET  Format version of 2004.05.18' )
      PARAMETER  ( AXOF_FORMAT__LABEL3 = '# AXOF Format of 2004.10.12 ' )
      REAL*8     APR_VAL(MSTA), EST_VAL(MSTA), EST_ERR(MSTA)
      LOGICAL*4  FL_EST(MSTA)
      INTEGER*4  NBLO, NSOL, NAXO, L_STA, IND, J1, J2, J3, J4, AXO_FMT, &
     &           IER, LUN, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ADD_CLIST, GET_UNIT, I_LEN, ILEN, LTM_DIF
!
      ALLOCATE ( SOLBUF(MBUF) )
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: axof_update.e {spl-file} '// &
     &                        '{apriori_axo_file} {output-file}'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 0, AXOF_EXE )
           CALL GETARG ( 1, SOLFIL   )
           CALL GETARG ( 2, AXOFIL   )
           CALL GETARG ( 3, FILOUT   )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( SOLFIL, MBUF, SOLBUF, NSOL, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!      
      IUER = -1
      CALL RD_TEXT ( AXOFIL, MAXF, AXOBUF, NAXO, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
      CALL NOUT_R8 ( MSTA, EST_VAL )
      CALL NOUT_R8 ( MSTA, EST_ERR )
!
      CALL CLRCH ( SOL_ID   ) 
      CALL CLRCH ( SOL_DATE ) 
!
      IF ( AXOBUF(1)(1:52) == '# STATION DESCRIPTION   Format version of 2013.09.18' ) THEN
           AXO_FMT = 1
         ELSE IF ( AXOBUF(1)(1:52) == '# STATION DESCRIPTION   Format version of 2004.01.26' ) THEN
           AXO_FMT = 2
         ELSE 
           AXO_FMT = 0
      END IF
!
      IF ( AXO_FMT == 0 ) THEN
           CALL ERR_LOG ( 1071, -2, 'AXOF_UPDATE', 'Cannot recognize format '// &
     &         'of the axis offset file '//AXOFIL )
           CALL EXIT ( 1 )
      END IF
!
      L_STA = 0
      DO 420 J2=2,NAXO
         IF ( AXOBUF(J2)(1:1) .EQ. '#' ) GOTO 420
         IF ( AXOBUF(J2)(1:1) .EQ. ' ' ) GOTO 420
         IF ( AXO_FMT == 1 .OR. AXO_FMT == 2 ) THEN
              IND = ADD_CLIST ( MSTA, L_STA, C_STA, AXOBUF(J2)(1:8), -2 )
              IF ( IND .LE. 0 ) THEN
                   WRITE ( 6, * ) 'L_STA = ', L_STA
                   CALL ERR_LOG ( 1072, -2, 'AXOF_UPDATE', 'Station '// &
     &                  AXOBUF(J2)(1:8)//' is not in the list' )
                   CALL EXIT ( 1 )
              END IF
              AXTY_STA(IND) = AXOBUF(J2)(12:15)
              READ ( UNIT=AXOBUF(J2)(18:25), FMT='(F11.6)' ) APR_VAL(IND)
              TECP_STA(IND) = AXOBUF(J2)(28:31)
              ANT_DIAM(IND) = AXOBUF(J2)(34:38)
              AXO_COM(IND)  = AXOBUF(J2)(41:)
         END IF
 420  CONTINUE 
!
      DO 430 J3=1,NSOL
         IF ( SOLBUF(J3)(1:13) .EQ. ' Solution ID:' .AND. &
     &        ILEN(SOL_ID) .EQ. 0                         ) THEN
              SOL_ID = SOLBUF(J3)(19:)
         END IF
!
         IF ( SOLBUF(J3)(1:12) .EQ. ' Local time:'  .AND. &
     &        ILEN(SOL_DATE) .EQ. 0                       ) THEN
              SOL_DATE = SOLBUF(J3)(19:)
         END IF
!
         IF ( SOLBUF(J3)(17:27) .EQ. 'Axis Offset' ) THEN
              CALL VTD_NAME_REPAIR ( SOLBUF(J3)(8:15) )
              IND = LTM_DIF ( 0, L_STA, C_STA, SOLBUF(J3)(8:15) )
              IF ( IND .LE. 0 ) THEN
                   CALL ERR_LOG ( 1073, -2, 'AXOF_UPDATE', 'Trap of '// &
     &                 'internal control: apriori axis offset for station '// &
     &                  SOLBUF(J3)(8:15)//' was not found in the axis '// &
     &                 'offset file '//AXOFIL )
                   CALL EXIT ( 1 )
              END IF
              READ ( UNIT=SOLBUF(J3)(46:53), FMT='(F8.5)' ) EST_VAL(IND)
              READ ( UNIT=SOLBUF(J3)(86:93), FMT='(F8.5)' ) EST_ERR(IND)
              EST_VAL(IND) = EST_VAL(IND)*1.D-3
              EST_ERR(IND) = EST_ERR(IND)*1.D-3
              IF ( ILEN(AXO_COM(IND)) == 0 ) THEN
                   AXO_COM(IND) = AXO_COM(IND)
                 ELSE
                   IF ( INDEX ( AXO_COM(IND), 'adjustment'  ) < 1 ) THEN
                        AXO_COM(IND) = AXO_COM(IND)//' adjustment'
                   END IF
              END IF
         END IF
 430  CONTINUE
!
      CALL LIB$MOVC3 ( 8*L_STA, %REF(C_STA), %REF(C_STA_SORTED) )
      CALL SORT_CH ( L_STA, C_STA_SORTED ) 
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * )'Failure to open output file '//FILOUT(1:I_LEN(FILOUT))
           CALL EXIT ( 1 )
      END IF
!
      WRITE( LUN, '(A)' ) AXOF_FORMAT__LABEL 
      WRITE( LUN, '(A)' ) '# '
      WRITE( LUN, '(A)' ) '# VLBI antenna axis offsets. '
      WRITE( LUN, '(A)' ) '# Offsets are adjusted for some antennas '// &
     &                  'in global solution '//SOL_ID(1:I_LEN(SOL_ID))
      WRITE( LUN, '(A)' ) '# Solution ran on '//SOL_DATE(1:I_LEN(SOL_DATE))
      WRITE( LUN, '(A)' ) '# This file was generated by '// &
     &                   USER_REALNAME(1:I_LEN(USER_REALNAME))//' on '// &
     &                   GET_CDATE()
      WRITE( LUN, '(A)' ) '# '
      WRITE( LUN, '(A)' ) '# '//TRIM(AXOF_EXE)//' '//TRIM(SOLFIL)//' '//TRIM(AXOFIL)//' '//TRIM(FILOUT)
      WRITE( LUN, '(A)' ) '# '
      WRITE( LUN, '(A)' ) '# Format:'
      WRITE( LUN, '(A)' ) '#'
      WRITE( LUN, '(A)' ) '# Field  1:8   A8    Station name'
      WRITE( LUN, '(A)' ) '# Field 11:14  A4    Mounting type'
      WRITE( LUN, '(A)' ) '# Field 17:23  F7.4  A priori offset according to '// &
     &                  'antenna design'
      WRITE( LUN, '(A)' ) '#                    or local measurements in meters'
      WRITE( LUN, '(A)' ) '# Field 26:32  F7.4  A posteriori offset in meters'
      WRITE( LUN, '(A)' ) '# Field 37:42  F6.4  Formal uncertainty either from the ground survey or '
      WRITE( LUN, '(A)' ) '#                    from the VLBI solution in meters. This field is blank '
      WRITE( LUN, '(A)' ) '#                    if the antenna axis offset was not adjusted for this '
      WRITE( LUN, '(A)' ) '#                    station and no results from ground survey were available'
      WRITE( LUN, '(A)' ) '# Field 45:48  A4    Tectonic plate name'
      WRITE( LUN, '(A)' ) '# Field 51:55  F5.1  Antenna diameter (meters)'
      WRITE( LUN, '(A)' ) '# Field 58:97  A40   Comment'
      WRITE( LUN, '(A)' ) '# '
      WRITE( LUN, '(A)' ) '# Name    AxTyp Apriori   Adjust    Error   Plate  Diam  Comment'
      WRITE( LUN, '(A)' ) '# '
      DO 440 J4=1,L_STA
         IND = LTM_DIF ( 0, L_STA, C_STA, C_STA_SORTED(J4) )
         IF ( ILEN(AXO_COM(IND)) == 0 ) THEN
              AXO_COM(IND) = 'apriori'
         END IF
         IF ( EST_ERR(IND) .GT. 1.D-5 ) THEN
              WRITE ( LUN, 110 ) C_STA(IND), AXTY_STA(IND), APR_VAL(IND), EST_VAL(IND), &
     &                           EST_ERR(IND), TECP_STA(IND), ANT_DIAM(IND), &
     &                           TRIM(AXO_COM(IND))
            ELSE 
              WRITE ( LUN, 120 ) C_STA(IND), AXTY_STA(IND), APR_VAL(IND), APR_VAL(IND), &
     &                           TECP_STA(IND), ANT_DIAM(IND), TRIM(AXO_COM(IND))
 110          FORMAT ( A8, 2X, A, 2X, F7.4, 2X, F7.4, ' -+ ', F6.4, 2X, A, 2X, A, 2X, A )
 120          FORMAT ( A8, 2X, A, 2X, F7.4, 2X, F7.4, 12X, A, 2X, A, 2X, A )
         END IF
 440  CONTINUE 
      WRITE( LUN, '(A)' ) '# '
      WRITE( LUN, '(A)' ) AXOF_FORMAT__LABEL 
!
      CLOSE ( UNIT=LUN )
!
      WRITE ( 6, '(A)' ) 'AXOF_UPDATE Output file: '//FILOUT(1:I_LEN(FILOUT))
      CALL EXIT ( 0 )
!
      END  PROGRAM  AXOF_UPDATE
