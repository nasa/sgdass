      PROGRAM    CHECK_PROC_SNAP
! ************************************************************************
! *                                                                      *
! *   Program CHECK_PROC_SNAP checks snap and proc file and generates    *
! *   a report.                                                          *
! *                                                                      *
! * ### 26-JAN-2022  CHECK_PROC_SNAP  v1.2 (c) L. Petrov 25-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 64*1024 )
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE       ( NERS__TYPE ) :: NERS
      CHARACTER  FILP*128, FILS*128, STR*128, CPS__LABEL*35
      CHARACTER  NERS_CONFIG*128
      LOGICAL*1  LEX
      PARAMETER  ( CPS__LABEL = 'check_proc_snap  1.2  20240226' )
      INTEGER*4  NP, NS, IVRB, J1, J2, J3, J4, J5, IUER
      CHARACTER  BUFS(MBUF)*1024, BUFP(MBUF)*256
      INTEGER*4, EXTERNAL :: ILEN
!
      IVRB = 0
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: check_proc_snap proc snap [verb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILP ) 
           CALL GETARG ( 2, FILS ) 
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR ) 
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB < 0 ) THEN
                     WRITE ( 6, '(A)' ) 'The thrid argument verbosity should be 0 or a positive number' 
                     CALL EXIT ( 1 )
                END IF
           END IF
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) TRIM(CPS__LABEL)
           WRITE ( 6, '(A)' ) ' '
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Checking schedule files '//TRIM(FILP)//' '//TRIM(FILS)
      END IF
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
      IUER = -1 
      CALL RD_TEXT ( FILP, MBUF, BUFP, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6201, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'reading proc file '//FILP )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1 
      CALL RD_TEXT ( FILS, MBUF, BUFS, NS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6202, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'reading snap file '//FILS )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6203, IUER, 'CHECK_PROC_SNAP', 'Error in initializing '// &
     &         'NERS data structure' )
           RETURN 
      END IF
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6204, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'an attempt to retrieve NERS forecast parameters '// &
     &         'form the remote server' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1 
      CALL CHECK_VERS ( NP, BUFP, FILP, NS, BUFS, FILS, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6205, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'checking versions of proc and snap files' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1 
      CALL CHECK_PROC_DEF ( NP, BUFP, FILP, NS, BUFS, FILS, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6206, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'checking procedure definitions' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1 
      CALL CHECK_AZEL ( NERS, NS, BUFS, FILS, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6207, IUER, 'CHECK_PROC_SNAP', 'Error in '// &
     &         'checking azimuths and elevations' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Schedule files '//TRIM(FILP)//' '//TRIM(FILS)//' are correct'
      END IF
!
      END  PROGRAM  CHECK_PROC_SNAP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_VERS ( NP, BUFP, FILP, NS, BUFS, FILS, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CHECK_VERS
! *                                                                      *
! *  ### 26-JAN-2022  CHECK_VERS   v1.1 (c)  L. Petrov  12-FEB-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NP, NS, IVRB, IUER
      CHARACTER  BUFP(NP)*(*), FILP*(*), BUFS(NS)*(*), FILS*(*)
      CHARACTER  PROC__LABEL*44, PROC__LABEL_V1*43, SNAP__LABEL*41, &
     &           ALPHABET*54, VEX2S__MIN_VERS*10
      PARAMETER  ( PROC__LABEL    = '" NASA style of VLBI schedule in proc format' )
      PARAMETER  ( PROC__LABEL_V1 = '" VLBI schedule in proc format. NASA style.' )
      PARAMETER  ( SNAP__LABEL    = '" VLBI experiment schedule in snap format'   )
      PARAMETER  ( VEX2S__MIN_VERS = '2022.01.26' )
      PARAMETER  ( ALPHABET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"' )
      CHARACTER   STA_NAM_PROC*8, STA_ACR_PROC*8, STA_NAM_SNAP*8, STA_ACR_SNAP*8, &
     &            EXP_CODE_SNAP*8, VEX2S_VERS*10, STR*128
      INTEGER*4    MIND
      PARAMETER  ( MIND = 128 )
      INTEGER*4  LIND, IND(2,MIND), J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'CHECK_VERS'
           WRITE ( 6, '(A)' ) ' ' 
      END IF
!
! --- Check proc file magic and version
!
      IF ( BUFP(1)(1:LEN(PROC__LABEL)) .NE. PROC__LABEL .AND. &
     &     BUFP(1) .NE. PROC__LABEL_V1                        ) THEN
           WRITE ( 6, '(A)' ) 'File '//TRIM(FILP)//' is not in the recognizable proc format'
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( STA_NAM_PROC  ) 
      CALL CLRCH ( STA_ACR_PROC  ) 
      CALL CLRCH ( STA_NAM_SNAP  ) 
      CALL CLRCH ( STA_ACR_SNAP  ) 
      CALL CLRCH ( EXP_CODE_SNAP ) 
      CALL CLRCH ( VEX2S_VERS    ) 
      DO 410 J1=1,NP
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         CALL EXWORD ( BUFP(J1), MIND, LIND, IND, ' ', IER )
         IF ( INDEX ( ALPHABET, BUFP(J1)(1:1) ) < 1 ) THEN
              WRITE ( 6, '(A)' ) 'ERROR at line '//TRIM(STR)//' of the procedure file '//TRIM(FILP)// &
     &                            ' : Wrong first character '//BUFP(J1)(1:1)
              CALL EXIT ( 1 )
         ENDIF
         IF ( INDEX ( BUFP(J1), 'Procedure file for station' ) > 0 .AND. LIND .GE. 7 ) THEN
              STA_NAM_PROC = BUFP(J1)(IND(1,6):IND(2,6)) 
              STA_ACR_PROC = BUFP(J1)(IND(1,7):IND(2,7)) 
         ENDIF
         IF ( '" Station:' == BUFP(J1)(1:10) .AND. LIND .GE. 4 ) THEN
              STA_NAM_PROC = BUFP(J1)(IND(1,3):IND(2,3)) 
              STA_ACR_PROC = BUFP(J1)(IND(1,4):IND(2,4)) 
         END IF
 410  CONTINUE 
      CALL TRAN ( 11, STA_NAM_PROC, STA_NAM_PROC )
      CALL TRAN ( 12, STA_ACR_PROC, STA_ACR_PROC )
!
      DO 420 J2=1,NS
         CALL CLRCH ( STR )
         CALL INCH  ( J2, STR )
         IF ( INDEX ( ALPHABET, BUFS(J2)(1:1) ) < 1 ) THEN
              WRITE ( 6, '(A)' ) 'ERROR at line '//TRIM(STR)//' of the snap file '//TRIM(FILS)
              WRITE ( 6, '(A)' ) 'Wrong first character '//BUFS(J2)(1:1)
              CALL EXIT ( 1 )
         ENDIF
!
         CALL EXWORD ( BUFS(J2), MIND, LIND, IND, ' ', IER )
         IF ( INDEX ( BUFS(J2), 'Generated with' ) > 0 .AND. LIND .EQ. 7 ) THEN
              IF ( .NOT. BUFS(J2)(IND(1,4):IND(2,4)) == 'vex_to_snap.py' ) THEN
                   WRITE ( 6, '(A)' ) 'ERROR: snap file '//TRIM(FILS)//' was '// &
     &                                'generated with software that is not suppored: '// &
     &                                BUFS(J2)(IND(1,4):IND(2,4)) 
                   CALL EXIT ( 1 )
              END IF 
              VEX2S_VERS = BUFS(J2)(IND(1,7):IND(2,7)) 
              IF ( VEX2S_VERS < VEX2S__MIN_VERS ) THEN
                   WRITE ( 6, '(A)' ) 'ERROR: snap file '//TRIM(FILS)//' was '// &
     &                                'generated with too old version of software: '// &
     &                                BUFS(J2)(IND(1,7):IND(2,7))
              END IF
         END IF
         IF ( BUFS(J2)(1:10) == '" Station:' ) THEN
              STA_NAM_SNAP = BUFS(J2)(IND(1,3):IND(2,3)) 
              STA_ACR_SNAP = BUFS(J2)(IND(1,4):IND(2,4)) 
         END IF
         IF ( BUFS(J2)(1:18) == '" Experiment_code:' ) THEN
              EXP_CODE_SNAP = BUFS(J2)(IND(1,3):IND(2,3)) 
         END IF
 420  CONTINUE 
!
      IF ( ILEN(STA_NAM_PROC) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Station name was not found in proc file '//TRIM(FILP)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( ILEN(STA_ACR_PROC) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Station acronym was not found in proc file '//TRIM(FILP)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( ILEN(STA_NAM_SNAP) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Station name was not found in snap file '//TRIM(FILS)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( ILEN(STA_NAM_SNAP) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Station acronym was not found in snap file '//TRIM(FILS)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( ILEN(EXP_CODE_SNAP) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Experinent code was not found in snap file '//TRIM(FILS)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( ILEN(VEX2S_VERS) == 0 ) THEN
           WRITE ( 6, '(A)' ) 'vex_to_snap version was not found in snap file '//TRIM(FILS)
           CALL EXIT (  1 ) 
      END IF
!
      IF ( STA_NAM_SNAP .NE. STA_NAM_PROC ) THEN
           WRITE ( 6, '(A)' ) 'ERROR: mismatch in station name between snap and proc file '
           WRITE ( 6, '(A)' ) 'Station name from the proc file: '//TRIM(STA_NAM_PROC)
           WRITE ( 6, '(A)' ) 'Station name from the snap file: '//TRIM(STA_NAM_SNAP)
           CALL EXIT ( 1 )
      END IF
!
      IF ( STA_ACR_SNAP .NE. STA_ACR_PROC ) THEN
           WRITE ( 6, '(A)' ) 'ERROR: mismatch in station acronym between snap and proc file '
           WRITE ( 6, '(A)' ) 'Station name from the proc file: '//TRIM(STA_ACR_PROC)
           WRITE ( 6, '(A)' ) 'Station name from the snap file: '//TRIM(STA_ACR_SNAP)
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'Experiment name:     '//EXP_CODE_SNAP 
           WRITE ( 6, '(A)' ) 'Station name:        '//STA_NAM_SNAP
           WRITE ( 6, '(A)' ) 'Station acronym:     '//STA_ACR_SNAP
           WRITE ( 6, '(A)' ) 'Vex_to_snap version: '//VEX2S_VERS
       END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE CHECK_VERS  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_PROC_DEF ( NP, BUFP, FILP, NS, BUFS, FILS, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_PROC_DEF
! *                                                                      *
! *  ### 26-JAN-2022 CHECK_PROC_DEF v1.2 (c)  L. Petrov 17-NOV-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NP, NS, IVRB, IUER 
      CHARACTER  FILP*(*), FILS*(*), BUFP(NP)*(*), BUFS(NS)*(*)
      INTEGER*4    MIND, M_PRC, M_COM, M_BCM, MAX_PRC_NAM, M_RCL, M_ARR
      PARAMETER  ( MIND  = 128 )
      PARAMETER  ( M_PRC = 128 )
      PARAMETER  ( M_COM = 128 )
      PARAMETER  ( M_BCM = 476 )
      PARAMETER  ( M_RCL =  64 )
      PARAMETER  ( M_ARR = 16*1024 )
      PARAMETER  ( MAX_PRC_NAM = 12 )
      CHARACTER  C_BCM(M_BCM)*16, C_HPR(M_BCM)*16
      DATA C_BCM  / &
     &                      '!               ', & !    1
     &                      'agc             ', & !    2
     &                      'antenna         ', & !    3
     &                      'azeloff         ', & !    4
     &                      'bank_check      ', & !    5
     &                      'bank_status     ', & !    6
     &                      'bbc001          ', & !    7
     &                      'bbc002          ', & !    8
     &                      'bbc003          ', & !    9
     &                      'bbc004          ', & !   10
     &                      'bbc005          ', & !   11
     &                      'bbc006          ', & !   12
     &                      'bbc007          ', & !   13
     &                      'bbc008          ', & !   14
     &                      'bbc009          ', & !   15
     &                      'bbc01           ', & !   16
     &                      'bbc010          ', & !   17
     &                      'bbc011          ', & !   18
     &                      'bbc012          ', & !   19
     &                      'bbc013          ', & !   20
     &                      'bbc014          ', & !   21
     &                      'bbc015          ', & !   22
     &                      'bbc016          ', & !   23
     &                      'bbc017          ', & !   24
     &                      'bbc018          ', & !   25
     &                      'bbc019          ', & !   26
     &                      'bbc02           ', & !   27
     &                      'bbc020          ', & !   28
     &                      'bbc021          ', & !   29
     &                      'bbc022          ', & !   30
     &                      'bbc023          ', & !   31
     &                      'bbc024          ', & !   32
     &                      'bbc025          ', & !   33
     &                      'bbc026          ', & !   34
     &                      'bbc027          ', & !   35
     &                      'bbc028          ', & !   36
     &                      'bbc029          ', & !   37
     &                      'bbc03           ', & !   38
     &                      'bbc030          ', & !   39
     &                      'bbc031          ', & !   40
     &                      'bbc032          ', & !   41
     &                      'bbc033          ', & !   42
     &                      'bbc034          ', & !   43
     &                      'bbc035          ', & !   44
     &                      'bbc036          ', & !   45
     &                      'bbc037          ', & !   46
     &                      'bbc038          ', & !   47
     &                      'bbc039          ', & !   48
     &                      'bbc04           ', & !   49
     &                      'bbc040          ', & !   50
     &                      'bbc041          ', & !   51
     &                      'bbc042          ', & !   52
     &                      'bbc043          ', & !   53
     &                      'bbc044          ', & !   54
     &                      'bbc045          ', & !   55
     &                      'bbc046          ', & !   56
     &                      'bbc047          ', & !   57
     &                      'bbc048          ', & !   58
     &                      'bbc049          ', & !   59
     &                      'bbc05           ', & !   60
     &                      'bbc050          ', & !   61
     &                      'bbc051          ', & !   62
     &                      'bbc052          ', & !   63
     &                      'bbc053          ', & !   64
     &                      'bbc054          ', & !   65
     &                      'bbc055          ', & !   66
     &                      'bbc056          ', & !   67
     &                      'bbc057          ', & !   68
     &                      'bbc058          ', & !   69
     &                      'bbc059          ', & !   70
     &                      'bbc06           ', & !   71
     &                      'bbc060          ', & !   72
     &                      'bbc061          ', & !   73
     &                      'bbc062          ', & !   74
     &                      'bbc063          ', & !   75
     &                      'bbc064          ', & !   76
     &                      'bbc065          ', & !   77
     &                      'bbc066          ', & !   78
     &                      'bbc067          ', & !   79
     &                      'bbc068          ', & !   80
     &                      'bbc069          ', & !   81
     &                      'bbc07           ', & !   82
     &                      'bbc070          ', & !   83
     &                      'bbc071          ', & !   84
     &                      'bbc072          ', & !   85
     &                      'bbc073          ', & !   86
     &                      'bbc074          ', & !   87
     &                      'bbc075          ', & !   88
     &                      'bbc076          ', & !   89
     &                      'bbc077          ', & !   90
     &                      'bbc078          ', & !   91
     &                      'bbc079          ', & !   92
     &                      'bbc08           ', & !   93
     &                      'bbc080          ', & !   94
     &                      'bbc081          ', & !   95
     &                      'bbc082          ', & !   96
     &                      'bbc083          ', & !   97
     &                      'bbc084          ', & !   98
     &                      'bbc085          ', & !   99
     &                      'bbc086          ', & !  100
     &                      'bbc087          ', & !  101
     &                      'bbc088          ', & !  102
     &                      'bbc089          ', & !  103
     &                      'bbc09           ', & !  104
     &                      'bbc090          ', & !  105
     &                      'bbc091          ', & !  106
     &                      'bbc092          ', & !  107
     &                      'bbc093          ', & !  108
     &                      'bbc094          ', & !  109
     &                      'bbc095          ', & !  110
     &                      'bbc096          ', & !  111
     &                      'bbc097          ', & !  112
     &                      'bbc098          ', & !  113
     &                      'bbc099          ', & !  114
     &                      'bbc1            ', & !  115
     &                      'bbc10           ', & !  116
     &                      'bbc100          ', & !  117
     &                      'bbc101          ', & !  118
     &                      'bbc102          ', & !  119
     &                      'bbc103          ', & !  120
     &                      'bbc104          ', & !  121
     &                      'bbc105          ', & !  122
     &                      'bbc106          ', & !  123
     &                      'bbc107          ', & !  124
     &                      'bbc108          ', & !  125
     &                      'bbc109          ', & !  126
     &                      'bbc11           ', & !  127
     &                      'bbc110          ', & !  128
     &                      'bbc111          ', & !  129
     &                      'bbc112          ', & !  130
     &                      'bbc113          ', & !  131
     &                      'bbc114          ', & !  132
     &                      'bbc115          ', & !  133
     &                      'bbc116          ', & !  134
     &                      'bbc117          ', & !  135
     &                      'bbc118          ', & !  136
     &                      'bbc119          ', & !  137
     &                      'bbc12           ', & !  138
     &                      'bbc120          ', & !  139
     &                      'bbc121          ', & !  140
     &                      'bbc122          ', & !  141
     &                      'bbc123          ', & !  142
     &                      'bbc124          ', & !  143
     &                      'bbc125          ', & !  144
     &                      'bbc126          ', & !  145
     &                      'bbc127          ', & !  146
     &                      'bbc128          ', & !  147
     &                      'bbc13           ', & !  148
     &                      'bbc14           ', & !  149
     &                      'bbc15           ', & !  150
     &                      'bbc16           ', & !  151
     &                      'bbc2            ', & !  152
     &                      'bbc3            ', & !  153
     &                      'bbc4            ', & !  154
     &                      'bbc_gain        ', & !  155
     &                      'bbcn            ', & !  156
     &                      'bbcnn           ', & !  157
     &                      'bbcnnn          ', & !  158
     &                      'beam            ', & !  159
     &                      'bit_density     ', & !  160
     &                      'bit_density1    ', & !  161
     &                      'bit_density2    ', & !  162
     &                      'bit_streams     ', & !  163
     &                      'bread           ', & !  164
     &                      'break           ', & !  165
     &                      'cable           ', & !  166
     &                      'cablediff       ', & !  167
     &                      'cablelong       ', & !  168
     &                      'cal             ', & !  169
     &                      'caltemp         ', & !  170
     &                      'check           ', & !  171
     &                      'cont            ', & !  172
     &                      'cont_cal        ', & !  173
     &                      'cor01           ', & !  174
     &                      'cor02           ', & !  175
     &                      'cor03           ', & !  176
     &                      'cor04           ', & !  177
     &                      'core3h          ', & !  178
     &                      'core3h_mode     ', & !  179
     &                      'cornn           ', & !  180
     &                      'data_check      ', & !  181
     &                      'data_valid      ', & !  182
     &                      'date            ', & !  183
     &                      'dbbc            ', & !  184
     &                      'dbbc2           ', & !  185
     &                      'dbbc3           ', & !  186
     &                      'dbbcclose       ', & !  187
     &                      'dbbcrelink      ', & !  188
     &                      'decode          ', & !  189
     &                      'decode4         ', & !  190
     &                      'diag            ', & !  191
     &                      'disk2file       ', & !  192
     &                      'disk_pos        ', & !  193
     &                      'disk_record     ', & !  194
     &                      'disk_serial     ', & !  195
     &                      'dqa             ', & !  196
     &                      'ds              ', & !  197
     &                      'echo            ', & !  198
     &                      'enable          ', & !  199
     &                      'enable1         ', & !  200
     &                      'enable2         ', & !  201
     &                      'encode          ', & !  202
     &                      'errmsg          ', & !  203
     &                      'et              ', & !  204
     &                      'et1             ', & !  205
     &                      'et2             ', & !  206
     &                      'fb              ', & !  207
     &                      'fb_config       ', & !  208
     &                      'fb_mode         ', & !  209
     &                      'fb_status       ', & !  210
     &                      'ff              ', & !  211
     &                      'ff1             ', & !  212
     &                      'ff2             ', & !  213
     &                      'fila10g         ', & !  214
     &                      'fila10g2        ', & !  215
     &                      'fila10g_mode    ', & !  216
     &                      'fivept          ', & !  217
     &                      'flush           ', & !  218
     &                      'flux            ', & !  219
     &                      'fmout           ', & !  220
     &                      'form            ', & !  221
     &                      'form4           ', & !  222
     &                      'fs              ', & !  223
     &                      'fsversion       ', & !  224
     &                      'ft01            ', & !  225
     &                      'ft02            ', & !  226
     &                      'ft03            ', & !  227
     &                      'ft04            ', & !  228
     &                      'ftnn            ', & !  229
     &                      'halt            ', & !  230
     &                      'hdata           ', & !  231
     &                      'hdata1          ', & !  232
     &                      'hdata2          ', & !  233
     &                      'hdcalc          ', & !  234
     &                      'hdcalc1         ', & !  235
     &                      'hdcalc2         ', & !  236
     &                      'hecho           ', & !  237
     &                      'hecho1          ', & !  238
     &                      'hecho2          ', & !  239
     &                      'help            ', & !  240
     &                      'holog           ', & !  241
     &                      'hpib            ', & !  242
     &                      'if              ', & !  243
     &                      'if3             ', & !  244
     &                      'ifa             ', & !  245
     &                      'ifadjust        ', & !  246
     &                      'ifb             ', & !  247
     &                      'ifc             ', & !  248
     &                      'ifd             ', & !  249
     &                      'ifdab           ', & !  250
     &                      'ifdcd           ', & !  251
     &                      'ife             ', & !  252
     &                      'iff             ', & !  253
     &                      'ifg             ', & !  254
     &                      'ifh             ', & !  255
     &                      'ifp01           ', & !  256
     &                      'ifp02           ', & !  257
     &                      'ifp03           ', & !  258
     &                      'ifp04           ', & !  259
     &                      'ifpnn           ', & !  260
     &                      'iftpa           ', & !  261
     &                      'iftpb           ', & !  262
     &                      'iftpc           ', & !  263
     &                      'iftpd           ', & !  264
     &                      'iftpe           ', & !  265
     &                      'iftpf           ', & !  266
     &                      'iftpg           ', & !  267
     &                      'iftph           ', & !  268
     &                      'iftpx           ', & !  269
     &                      'ifx             ', & !  270
     &                      'in2net          ', & !  271
     &                      'iread           ', & !  272
     &                      'k4ib            ', & !  273
     &                      'k4pcalports     ', & !  274
     &                      'label           ', & !  275
     &                      'label1          ', & !  276
     &                      'label2          ', & !  277
     &                      'last_check      ', & !  278
     &                      'list            ', & !  279
     &                      'lo              ', & !  280
     &                      'lo_config       ', & !  281
     &                      'locate          ', & !  282
     &                      'locate1         ', & !  283
     &                      'locate2         ', & !  284
     &                      'log             ', & !  285
     &                      'logout          ', & !  286
     &                      'lvdt            ', & !  287
     &                      'lvdt1           ', & !  288
     &                      'lvdt2           ', & !  289
     &                      'mat             ', & !  290
     &                      'matload         ', & !  291
     &                      'mcb             ', & !  292
     &                      'mk5             ', & !  293
     &                      'mk5_status      ', & !  294
     &                      'mk5b_mode       ', & !  295
     &                      'mk5c_mode       ', & !  296
     &                      'mk5close        ', & !  297
     &                      'mk5relink       ', & !  298
     &                      'mk6             ', & !  299
     &                      'mk6a            ', & !  300
     &                      'mk6b            ', & !  301
     &                      'mode            ', & !  302
     &                      'mon01           ', & !  303
     &                      'mon02           ', & !  304
     &                      'mon03           ', & !  305
     &                      'mon04           ', & !  306
     &                      'monnn           ', & !  307
     &                      'mount           ', & !  308
     &                      'mount1          ', & !  309
     &                      'mount2          ', & !  310
     &                      'newtape         ', & !  311
     &                      'newtape1        ', & !  312
     &                      'newtape2        ', & !  313
     &                      'oldtape         ', & !  314
     &                      'oldtape1        ', & !  315
     &                      'onoff           ', & !  316
     &                      'onsource        ', & !  317
     &                      'op              ', & !  318
     &                      'op_stream       ', & !  319
     &                      'parity          ', & !  320
     &                      'parity1         ', & !  321
     &                      'parity2         ', & !  322
     &                      'pass            ', & !  323
     &                      'pass1           ', & !  324
     &                      'pass2           ', & !  325
     &                      'patch           ', & !  326
     &                      'pcal            ', & !  327
     &                      'pcal1           ', & !  328
     &                      'pcal2           ', & !  329
     &                      'pcald           ', & !  330
     &                      'pcalform        ', & !  331
     &                      'pcalports       ', & !  332
     &                      'peak            ', & !  333
     &                      'peak1           ', & !  334
     &                      'peak2           ', & !  335
     &                      'perr            ', & !  336
     &                      'perr1           ', & !  337
     &                      'perr2           ', & !  338
     &                      'pfmed           ', & !  339
     &                      'proc            ', & !  340
     &                      'proc_library    ', & !  341
     &                      'pwrmon          ', & !  342
     &                      'radecoff        ', & !  343
     &                      'rcl             ', & !  344
     &                      'rdbe            ', & !  345
     &                      'rdbe_atten      ', & !  346
     &                      'rdbe_attena     ', & !  347
     &                      'rdbe_attenb     ', & !  348
     &                      'rdbe_attenc     ', & !  349
     &                      'rdbe_attend     ', & !  350
     &                      'rdbea           ', & !  351
     &                      'rdbeb           ', & !  352
     &                      'rdbec           ', & !  353
     &                      'rdbed           ', & !  354
     &                      'rec             ', & !  355
     &                      'rec1            ', & !  356
     &                      'rec2            ', & !  357
     &                      'rec_mode        ', & !  358
     &                      'rec_mode1       ', & !  359
     &                      'recpatch        ', & !  360
     &                      'recpatch1       ', & !  361
     &                      'repro           ', & !  362
     &                      'repro1          ', & !  363
     &                      'repro2          ', & !  364
     &                      'reset           ', & !  365
     &                      'rollform        ', & !  366
     &                      'rvac            ', & !  367
     &                      'rvac1           ', & !  368
     &                      'rvac2           ', & !  369
     &                      'rw              ', & !  370
     &                      'rw1             ', & !  371
     &                      'rw2             ', & !  372
     &                      'rx              ', & !  373
     &                      's2check         ', & !  374
     &                      's2delays        ', & !  375
     &                      's2ping          ', & !  376
     &                      's2status        ', & !  377
     &                      's2version       ', & !  378
     &                      'satellite       ', & !  379
     &                      'satoff          ', & !  380
     &                      'save_file       ', & !  381
     &                      'savev           ', & !  382
     &                      'savev1          ', & !  383
     &                      'savev2          ', & !  384
     &                      'scan_check      ', & !  385
     &                      'scan_name       ', & !  386
     &                      'schedule        ', & !  387
     &                      'sched_end       ', & !  388
     &                      'sched_initi     ', & !  389
     &                      'select          ', & !  390
     &                      'sff             ', & !  391
     &                      'sff1            ', & !  392
     &                      'sff2            ', & !  393
     &                      'source          ', & !  394
     &                      'srw             ', & !  395
     &                      'srw1            ', & !  396
     &                      'srw2            ', & !  397
     &                      'st              ', & !  398
     &                      'st1             ', & !  399
     &                      'st2             ', & !  400
     &                      'stack           ', & !  401
     &                      'stack1          ', & !  402
     &                      'stack2          ', & !  403
     &                      'stamsg          ', & !  404
     &                      'status          ', & !  405
     &                      'sy              ', & !  406
     &                      'systracks       ', & !  407
     &                      'systracks1      ', & !  408
     &                      'systracks2      ', & !  409
     &                      'tacd            ', & !  410
     &                      'tape            ', & !  411
     &                      'tape1           ', & !  412
     &                      'tape2           ', & !  413
     &                      'tapeform        ', & !  414
     &                      'tapeform1       ', & !  415
     &                      'tapeform2       ', & !  416
     &                      'tapepos         ', & !  417
     &                      'tapepos1        ', & !  418
     &                      'tapepos2        ', & !  419
     &                      'terminate       ', & !  420
     &                      'ti              ', & !  421
     &                      'tle             ', & !  422
     &                      'tnx             ', & !  423
     &                      'tonedet         ', & !  424
     &                      'tonemeas        ', & !  425
     &                      'tpdiff          ', & !  426
     &                      'tpdiffgain      ', & !  427
     &                      'tpgain          ', & !  428
     &                      'tpi             ', & !  429
     &                      'tpical          ', & !  430
     &                      'tpicd           ', & !  431
     &                      'tpzero          ', & !  432
     &                      'track           ', & !  433
     &                      'trackform       ', & !  434
     &                      'tracks          ', & !  435
     &                      'tsys            ', & !  436
     &                      'user_device     ', & !  437
     &                      'user_info       ', & !  438
     &                      'user_info1      ', & !  439
     &                      'vc01            ', & !  440
     &                      'vc02            ', & !  441
     &                      'vc03            ', & !  442
     &                      'vc04            ', & !  443
     &                      'vc05            ', & !  444
     &                      'vc06            ', & !  445
     &                      'vc07            ', & !  446
     &                      'vc08            ', & !  447
     &                      'vc09            ', & !  448
     &                      'vc10            ', & !  449
     &                      'vc11            ', & !  450
     &                      'vc12            ', & !  451
     &                      'vc13            ', & !  452
     &                      'vc14            ', & !  453
     &                      'vc15            ', & !  454
     &                      'vcbw            ', & !  455
     &                      'vcif            ', & !  456
     &                      'vcnn            ', & !  457
     &                      'vdqa            ', & !  458
     &                      'vform           ', & !  459
     &                      'vsi1            ', & !  460
     &                      'vsi2            ', & !  461
     &                      'vsi4            ', & !  462
     &                      'vsin            ', & !  463
     &                      'vx              ', & !  464
     &                      'vxlo            ', & !  465
     &                      'wakeup          ', & !  466
     &                      'worm            ', & !  467
     &                      'worm1           ', & !  468
     &                      'worm2           ', & !  469
     &                      'wvolt           ', & !  470
     &                      'wvolt1          ', & !  471
     &                      'wvolt2          ', & !  472
     &                      'wx              ', & !  473
     &                      'xdisp           ', & !  474
     &                      'xlog            ', & !  475
     &                      'xyoff           '  & !  476
     &            /
!
      CHARACTER  C_PRC(M_PRC)*32, C_COM(M_PRC)*32, COM*18, COMS*18, PRC*18, &
     &           C_ARR(M_ARR)*256, CN_ARR(M_ARR)*256, STR*128, STR1*128, &
     &           MESS*1024
      LOGICAL*1  FL_DEFINE
      INTEGER*4  LIND, IND(2,MIND), L_PRC, L_COM, I_PRC, I_COM, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           IPRC_BEG(M_PRC), IPRC_END(M_PRC), L_ARR, LA_ARR, LN_ARR, &
     &           L_HPR, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' ' 
           WRITE ( 6, '(A)' ) 'CHECK_PROC_DEF'
      END IF
      L_PRC = 0
      I_PRC = 0
      L_HPR = 0
      FL_DEFINE = .FALSE.
      DO 410 J1=1,NP
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         CALL EXWORD ( BUFP(J1), MIND, LIND, IND, ' ', IER )
         IF ( BUFP(J1)(1:20) == '" Hidden procedures:' .AND. LIND .GE. 4 ) THEN
              DO 520 J2=4,LIND
                 L_HPR = L_HPR + 1
                 C_HPR(L_HPR) = BUFP(J1)(IND(1,J2):IND(2,J2))
 520          CONTINUE 
         END IF
         IF ( BUFP(J1)(1:1) == '"' ) GOTO 410
         COM = BUFP(J1)(IND(1,1):IND(2,1)) 
         IF ( LIND .GE. 2 ) THEN
              IF ( TRIM(COM) == 'define' ) THEN
                   IF ( FL_DEFINE ) THEN
                        WRITE ( 6, '(A)' ) 'ERROR: command define on line '//TRIM(STR)// &
     &                                     ' was encountered, but enddef from the'// &
     &                                     ' previos definition was not found' 
                        CALL EXIT ( 1 )
                   END IF
                   PRC = BUFP(J1)(IND(1,2):IND(2,2)) 
                   IF ( LEN(BUFP(J1)(IND(1,2):IND(2,2))) > MAX_PRC_NAM ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( MAX_PRC_NAM, STR1 )
                        WRITE ( 6, '(A)' ) 'ERROR: procedure '//BUFP(J1)(IND(1,2):IND(2,2))// &
     &                                     ' defined on line '//TRIM(STR)// &
     &                                     ' has name longer than '//TRIM(STR1)//' characters' 
                        CALL EXIT ( 1 )
                   END IF
!
                   IF ( L_PRC > 0 ) THEN
                        IF ( LTM_DIF ( 1, L_PRC, C_PRC, TRIM(PRC) ) > 0 ) THEN
                             WRITE ( 6, '(A)' ) 'ERROR: Procedure '//TRIM(PRC)// &
     &                                          ' in line '//TRIM(STR)// &
     &                                          ' is definded more than once'
                             CALL EXIT ( 1 )
                        END IF 
                   END IF
                   I_PRC = ADD_CLIST ( M_PRC, L_PRC, C_PRC, TRIM(PRC), IER )
                   IPRC_BEG(I_PRC) = J1 + 1
                   FL_DEFINE = .TRUE.
              END IF
         END IF
         IF ( TRIM(COM) == 'enddef' .AND. I_PRC > 0 ) THEN
              IF ( .NOT. FL_DEFINE ) THEN
                   WRITE ( 6, '(A)' ) 'ERROR: command enddef on line '//TRIM(STR)// &
     &                                     ' was encountered, but not preceeding'// &
     &                                     ' define was found'
                   CALL EXIT ( 1 )
              END IF
              IPRC_END(I_PRC) = J1 - 1
              FL_DEFINE = .FALSE.
         END IF
 410  CONTINUE 
      IF ( FL_DEFINE ) THEN
           WRITE ( 6, '(A)' ) 'ERROR: definition of the procedure did not ended '// &
     &                        'with enddef when prcedure file ended' 
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' '
           DO 420 J2=1,L_PRC
              WRITE ( 6, 110 ) J2, C_PRC(J2), IPRC_BEG(J2), IPRC_END(J2)
 110          FORMAT( '  Procedure defintions: ', I3, 2X, A12, ' inds: ', I4, 1X, I4 ) 
 420       CONTINUE 
      END IF
!
      L_COM = 0
      DO 430 J3=1,NP
         CALL CLRCH ( STR )
         CALL INCH  ( J3, STR )
         CALL EXWORD ( BUFP(J3), MIND, LIND, IND, ' ,=', IER )
         IF ( BUFP(J3)(1:1) == '"' ) GOTO 430
!
         COM = BUFP(J3)(IND(1,1):IND(2,1)) 
         IF ( BUFP(J3)(IND(2,1):IND(2,1)) == '@' ) THEN
              COMS = BUFP(J3)(IND(1,1):IND(2,1)-1) 
            ELSE
              COMS = COM
         END IF
         IF ( TRIM(COM) == 'define' ) THEN
              CONTINUE 
            ELSE IF ( TRIM(COM) == 'enddef' ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_PRC, C_PRC, TRIM(COM) ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_PRC, C_PRC, TRIM(COMS) ) > 0 ) THEN
              CONTINUE 
            ELSE              
              IF ( COM(1:1) == '!' ) THEN
                   IF ( .NOT. COM(1:2) == '!*' ) THEN
                        CALL CHECK_WAIT ( BUFP(J3), MESS )
                        IF ( ILEN(MESS) > 0 ) THEN
                             WRITE ( 6, '(A)' ) 'ERROR: wrong format of wait command '//TRIM(BUFP(J3))// &
     &                                          ' at line '//TRIM(STR)//' -- of the procedure '// &
     &                                          'file '//TRIM(FILP)//' : '//TRIM(MESS)
                             CALL EXIT ( 1 )
                        END IF
                      ELSE
                        CONTINUE 
                   END IF
                   CALL CLRCH ( COM(2:) )
              END IF
              I_PRC = ADD_CLIST ( M_COM, L_COM, C_COM, TRIM(COM), IER )
         END IF
 430  CONTINUE 
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' '
           DO 440 J4=1,L_COM
              WRITE ( 6, '("  Procedure commands:   ", I3, 2X, A)' ) J4, TRIM(C_COM(J4))
 440       CONTINUE 
      END IF
!
! --- Now check for procedure name of command name that is not defined
!
      DO 450 J5=1,NP
         CALL CLRCH ( STR )
         CALL INCH  ( J5, STR )
         CALL EXWORD ( BUFP(J5), MIND, LIND, IND, ' ,=', IER )
         IF ( BUFP(J5)(1:1) == '"' ) GOTO 450
         COM = BUFP(J5)(IND(1,1):IND(2,1))
         IF ( COM(1:1) == '!' ) THEN
              CALL CLRCH ( COM(2:) )
         END IF
         IF ( BUFP(J5)(IND(2,1):IND(2,1)) == '@' ) THEN
              COMS = BUFP(J5)(IND(1,1):IND(2,1)-1) 
            ELSE
              COMS = COM
         END IF
         IF ( TRIM(COM) == 'define' ) THEN
              CONTINUE 
            ELSE IF ( TRIM(COM) == 'enddef' ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_PRC, C_PRC, TRIM(COM)  ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_PRC, C_PRC, TRIM(COMS) ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, M_BCM, C_BCM, TRIM(COM)  ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, M_BCM, C_BCM, TRIM(COMS) ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_HPR, C_HPR, TRIM(COM)  ) > 0 ) THEN
              CONTINUE 
            ELSE IF ( LTM_DIF ( 1, L_HPR, C_HPR, TRIM(COMS) ) > 0 ) THEN
              CONTINUE 
            ELSE
              WRITE ( 6, '(A)' ) 'ERROR: command or procedure '//BUFP(J5)(IND(1,1):IND(2,1))// &
     &                           ' on line '//TRIM(STR)//' is not defined'
              CALL EXIT ( 1 )
         END IF
 450  CONTINUE 
!
! --- Expand procedures of a snap file
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) '============ Expanded snap file: ============='
           WRITE ( 6, '(A)' ) ' '
      END IF
!
      DO 460 J6=1,NS
         IF ( BUFS(J6)(1:1) == '"' ) GOTO 460
         L_ARR = 1
         C_ARR(L_ARR) = BUFS(J6)
         DO 470 J7=1,M_RCL
            CALL CLRCH ( STR )
            DO 480 J8=1,L_ARR
               CALL EXWORD ( C_ARR(J8), MIND, LIND, IND, ' ,=', IER )
               COM = C_ARR(J8)(IND(1,1):IND(2,1))
               I_PRC = LTM_DIF ( 1, L_PRC, C_PRC, TRIM(COM) )
               IF ( I_PRC > 0 ) THEN
                    IF ( IPRC_END(I_PRC) .GE. IPRC_BEG(I_PRC) ) THEN
                         LA_ARR = IPRC_END(I_PRC) - IPRC_BEG(I_PRC) + 1
                         IF ( IND(1,1) == 1 ) THEN
                              C_ARR(J8) = '$'//C_ARR(J8)
                            ELSE
                              C_ARR(J8) = C_ARR(J8)(1:IND(1,1)-1)//'$'//C_ARR(J8)(IND(1,1):)
                         END IF
                         IF ( J8 < L_ARR ) THEN
                              DO 490 J9=J8+1,L_ARR
                                 C_ARR(J9+LA_ARR) = C_ARR(J9)
 490                          CONTINUE 
                         END IF
                         DO 4100 J10=1,LA_ARR
                            C_ARR(J8+J10) = STR(1:IND(1,1)+4)//BUFP(IPRC_BEG(I_PRC)+J10-1)
 4100                    CONTINUE 
                         LN_ARR = L_ARR + LA_ARR
                         GOTO 880
                    END IF
                 ELSE IF ( LTM_DIF ( 1, M_BCM, C_BCM, TRIM(COM) ) > 0 ) THEN
                    CONTINUE 
                 ELSE IF ( COM(1:1) == '"' ) THEN
                    CONTINUE 
                 ELSE IF ( COM(1:1) == '$' ) THEN
                    CONTINUE 
                 ELSE IF ( LTM_DIF ( 1, L_HPR, C_HPR, TRIM(COM)  ) > 0 ) THEN
                    CONTINUE 
                 ELSE IF ( COM(1:1) == '!' ) THEN
                   IF ( .NOT. COM(1:2) == '!*' ) THEN
                        CALL CHECK_WAIT ( COM, MESS )
                        IF ( ILEN(MESS) > 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J6, STR )
                             WRITE ( 6, '(A)' ) 'ERROR: wrong format of wait command '//TRIM(COM)// &
     &                                          ' at line '//TRIM(STR)//' of the procedure '// &
     &                                          'file '//TRIM(FILP)//' : '//TRIM(MESS)
                            CALL EXIT ( 1 )
                        END IF
                   END IF
                 ELSE
                    CALL CLRCH ( STR )
                    CALL INCH  ( J6, STR )
                    WRITE ( 6, '(A)' ) 'ERROR in line '//TRIM(STR)//' of the snap file '//TRIM(FILS)// &
     &                                 ' : command '//TRIM(COM)//' does not match'// &
     &                                 ' defined procedures of functions' 
                    CALL EXIT ( 1 )
               END IF
 480        CONTINUE
            GOTO 870
 880        CONTINUE 
            L_ARR = LN_ARR
 470     CONTINUE 
         CALL CLRCH ( STR )
         CALL INCH  ( J6, STR )
         CALL CLRCH ( STR1 )
         CALL INCH  ( M_RCL, STR1 )
         WRITE ( 6, '(A)' ) 'Error in processing line '//TRIM(STR)//' of the snap file '//TRIM(FILS)// &
     &                      ' : exceeded the limit of '//TRIM(STR1)// &
     &                      ' of nested procedure expansions' 
         CALL EXIT ( 1 )
 870     CONTINUE 
!
         CALL CLRCH ( STR )
         CALL INCH  ( J6, STR )
         DO 4110 J11=1,L_ARR
            IF ( C_ARR(J11)(1:1) == '!' ) THEN
                 CALL CHECK_WAIT ( C_ARR(J11), MESS )
                 IF ( ILEN(MESS) > 0 ) THEN
                      WRITE ( 6, '(A)' ) 'ERROR: wrong format of wait command '//TRIM(C_ARR(J11))// &
     &                                   ' at line '//TRIM(STR)//' -- of the snap '// &
     &                                   'file '//TRIM(FILS)//' : '//TRIM(MESS)
                      CALL EXIT ( 1 )
                 END IF
            END IF
            IF ( IVRB .GE. 3 ) THEN
                 WRITE ( 6, '(A)' ) TRIM(C_ARR(J11))
            END IF            
 4110    CONTINUE 
 460  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CHECK_PROC_DEF !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_WAIT ( COM, MESS )
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_WAIT checks the Field System wait command "!"        *
! *                                                                      *
! *  ### 26-JAN-2022  CHECK_WAIT  v1.0 (c)  L. Petrov  26-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  COM*(*), MESS*(*)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  IM, IS, IVAL
!
      CALL CLRCH  ( MESS )
      IF ( ILEN(COM) == 1 ) THEN
           MESS = '! command does not specify time'
           RETURN 
      END IF
      IF ( COM(2:2) .EQ. '+' ) THEN
           IM = INDEX ( COM, 'm' )
           IS = INDEX ( COM, 's' )
           IF ( IM > 0 ) THEN
                IF ( IM > IS ) THEN
                     MESS = 'minutes field follows seconds field'
                     RETURN 
                END IF
                IF ( IM .EQ. 3 ) THEN
                     MESS = 'minute field is empty'
                     RETURN 
                  ELSE IF ( IM .GT. 3 ) THEN
                     CALL CHIN ( COM(2:IM-1), IVAL )
                     IF ( IVAL .LT. 0 .OR. IVAL .GT. 59 ) THEN
                          MESS = 'minutes field should be in a range of [0, 59]'
                          RETURN 
                     END IF
                     IF ( IS == IM+1 ) THEN
                          MESS = 'Wrong wait command '//TRIM(COM)// &
     &                           ' -- seconds field is empty'
                          RETURN 
                       ELSE IF ( IS > IM+1 ) THEN
                          CALL CHIN ( COM(IM+1:IS-1), IVAL )
                          IF ( IVAL .LT. 0 .OR. IVAL .GT. 59 ) THEN
                               MESS = 'seconds field should be in a range of [0, 59]'
                               RETURN 
                          END IF
                     END IF
                END IF
              ELSE 
                IF ( IS < 3 ) THEN
                     MESS = 'Wrong wait command '//TRIM(COM)// &
     &                           ' -- second field is empty'
                     RETURN 
                   ELSE
                     CALL CHIN ( COM(2:IS-1), IVAL )
                     IF ( IVAL .LT. 1 .OR. IVAL .GT. 59 ) THEN
                          MESS = 'seconds field should be in a range of [1, 59]'
                          RETURN 
                     END IF
                END IF
           END IF
         ELSE IF ( COM(6:6)   == '.' .AND. &
     &             COM(10:10) == '.' .AND. &
     &             COM(13:13) == ':' .AND. &
     &             COM(16:16) == ':'       ) THEN
           CALL CHIN ( COM(2:5),   IVAL )
           IF ( IVAL .LT. 2000 .OR. IVAL .GT. 2099 ) THEN
                MESS = 'year field should be in a range of [2000, 2099]'
                RETURN 
           END IF
!
           CALL CHIN ( COM(7:9),   IVAL )
           IF ( IVAL .LT. 1 .OR. IVAL .GT. 366 ) THEN
                MESS = 'day of year field should be in a range of [1, 366]'
                RETURN 
           END IF
!
           CALL CHIN ( COM(11:12), IVAL )
           IF ( IVAL .LT. 0 .OR. IVAL .GT. 24 ) THEN
                MESS = 'hour field should be in a range of [0, 24]'
                RETURN 
           END IF
!
           CALL CHIN ( COM(14:15), IVAL )
           IF ( IVAL .LT. 0 .OR. IVAL .GT. 69 ) THEN
                MESS = 'minute field should be in a range of [0, 59]'
                RETURN 
           END IF
!
           CALL CHIN ( COM(17:18), IVAL )
           IF ( IVAL .LT. 0 .OR. IVAL .GT. 69 ) THEN
                MESS = 'second field should be in a range of [0, 59]'
                RETURN 
           END IF
         ELSE
           MESS = 'unrecognsed format: it should have + or date'
      END IF
!
      RETURN
      END  SUBROUTINE  CHECK_WAIT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FS_TO_DATE ( STR, MJD, SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FS_TO_DATE
! *                                                                      *
! *  ### 28-JAN-2022   FS_TO_DATE  v1.0 (c)  L. Petrov  28-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STR*(*)
      INTEGER*4  MJD, IUER 
      REAL*8     SEC
      INTEGER*4  DOY, IER 
      CHARACTER  DATE_STR*19
!
      DATE_STR = STR(2:5)//'.01.01_'//STR(11:18)
      CALL DATE_TO_TIME ( DATE_STR, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IER, IUER )
      END IF
      CALL CHIN ( STR(7:9), DOY )
      IF ( DOY < 1 .OR. DOY > 366 ) THEN
           CALL ERR_LOG ( 2921, IUER, 'FS_TO_DATE', 'Wrong DOY '// &
     &         'field in '//STR )
           RETURN 
      END IF
      MJD = MJD + DOY - 1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FS_TO_DATE  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_AZEL ( NERS, NS, BUFS, FILS, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CHECK_AZEL
! *                                                                      *
! *  ### 27-JAN-2022   CHECK_AZEL  v1.0 (c)  L. Petrov  27-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE       ( NERS__TYPE ) :: NERS
      INTEGER*4  NS, IVRB, IUER 
      CHARACTER  FILS*(*), BUFS(NS)*(*)
      INTEGER*4  MIND, MHOR
      PARAMETER  ( MIND = 128 )
      PARAMETER  ( MHOR = 360 )
      CHARACTER  SOU_NAM*8, SCAN_NAM*10, STA_NAM*8, RA_STR*13, DEC_STR*13, &
     &           AZ_STR*10, EL_STR*10, FRAME_STR*8, WRAP_STR*3, STR*128, &
     &           DATE_PRE*21, DATE_POS*21, WRAP_LAST*3, SLEW_TYPE*2
      REAL*8     UTC_M_TAI, UTC_PREOB, UTC_POSTOB, AZ(2), EL(2), &
     &           HA, AZ_RATE, EL_RATE, HA_RATE, COO_TRS(3), RA, DEC, &
     &           HOR_MASK_AZ(MHOR), HOR_MASK_EL(MHOR), EL_RANGE(2), &
     &           AZ_RANGE(4), SLEW_RATE(2), SLEW_ACCL(2), TSETTLE(2), &
     &           ARC_AZ, ARC_EL, SLEW_TIME
      LOGICAL*1  FL_HM, FL_AZLIM
      INTEGER*4  LIND, IND(2,MIND), N_SCA, MJD_PREOB, MJD_POSTOB, &
     &           N_HAZ, N_HEL, IS, J1, J2, J3, J4, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1, EXTERNAL :: HOR_MASK_OK, AZLIM_OK
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' ' 
           WRITE ( 6, '(A)' ) 'CHECK_AZEL'
      END IF
      UTC_M_TAI = 0.0D0
      N_SCA = 0
      N_HEL = 0
      N_HAZ = 0
      EL_RANGE = -1.001D31
      AZ_RANGE = -1.001D31
      WRAP_LAST = '???'
      DO 410 J1=1,NS
         CALL EXWORD ( BUFS(J1), MIND, LIND, IND, CHAR(9)//' ,=', IER )
         IF ( BUFS(J1)(1:10) == '" Station:' ) THEN
              STA_NAM = BUFS(J1)(IND(1,3):IND(2,3))
         END IF
         IF ( BUFS(J1)(1:14) == '" Coordinates:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F13.3)' ) COO_TRS(1)
              READ ( UNIT=BUFS(J1)(IND(1,5):IND(2,5)), FMT='(F13.3)' ) COO_TRS(2)
              READ ( UNIT=BUFS(J1)(IND(1,6):IND(2,6)), FMT='(F13.3)' ) COO_TRS(3)
         END IF
!
         IF ( BUFS(J1)(1:24) == '" 1st_axis_slewing_rate:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_RATE(1)
              SLEW_RATE(1) = SLEW_RATE(1)*DEG__TO__RAD
         END IF
         IF ( BUFS(J1)(1:24) == '" 2nd_axis_slewing_rate:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_RATE(2)
              SLEW_RATE(2) = SLEW_RATE(2)*DEG__TO__RAD
         END IF
         IF ( BUFS(J1)(1:24) == '" 1st_axis_slewing_accl:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_ACCL(1)
              SLEW_ACCL(1) = SLEW_ACCL(1)*DEG__TO__RAD
         END IF
         IF ( BUFS(J1)(1:24) == '" 2nd_axis_slewing_accl:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) SLEW_ACCL(2)
              SLEW_ACCL(2) = SLEW_ACCL(2)*DEG__TO__RAD
         END IF
         IF ( BUFS(J1)(1:24) == '" 1st_axis_settle_time: ' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) TSETTLE(1)
         END IF
         IF ( BUFS(J1)(1:24) == '" 2nd_axis_settle_time: ' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) TSETTLE(2)
         END IF
!
         IF ( BUFS(J1)(1:14) == '" AZ_hor_mask:' ) THEN
              N_HAZ = LIND - 3
              DO 420 J2=1,N_HAZ
                 READ ( UNIT=BUFS(J1)(IND(1,J2+3):IND(2,J2+3)), FMT='(F6.2)' ) HOR_MASK_AZ(J2)
 420          CONTINUE 
              HOR_MASK_AZ = HOR_MASK_AZ*DEG__TO__RAD
         END IF
!
         IF ( BUFS(J1)(1:17) == '" 1st_axis_range:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) AZ_RANGE(1)
              READ ( UNIT=BUFS(J1)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) AZ_RANGE(2)
              READ ( UNIT=BUFS(J1)(IND(1,6):IND(2,6)), FMT='(F6.2)' ) AZ_RANGE(3)
              READ ( UNIT=BUFS(J1)(IND(1,7):IND(2,7)), FMT='(F6.2)' ) AZ_RANGE(4)
              AZ_RANGE = AZ_RANGE*DEG__TO__RAD
         END IF
!
         IF ( BUFS(J1)(1:17) == '" 2nd_axis_range:' ) THEN
              READ ( UNIT=BUFS(J1)(IND(1,4):IND(2,4)), FMT='(F6.2)' ) EL_RANGE(1)
              READ ( UNIT=BUFS(J1)(IND(1,5):IND(2,5)), FMT='(F6.2)' ) EL_RANGE(2)
              EL_RANGE = EL_RANGE*DEG__TO__RAD
         END IF
!
         IF ( BUFS(J1)(1:14) == '" EL_hor_mask:' ) THEN
              N_HEL = LIND - 3
              DO 430 J3=1,N_HEL
                 READ ( UNIT=BUFS(J1)(IND(1,J3+3):IND(2,J3+3)), FMT='(F6.2)' ) HOR_MASK_EL(J3)
 430          CONTINUE 
              IF ( N_HEL < 2 ) THEN
                   CALL CLRCH (     STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3131, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                 'the horizontal mask at line '//TRIM(STR)//' -- the '// &
     &                 'number of elements should be at least 2' )
                   RETURN 
              END IF
              HOR_MASK_EL = HOR_MASK_EL*DEG__TO__RAD
!
              IF ( N_HEL .NE. N_HAZ ) THEN
                   CALL CLRCH (     STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3132, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                 'the horizontal mask at line '//TRIM(STR)//' -- '// &
     &                 'mismatch of the number elements for azimith and elevation' )
                   RETURN 
              END IF
         END IF
!
         IF ( BUFS(J1)(1:10) == 'scan_name=' ) THEN
              IF ( N_HEL < 2 ) THEN
                   CALL ERR_LOG ( 3133, IUER, 'CHECK_AZEL', 'Did not find '// &
     &                 'the horizontal mask definitions in snap file '//FILS )
                   RETURN 
              END IF
!
              IF ( AZ_RANGE(1) < -1.D15 ) THEN
                   CALL ERR_LOG ( 3134, IUER, 'CHECK_AZEL', 'Azimuth range '// &
     &                 'definition was not found in snap file '//FILS )
                   RETURN 
              END IF
!
              IF ( EL_RANGE(1) < -1.D15 ) THEN
                   CALL ERR_LOG ( 3135, IUER, 'CHECK_AZEL', 'Elevation range '// &
     &                 'definition was not found in snap file '//FILS )
                   RETURN 
              END IF
!
              SCAN_NAM = BUFS(J1)(IND(1,2):IND(2,2))
              N_SCA = N_SCA + 1
         END IF
!
         IF ( BUFS(J1)(1:7) == 'source=' ) THEN
              SOU_NAM = BUFS(J1)(IND(1,2):IND(2,2))
              RA_STR  = BUFS(J1)(IND(1,3):IND(2,3))
              DEC_STR = BUFS(J1)(IND(1,4):IND(2,4))
              FRAME_STR = BUFS(J1)(IND(1,5):IND(2,5))
              IF ( LIND .GE. 6 ) THEN
                   WRAP_STR  = BUFS(J1)(IND(1,6):IND(2,6))
                   RA_STR  = RA_STR(1:2)//':'//RA_STR(3:4)//':'//RA_STR(5:)
                   DEC_STR = DEC_STR(1:3)//':'//DEC_STR(4:5)//':'//DEC_STR(6:)
                 ELSE 
                   FRAME_STR = 'none'
                   WRAP_STR  = BUFS(J1)(IND(1,5):IND(2,5))
              END IF
!
              IF ( IVRB .GE. 2 ) THEN
                   IF ( IVRB .GE. 2 ) WRITE ( 6, '(A)' ) ' ' 
                   WRITE ( 6, 110  ) N_SCA, SCAN_NAM, SOU_NAM, RA_STR, DEC_STR, FRAME_STR, WRAP_STR
 110               FORMAT ( I5, ') Scan: ', A, ' Sou: ', A, '  Coo: ', A, 1X, A, &
     &                      '  Frame: ', A, ' Wrap: ', A )
              END IF
!
              IF ( WRAP_STR == 'neu' .OR. &
     &             WRAP_STR == 'cw'  .OR. &
     &             WRAP_STR == 'ccw'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3136, IUER, 'CHECK_AZEL', 'Error in '// &
     &                 'pasing cable wrap designator at line '//TRIM(STR)// &
     &                 ' -- '//WRAP_STR//' is not recognized' )
                   RETURN 
              END IF
!
              IF ( SOU_NAM .NE. 'azel' ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL HR_TAT   ( RA_STR,  RA,  IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (     STR ) 
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 3137, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                      'source right ascension at line '//TRIM(STR)// &
     &                      ' of the snap file '//FILS )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL GR_TAT   ( DEC_STR, DEC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (     STR ) 
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 3138, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                      'source declination at line '//TRIM(STR)// &
     &                      ' of the snap file '//FILS )
                        RETURN 
                   END IF
                ELSE
                   RA  = 0.0D0
                   DEC = 0.0D0
                   AZ_STR = BUFS(J1)(IND(1,3):IND(2,3)-1)
                   EL_STR = BUFS(J1)(IND(1,4):IND(2,4)-1)
                   READ ( UNIT=AZ_STR, FMT='(F15.7)' ) AZ(1)
                   READ ( UNIT=EL_STR, FMT='(F15.7)' ) EL(1)
                   AZ(1) = AZ(1)*DEG__TO__RAD
                   EL(1) = EL(1)*DEG__TO__RAD
                   AZ(2) = AZ(1)
                   EL(2) = EL(1)
                   HA = 66
              END IF
         END IF
!
         IF ( BUFS(J1)(1:5) == 'preob' ) THEN
              IS = J1-1
              DO 440 J4=J1-1,1,-1
                 IF ( BUFS(J4)(1:1) == '!' ) THEN
                      IS = J4
                      GOTO 840
                 END IF
 440          CONTINUE 
 840          CONTINUE 
              CALL ERR_PASS ( IUER, IER )
              CALL FS_TO_DATE ( BUFS(IS)(1:18), MJD_PREOB, UTC_PREOB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3139, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                 'date at line '//TRIM(STR)//' of the snap file '//FILS )
                   RETURN 
              END IF
!
              IF ( UTC_M_TAI == 0.0D0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_GET_UTCMTAI ( NERS, ((MJD_PREOB-J2000__MJD)*86400.0D0 + UTC_PREOB), &
     &                                     UTC_M_TAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 3140, IUER, 'CHECK_AZEL', 'Error in computation '// &
     &                      'of UTC minus TAI' )
                        RETURN 
                   END IF
              END IF
!
              IF ( SOU_NAM .NE. 'azel' ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_AZELHA_COMP ( NERS, (MJD_PREOB-J2000__MJD)*86400.0D0 + UTC_PREOB - UTC_M_TAI, &
     &                                     COO_TRS, RA, DEC, &
     &                                     'radio', AZ(1), EL(1), &
     &                                     HA, AZ_RATE, EL_RATE, HA_RATE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (     STR ) 
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 3141, IUER, 'CHECK_AZEL', 'Error in '// &
     &                      'computation of azimuth' ) 
                        RETURN 
                   END IF
              END IF
!
              IF ( N_SCA > 1 ) THEN
                   CALL GET_SLEW_TIME ( AZ(2), EL(2), WRAP_LAST, &
     &                                  AZ(1), EL(1), WRAP_STR,  &
     &                                  SLEW_RATE, SLEW_ACCL, TSETTLE, AZ_RANGE, &
     &                                  STA_NAM, ARC_AZ, ARC_EL, SLEW_TYPE, SLEW_TIME )
                    IF ( IVRB .GE. 3 ) THEN
                         WRITE  ( 6, 120 ) ARC_AZ/DEG__TO__RAD, ARC_EL/DEG__TO__RAD, &
     &                                     SLEW_TIME, SLEW_TYPE
 120                     FORMAT ( 24X, 'SLEW:  AZ_arc: ', F6.2, ' deg,  EL_arc: ', &
     &                            F5.2, ' deg, Slew time: ', F5.1, ' sec, Slew type: ', A )
                    END IF
              END IF
         END IF
!
         IF ( BUFS(J1)(1:6) == 'postob' ) THEN
              CALL ERR_PASS ( IUER, IER )
              IF ( BUFS(J1-1)(1:1) == '!' ) THEN
                   CALL FS_TO_DATE ( BUFS(J1-1)(1:18), MJD_POSTOB, UTC_POSTOB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (     STR ) 
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 3142, IUER, 'CHECK_AZEL', 'Error in parsing '// &
     &                      'date at line '//TRIM(STR)//' of the snap file '//FILS )
                        RETURN 
                   END IF
                 ELSE
                   MJD_POSTOB = MJD_PREOB
                   UTC_POSTOB = UTC_PREOB
              END IF
              IF ( SOU_NAM .NE. 'azel' ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_AZELHA_COMP ( NERS, (MJD_POSTOB-J2000__MJD)*86400.0D0 + UTC_POSTOB - UTC_M_TAI, &
     &                                     COO_TRS, RA, DEC, &
     &                                     'radio', AZ(2), EL(2), &
     &                                     HA, AZ_RATE, EL_RATE, HA_RATE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH (     STR ) 
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 3143, IUER, 'CHECK_AZEL', 'Error in '// &
     &                      'computation of azimuth' ) 
                        RETURN 
                   END IF
              END IF
!
              DATE_PRE = MJDSEC_TO_DATE ( MJD_PREOB,  UTC_PREOB,  IER )
              DATE_POS = MJDSEC_TO_DATE ( MJD_POSTOB, UTC_POSTOB, IER )
              IF ( IVRB .GE. 3 ) THEN
                   WRITE ( 6, 130 ) DATE_PRE, AZ(1)/DEG__TO__RAD, EL(1)/DEG__TO__RAD, &
     &                              DATE_POS, AZ(2)/DEG__TO__RAD, EL(2)/DEG__TO__RAD
 130               FORMAT ( 13X,  'Preob: ', A, ' Az/El: ', F8.4, 1X, F8.4, &
     &                       ' || Postob: ', A, ' Az/El: ', F8.4, 1X, F8.4 )
              END IF          
!
! ----------- Check the elevation range and the horizon mask for PREOB
!
              FL_HM = HOR_MASK_OK ( AZ(1), EL(1), N_HAZ, HOR_MASK_AZ, HOR_MASK_EL, EL_RANGE )
              IF ( .NOT. FL_HM ) THEN
                   WRITE ( 6, 140 ) N_SCA, DATE_PRE, AZ(1)/DEG__TO__RAD, EL(1)/DEG__TO__RAD
 140               FORMAT  ( 'ERROR: Scan ', I5, ' Date: ', A, ' Az= ', F8.4, ' El= ' F8.4, &
     &                       ' does not satsity elevation range or the horizon mask' )
                   CALL EXIT ( 1 )                   
              END IF
!
! ----------- Check the elevation range and the horizon mask for POSTOB
!
              FL_HM = HOR_MASK_OK ( AZ(2), EL(2), N_HAZ, HOR_MASK_AZ, HOR_MASK_EL, EL_RANGE )
              IF ( .NOT. FL_HM ) THEN
                   WRITE ( 6, 140 ) N_SCA, DATE_POS, AZ(2)/DEG__TO__RAD, EL(2)/DEG__TO__RAD
                   CALL EXIT ( 1 )                   
              END IF
!
              FL_AZLIM = AZLIM_OK ( AZ(1), AZ_RANGE, WRAP_STR )
              IF ( .NOT. FL_AZLIM ) THEN
                   WRITE ( 6, 150 ) N_SCA, DATE_PRE, AZ(1)/DEG__TO__RAD 
 150               FORMAT  ( 'ERROR: Scan ', I5, ' Date: ', A, ' Az= ', F8.4, &
     &                       ' is out of azimuth rante and tears the cable wrap' )
                   CALL EXIT ( 1 )                   
              END IF
!
              FL_AZLIM = AZLIM_OK ( AZ(2), AZ_RANGE, WRAP_STR )
              IF ( .NOT. FL_AZLIM ) THEN
                   WRITE ( 6, 150 ) N_SCA, DATE_POS, AZ(2)/DEG__TO__RAD 
                   CALL EXIT ( 1 )                   
              END IF
!
              WRAP_LAST = WRAP_STR
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE CHECK_AZEL !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   HOR_MASK_OK ( AZ, EL, N_HAZ, HOR_MASK_AZ, HOR_MASK_EL, &
     &                         EL_RANGE )
! ************************************************************************
! *                                                                      *
! *   Routine  HOR_MASK_OK 
! *                                                                      *
! *  ### 28-JAN-2022  HOR_MASK_OK  v1.0 (c)  L. Petrov  28-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      LOGICAL*1  HOR_MASK_OK
      INTEGER*4  N_HAZ
      REAL*8     AZ, EL, HOR_MASK_AZ(N_HAZ), HOR_MASK_EL(N_HAZ), EL_RANGE(2)
      INTEGER*4  J1
!
      HOR_MASK_OK = .TRUE.
      IF ( EL .LE. EL_RANGE(1) ) HOR_MASK_OK = .FALSE.
      IF ( EL .GE. EL_RANGE(2) ) HOR_MASK_OK = .FALSE.
!
!%   write ( 6, * ) 'n_haz=', n_haz, ' haz= ', sngl(HOR_MASK_AZ(1:n_haz)) ! %%%%
!%   write ( 6, * ) 'n_has=', n_haz, ' ele= ', sngl(HOR_MASK_EL(1:n_haz)) ! %%%%
      DO 410 J1=1,N_HAZ-1
         IF ( AZ .GE. HOR_MASK_AZ(J1) .AND. AZ .LT. HOR_MASK_AZ(J1+1) ) THEN
              IF ( EL .LE. HOR_MASK_EL(J1) ) THEN
                   HOR_MASK_OK = .FALSE.
!%   write ( 6, * ) 'H1 az= ', az, ' haz ', HOR_MASK_AZ(J1), HOR_MASK_AZ(J1+1), ' j1= ', j1 !%%%
!%   write ( 6, * ) 'H2 el= ', el, ' hel ', HOR_MASK_EL(J1)  ! %%%%
              END IF
         END IF
 410  CONTINUE 
!
      RETURN
      END  FUNCTION   HOR_MASK_OK  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   AZLIM_OK ( AZ, AZ_RANGE, WRAP )
! ************************************************************************
! *                                                                      *
! *   Routine  AZLIM_OK
! *                                                                      *
! *  ### 28-JAN-2022    AZLIM_OK   v1.0 (c)  L. Petrov  28-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      LOGICAL*1  AZLIM_OK
      REAL*8     AZ, AZ_RANGE(4)
      CHARACTER  WRAP*(*)
      REAL*8     AZM, AZP
!
      AZM = AZ - PI2
      AZP = AZ + PI2
      AZLIM_OK = .FALSE.
!
      IF ( WRAP == 'ccw' ) THEN
           IF ( AZ         .GE. AZ_RANGE(1) .AND. AZ  .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(1) .AND. AZM .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(1) .AND. AZP .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(1) .AND. AZM .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(1) .AND. AZ  .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(1) .AND. AZP .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(1) .AND. AZM .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(1) .AND. AZ  .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(1) .AND. AZP .LT. RANGE(2) ) THEN
                AZLIM_OK = .TRUE.
           END IF
      END IF 
!
      IF ( WRAP == 'neu' ) THEN
           IF ( AZ         .GE. AZ_RANGE(2) .AND. AZ  .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(2) .AND. AZM .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(2) .AND. AZP .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(2) .AND. AZM .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(2) .AND. AZ  .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(2) .AND. AZP .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(2) .AND. AZM .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(2) .AND. AZ  .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(2) .AND. AZP .LT. RANGE(3) ) THEN
                AZLIM_OK = .TRUE.
           END IF
      END IF 
!
      IF ( WRAP == 'cw' ) THEN
           IF ( AZ         .GE. AZ_RANGE(3) .AND. AZ  .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(3) .AND. AZM .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZ .GE.  AZ_RANGE(3) .AND. AZP .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(3) .AND. AZM .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(3) .AND. AZ  .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZM .GE. AZ_RANGE(3) .AND. AZP .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(3) .AND. AZM .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(3) .AND. AZ  .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
             ELSE IF ( AZP .GE. AZ_RANGE(3) .AND. AZP .LT. RANGE(4) ) THEN
                AZLIM_OK = .TRUE.
           END IF
      END IF 
!
      RETURN
      END  FUNCTION   AZLIM_OK  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_SLEW_TIME ( AZ_OLD, EL_OLD, WRAP_OLD, &
     &                           AZ_NEW, EL_NEW, WRAP_NEW, &
     &                           SLEW_RATE, SLEW_ACCL, TSETTLE, AZ_RANGE, &
     &                           STA_NAM, ARC_AZ, ARC_EL, SLEW_TYPE, &
     &                           SLEW_TIME )
! ************************************************************************
! *                                                                      *
! *   Routine GET_SLEW_TIME computes slewing time.                       *
! *                                                                      *
! *  ### 28-JAN-2022  GET_SLEW_TIME  v1.0 (c) L. Petrov  28-JAN-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      CHARACTER  WRAP_OLD*(*), WRAP_NEW*(*), STA_NAM*(*), SLEW_TYPE*(*)
      REAL*8     AZ_OLD, EL_OLD, &
     &           AZ_NEW, EL_NEW, &
     &           SLEW_RATE(2), SLEW_ACCL(2), TSETTLE(2), AZ_RANGE(4), &
     &           ARC_AZ, ARC_EL, SLEW_TIME
      REAL*8     DST_MARGIN
      PARAMETER  ( DST_MARGIN = 2.0D0/DEG__TO__RAD )
      REAL*8     AZIM_OLD, AZIM_NEW, DIST_AZ_ACCEL, DIST_EL_ACCEL, &
     &           SLEW_TIME_AZ, SLEW_TIME_EL
!
      IF ( WRAP_OLD == 'ccw' ) THEN
          IF ( AZ_OLD > AZ_RANGE(2) ) THEN
               AZIM_OLD = AZ_OLD - PI2
             ELSE IF ( AZ_OLD < AZ_RANGE(1) ) THEN
               AZIM_OLD = AZ_OLD + PI2
             ELSE
               AZIM_OLD = AZ_OLD
           END IF
         ELSE IF ( WRAP_OLD == 'neu' ) THEN
           IF ( AZ_OLD > AZ_RANGE(3) ) THEN
               AZIM_OLD = AZ_OLD - PI2
             ELSE IF ( AZ_OLD < AZ_RANGE(2) ) THEN
               AZIM_OLD = AZ_OLD + PI2
             ELSE
               AZIM_OLD = AZ_OLD
           END IF
         ELSE IF ( WRAP_OLD == 'cw' ) THEN
           IF ( AZ_OLD > AZ_RANGE(4) ) THEN
               AZIM_OLD = AZ_OLD - PI2
             ELSE IF ( AZ_OLD < AZ_RANGE(3) ) THEN
               AZIM_OLD = AZ_OLD + PI2
             ELSE
               AZIM_OLD = AZ_OLD
           END IF
      END IF
!
      IF ( WRAP_NEW == 'ccw' ) THEN
           IF ( AZ_NEW > AZ_RANGE(2) ) THEN
                AZIM_NEW = AZ_NEW - PI2
             ELSE IF ( AZ_NEW < AZ_RANGE(1) ) THEN
                AZIM_NEW = AZ_NEW + PI2
             ELSE
               AZIM_NEW = AZ_NEW
           END IF
         ELSE IF ( WRAP_NEW == 'neu' ) THEN
           IF ( AZ_NEW > AZ_RANGE(3) ) THEN
               AZIM_NEW = AZ_NEW - PI2
             ELSE IF ( AZ_NEW < AZ_RANGE(2) ) THEN
               AZIM_NEW = AZ_NEW + PI2
             ELSE
               AZIM_NEW = AZ_NEW
           END IF
         ELSE IF ( WRAP_NEW == 'cw' ) THEN
           IF ( AZ_NEW > AZ_RANGE(4) ) THEN
               AZIM_NEW = AZ_NEW - PI2
             ELSE IF ( AZ_NEW < AZ_RANGE(3) ) THEN
               AZIM_NEW = AZ_NEW + PI2
             ELSE
               AZIM_NEW = AZ_NEW
           END IF
      END IF
!
      ARC_AZ = DABS ( AZIM_NEW - AZIM_OLD )
      DIST_AZ_ACCEL = SLEW_RATE(1)**2/SLEW_ACCL(1)
      IF ( ARC_AZ > DIST_AZ_ACCEL ) THEN
           SLEW_TIME_AZ = ABS(ARC_AZ)/SLEW_RATE(1) + SLEW_RATE(1)/SLEW_ACCL(1)
         ELSE
           SLEW_TIME_AZ =  2.0D0 * DSQRT ( ABS(ARC_AZ)/SLEW_ACCL(1) )
      END IF
!
      ARC_EL = DABS ( EL_NEW - EL_OLD )
      DIST_EL_ACCEL = SLEW_RATE(2)**2/SLEW_ACCL(2)
      IF ( ARC_EL > DIST_EL_ACCEL ) THEN
           SLEW_TIME_EL = ABS(ARC_EL)/SLEW_RATE(2) + SLEW_RATE(2)/SLEW_ACCL(2)
         ELSE
           SLEW_TIME_EL =  2.0D0 * DSQRT ( ABS(ARC_EL)/SLEW_ACCL(2) )
      END IF
!
      IF ( SLEW_TIME_AZ + TSETTLE(1) > SLEW_TIME_EL + TSETTLE(2) ) THEN
           SLEW_TYPE = 'az'
           SLEW_TIME = SLEW_TIME_AZ + TSETTLE(1)
         ELSE
           SLEW_TYPE = 'el'
           SLEW_TIME = SLEW_TIME_EL + TSETTLE(2)
      END IF
!
      RETURN
      END  SUBROUTINE  GET_SLEW_TIME  !#!#
