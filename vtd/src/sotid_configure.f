      PROGRAM    SOTID_CONFIGURE
! ************************************************************************
! *                                                                      *
! *    Program  SOTID_CONFIGURE parses sotid.cnf file and creates actual *
! *  include file on the basis of template files.                        *
! *                                                                      *
! * ### 12-JUL-2002  SOTID_CONFIGURE v1.0 (c)  L. Petrov 15-JUL-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 512 )
      INTEGER*4  L_MAK, L_HW
      PARAMETER  ( L_MAK = 9, L_HW = 5 )
      CHARACTER  CNF_FILE*128
      CHARACTER  CNF_BUF(MBUF)*128, BUF(MBUF)*128, CIS_MAK(L_MAK)*16, &
     &           CIS_HW(L_HW)*64
      LOGICAL*4  FL_MAX_STA, FL_NAM_LEN, FL_2D, FL_3D, FL_FREQ_RES, &
     &           FL_MAK(L_MAK), FL_HW(L_HW)
      DATA          CIS_MAK  &
     &           / &
     &             'FOPT            ', &
     &             'LD_OPT          ', &
     &             'BIN_DIR         ', &
     &             'LIB_DIR         ', &
     &             'INC_DIR         ', &
     &             'MAN_DIR         ', &
     &             'DOC_DIR         ', &
     &             'CCOMP           ', &
     &             'LIB_F90         '  &
     &           /
      DATA          CIS_HW  &
     &           / &
     &             'HW_FILE         ', &
     &             'HW1_OUTFIL      ', &
     &             'HW2_OUTFIL      ', &
     &             'HW3_OUTFIL      ', &
     &             'HW_INC_FORMAT   '  &
     &           /
      INTEGER*4  NBUF, CNF_NBUF, IC, IP, IPA, ILA, IPC, IPE, IPI, IPQ, ILQ, &
     &           IPD, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12,    &
     &           J13, J14, J15, J16, IUER
      INTEGER*4  I_LEN, LINDEX
!
      CNF_FILE = 'sotid.cnf'
!
! --- Read configuration file
!
      IUER = -1
      CALL SOTID_RD_TEXT ( CNF_FILE, MBUF, CNF_BUF, CNF_NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read template file for sotid_type.i
!
      IUER = -1
      CALL SOTID_RD_TEXT ( 'sotid_type.in', MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Parse template file and configuration file
!
      FL_MAX_STA = .FALSE.
      FL_NAM_LEN = .FALSE.
      DO 410 J1=1,NBUF
         IPA = INDEX ( BUF(J1), '@' ) 
         IF ( IPA .GT. 1 ) THEN
              DO 420 J2=1,CNF_NBUF
                 IF ( CNF_BUF(J2)(1:1) .EQ. '#' ) GOTO 420
!
! -------------- Search for SOTID__MAX_STA stuff
!
                 IPE = INDEX ( CNF_BUF(J2), '=' )
                 IPQ = INDEX ( CNF_BUF(J2), "'" )
                 IF ( IPQ .LE. 0 ) IPQ = INDEX ( CNF_BUF(J2), '"' )
                 IPI  = INDEX ( BUF(J1),     'SOTID__MAX_STA' ) 
                 IPC  = INDEX ( CNF_BUF(J2), 'SOTID__MAX_STA' ) 
                 IF ( IPC .GT. 0  .AND.  IPI .GT. 0  .AND.  IPE .GT. 0  ) THEN
                      CALL CLRCH  ( CNF_BUF(J2)(1:IPE) )
                      CALL CHASHL ( CNF_BUF(J2)        )
                      BUF(J1) = BUF(J1)(1:IPA-1)// &
     &                          CNF_BUF(J2)(1:I_LEN(CNF_BUF(J2)))// &
     &                          BUF(J1)(IPA+1:)
                      FL_MAX_STA = .TRUE.
                 END IF
!
! -------------- Search for SOTID__NAM_LEN stuff
!
                 IPI  = INDEX ( BUF(J1),     'SOTID__NAM_LEN' ) 
                 IPC  = INDEX ( CNF_BUF(J2), 'SOTID__NAM_LEN' ) 
                 IF ( IPC .GT. 0  .AND.  IPI .GT. 0  .AND.  IPE .GT. 0  ) THEN
                      CALL CLRCH  ( CNF_BUF(J2)(1:IPE) )
                      CALL CHASHL ( CNF_BUF(J2)        )
                      BUF(J1) = BUF(J1)(1:IPA-1)// &
     &                          CNF_BUF(J2)(1:I_LEN(CNF_BUF(J2)))// &
     &                          BUF(J1)(IPA+1:)
                      FL_NAM_LEN = .TRUE.
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
! --- Check whether all parameters were found
!
      IUER = -1
      IF ( .NOT. FL_MAX_STA ) THEN
           CALL SOTID_ERR_LOG ( 5811, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter SOTID__MAX_STA was not found.'// &
     &         ' Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
      IF ( .NOT. FL_NAM_LEN ) THEN
           CALL SOTID_ERR_LOG ( 5812, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter SOTID__NAM_LEN was not found.'// &
     &         ' Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
!
! --- Write down updated file
!
      IUER = -1
      CALL SOTID_WR_TEXT ( NBUF, BUF, 'sotid_type.i', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read template file for sotid_data.i
!
      IUER = -1
      CALL SOTID_RD_TEXT ( 'sotid_data.in', MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      FL_2D = .FALSE.
      FL_3D = .FALSE.
      DO 430 J3=1,NBUF
         IPA =  INDEX ( BUF(J3), '@' ) 
         ILA = LINDEX ( BUF(J3), '@' ) 
         IF ( IPA .GT. 1  ) THEN
              DO 440 J4=1,CNF_NBUF
                 IF ( CNF_BUF(J4)(1:1) .EQ. '#' ) GOTO 440
                 IPE = INDEX ( CNF_BUF(J4), '=' )
                 IPQ = INDEX ( CNF_BUF(J4), "'" )
                 IF ( IPQ .LE. 0 ) IPQ = INDEX ( CNF_BUF(J4), '"' )
                 ILQ = LINDEX ( CNF_BUF(J4), "'" )
                 IF ( ILQ .LE. 0 ) ILQ = LINDEX ( CNF_BUF(J4), '"' )
                 IPI  =  INDEX ( BUF(J3),     '@HW95_2D_INC@' ) 
                 IPC  =  INDEX ( CNF_BUF(J4),  'HW95_2D_INC'  ) 
                 IF ( IPC .GT. 0  .AND.  &
     &                IPI .GT. 0  .AND.  &
     &                IPE .GT. 0  .AND.  &
     &                IPQ .GT. 0  .AND.  &
     &                ILQ .GT. 0            ) THEN
!
                      CALL CLRCH  ( CNF_BUF(J4)(ILQ:) )
                      CALL CLRCH  ( CNF_BUF(J4)(1:IPQ) )
                      CALL CHASHL ( CNF_BUF(J4)        )
                      BUF(J3) = BUF(J3)(1:IPA-1)//"'"// &
     &                          CNF_BUF(J4)(1:I_LEN(CNF_BUF(J4)))//"'"// &
     &                          BUF(J3)(ILA+1:)
                      FL_2D = .TRUE.
                      FL_3D = .TRUE.
                 END IF
!!
                 IPE = INDEX ( CNF_BUF(J4), '=' )
                 IPQ = INDEX ( CNF_BUF(J4), "'" )
                 IF ( IPQ .LE. 0 ) IPQ = INDEX ( CNF_BUF(J4), '"' )
                 ILQ = LINDEX ( CNF_BUF(J4), "'" )
                 IF ( ILQ .LE. 0 ) ILQ = LINDEX ( CNF_BUF(J4), '"' )
                 IPI  =  INDEX ( BUF(J3),     '@HW95_3D_INC@' ) 
                 IPC  =  INDEX ( CNF_BUF(J4),  'HW95_3D_INC' ) 
                 IF ( IPC .GT. 0  .AND.  &
     &                IPI .GT. 0  .AND.  &
     &                IPE .GT. 0  .AND.  &
     &                IPQ .GT. 0  .AND.  &
     &                ILQ .GT. 0            ) THEN
!
                      CALL CLRCH  ( CNF_BUF(J4)(ILQ:) )
                      CALL CLRCH  ( CNF_BUF(J4)(1:IPQ) )
                      CALL CHASHL ( CNF_BUF(J4)        )
                      BUF(J3) = BUF(J3)(1:IPA-1)//"'"// &
     &                          CNF_BUF(J4)(1:I_LEN(CNF_BUF(J4)))//"'"// &
     &                          BUF(J3)(ILA+1:)
                      FL_2D = .TRUE.
                      FL_3D = .TRUE.
                 END IF
 440          CONTINUE 
         END IF
 430  CONTINUE 
!
! --- Check whether all parameters were found
!
      IUER = -1
      IF ( .NOT. FL_2D ) THEN
           CALL SOTID_ERR_LOG ( 5813, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter HW95_2D_INC was not found. '// &
     &         'Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
      IF ( .NOT. FL_3D ) THEN
           CALL SOTID_ERR_LOG ( 5814, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter HW95_3D_INC was not found. '// &
     &         'Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
!
! --- Write down updated file
!
      IUER = -1
      CALL SOTID_WR_TEXT ( NBUF, BUF, 'sotid_data.i', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read template for file for Makefile
!
      IUER = -1
      CALL SOTID_RD_TEXT ( 'Makefile.in', MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 450 J5=1,L_MAK
         FL_MAK(J5) = .FALSE.
 450  CONTINUE 
!
      DO 460 J6=1,NBUF
         IPA =  INDEX ( BUF(J6), '@' ) 
         ILA = LINDEX ( BUF(J6), '@' ) 
         IF ( IPA .GT. 1  ) THEN
              DO 470 J7=1,CNF_NBUF
                 IF ( CNF_BUF(J7)(1:1) .EQ. '#' ) GOTO 470
                 IPE = INDEX ( CNF_BUF(J7), '=' )
                 IPQ = INDEX ( CNF_BUF(J7), "'" )
                 IF ( IPQ .LE. 0 ) IPQ = INDEX ( CNF_BUF(J7), '"' )
                 ILQ = LINDEX ( CNF_BUF(J7), "'" )
                 IF ( ILQ .LE. 0 ) ILQ = LINDEX ( CNF_BUF(J7), '"' )
                 DO 480 J8=1,L_MAK
                    IPC  = INDEX ( CNF_BUF(J7), &
     &                     CIS_MAK(J8)(1:I_LEN(CIS_MAK(J8))) )
                    IPI  = INDEX ( BUF(J6), &
     &                     CIS_MAK(J8)(1:I_LEN(CIS_MAK(J8))) )
!
                    IF ( IPC .GT. 0  .AND.  &
     &                   IPE .GT. 0  .AND.  &
     &                   IPI .GT. 0  .AND.  &
     &                   IPQ .GT. 0  .AND.  &
     &                   ILQ .GT. 0            ) THEN
!
                         CALL CLRCH  ( CNF_BUF(J7)(ILQ:) )
                         CALL CLRCH  ( CNF_BUF(J7)(1:IPQ) )
                         CALL CHASHL ( CNF_BUF(J7)        )
                         BUF(J6) = BUF(J6)(1:IPA-1)// &
     &                          CNF_BUF(J7)(1:I_LEN(CNF_BUF(J7)))// &
     &                          BUF(J6)(ILA+1:)
                         FL_MAK(J8) = .TRUE.
                    END IF
 480             CONTINUE 
 470          CONTINUE 
         END IF
 460  CONTINUE 
!
! --- Check whether all parameters of the make file were replaced correctly
!
      IUER = -1
      DO 490 J9=1,L_MAK
         IF ( .NOT. FL_MAK(J9) ) THEN
              CALL SOTID_ERR_LOG ( 5815, IUER, 'SOTID_CONFIGURE', 'Error '// &
     &            'in configuration file: parameter '// &
     &             CIS_MAK(J9)(1:I_LEN(CIS_MAK(J9)))// &
     &            ' was not found. Check syntax, pleeeeease' )
              CALL EXIT ( 2 )
         END IF
 490  CONTINUE 
!
! --- ... and then write down updated makefile
!
      IUER = -1
      CALL SOTID_WR_TEXT ( NBUF, BUF, 'Makefile', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read template for file for hw95_out.i
!
      IUER = -1
      CALL SOTID_RD_TEXT ( 'hw95_out.in', MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 4100 J10=1,L_HW
         FL_HW(J10) = .FALSE.
 4100 CONTINUE 
      FL_FREQ_RES = .FALSE.
!
      DO 4110 J11=2,NBUF
         IPA =  INDEX ( BUF(J11), '@' ) 
         IF ( IPA .GT. 1  ) THEN
              DO 4120 J12=1,CNF_NBUF
                 IF ( CNF_BUF(J12)(1:1) .EQ. '#' ) GOTO 4120
                 IPE = INDEX ( CNF_BUF(J12), '=' )
                 IPQ = INDEX ( CNF_BUF(J12), "'" )
                 IF ( IPQ .LE. 0 ) IPQ = INDEX ( CNF_BUF(J12), '"' )
                 ILQ = LINDEX ( CNF_BUF(J12), "'" )
                 IF ( ILQ .LE. 0 ) ILQ = LINDEX ( CNF_BUF(J12), '"' )
!
! -------------- Look for hw95_out character constants
!
                 DO 4130 J13=1,L_HW
                    IPC  = INDEX ( CNF_BUF(J12), &
     &                     CIS_HW(J13)(1:I_LEN(CIS_HW(J13))) )
                    IPI  = INDEX ( BUF(J11-1), &
     &                     CIS_HW(J13)(1:I_LEN(CIS_HW(J13))) )
!
                    IF ( IPC .GT. 0  .AND.  &
     &                   IPE .GT. 0  .AND.  &
     &                   IPI .GT. 0  .AND.  &
     &                   IPQ .GT. 0  .AND.  &
     &                   ILQ .GT. 0            ) THEN
!
                         CALL CLRCH  ( CNF_BUF(J12)(ILQ:)  )
                         CALL CLRCH  ( CNF_BUF(J12)(1:IPQ) )
                         CALL CHASHL ( CNF_BUF(J12)        )
                         IC = I_LEN(CNF_BUF(J12))
                         IF ( IC .GT. 64 ) IC = 64
                         BUF(J11)(IPA:IPA+IC-1) = CNF_BUF(J12)(1:IC)
                         FL_HW(J13) = .TRUE.
                    END IF
 4130             CONTINUE 
!
! --------------- Look for the hw95 real*8 constant
!
                  IPC  = INDEX ( CNF_BUF(J12), 'FREQ_RES' ) 
                  IPI  = INDEX ( BUF(J11),     'FREQ_RES' ) 
                  IF ( IPC .GT. 0  .AND.  &
     &                 IPI .GT. 0  .AND.  &
     &                 IPE .GT. 0            ) THEN
!
                       CALL CLRCH  ( CNF_BUF(J12)(1:IPE) )
                       CALL CHASHL ( CNF_BUF(J12)        )
                       BUF(J11) = BUF(J11)(1:IPA-1)// &
     &                            CNF_BUF(J12)(1:I_LEN(CNF_BUF(J12)))// &
     &                            BUF(J11)(IPA+1:)
                       FL_FREQ_RES = .TRUE.
                  END IF
 4120          CONTINUE 
         END IF
 4110  CONTINUE 
!
! --- Check whether all parameters of the make file were replaced correctly
!
      IUER = -1
      DO 4140 J14=1,L_HW
         IF ( .NOT. FL_HW(J14) ) THEN
              CALL SOTID_ERR_LOG ( 5816, IUER, 'SOTID_CONFIGURE', 'Error '// &
     &            'in configuration file: parameter '// &
     &             CIS_HW(J14)(1:I_LEN(CIS_HW(J14)))// &
     &            ' was not found. Check syntax, pleeeeease' )
              CALL EXIT ( 2 )
         END IF
 4140 CONTINUE 
!
      IF ( .NOT. FL_FREQ_RES ) THEN
           CALL SOTID_ERR_LOG ( 5817, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter FREQ_RES was not found.'// &
     &         ' Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
!
! --- ... and then write down updated makefile
!
      IUER = -1
      CALL SOTID_WR_TEXT ( NBUF, BUF, 'hw95_out.i', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read configuration file once more
!
      IUER = -1
      CALL SOTID_RD_TEXT ( CNF_FILE, MBUF, CNF_BUF, CNF_NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Read template file for sotid_type.i
!
      IUER = -1
      CALL SOTID_RD_TEXT ( 'sotid_type.hi', MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Parse template file and configuration file
!
      FL_MAX_STA = .FALSE.
      FL_NAM_LEN = .FALSE.
      DO 4150 J15=1,NBUF
         IPA = INDEX ( BUF(J15), '@' ) 
         IF ( IPA .GT. 1 ) THEN
              DO 4160 J16=1,CNF_NBUF
                 IF ( CNF_BUF(J16)(1:1) .EQ. '#' ) GOTO 4160
!
! -------------- Search for SOTID__MAX_STA stuff
!
                 IPE = INDEX ( CNF_BUF(J16), '=' )
                 IPI  = INDEX ( BUF(J15),     'SOTID__MAX_STA' ) 
                 IPD  = INDEX ( BUF(J15),     '#define' ) 
                 IPC  = INDEX ( CNF_BUF(J16), 'SOTID__MAX_STA' ) 
                 IF ( IPI .GT. 0  .AND.  IPE .GT. 0  .AND. &
     &                IPD .GT. 0  .AND.  IPC .GT. 0  ) THEN
!
                      CALL CLRCH  ( CNF_BUF(J16)(1:IPE) )
                      CALL CHASHL ( CNF_BUF(J16)        )
                      BUF(J15) = BUF(J15)(1:IPA-1)// &
     &                          CNF_BUF(J16)(1:I_LEN(CNF_BUF(J16)))// &
     &                          BUF(J15)(IPA+1:)
                      FL_MAX_STA = .TRUE.
                 END IF
!
! -------------- Search for SOTID__NAM_LEN stuff
!
                 IPI  = INDEX ( BUF(J15),     'SOTID__NAM_LEN' ) 
                 IPC  = INDEX ( CNF_BUF(J16), 'SOTID__NAM_LEN' ) 
                 IF ( IPI .GT. 0  .AND.  IPE .GT. 0  .AND. &
     &                IPD .GT. 0  .AND.  IPC .GT. 0  ) THEN
!
                      CALL CLRCH  ( CNF_BUF(J16)(1:IPE) )
                      CALL CHASHL ( CNF_BUF(J16)        )
                      BUF(J15) = BUF(J15)(1:IPA-1)// &
     &                          CNF_BUF(J16)(1:I_LEN(CNF_BUF(J16)))// &
     &                          BUF(J15)(IPA+1:)
                      FL_NAM_LEN = .TRUE.
                 END IF
 4160          CONTINUE 
         END IF
 4150 CONTINUE 
!
! --- Check whether all parameters were found
!
      IUER = -1
      IF ( .NOT. FL_MAX_STA ) THEN
           CALL SOTID_ERR_LOG ( 5818, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter SOTID__MAX_STA was not found.'// &
     &         ' Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
      IF ( .NOT. FL_NAM_LEN ) THEN
           CALL SOTID_ERR_LOG ( 5819, IUER, 'SOTID_CONFIGURE', 'Error in '// &
     &         'configuration file: parameter SOTID__NAM_LEN was not found.'// &
     &         ' Check syntax, pleeeeease' )
           CALL EXIT ( 2 )
      END IF
!
! --- Write down updated file
!
      IUER = -1
      CALL SOTID_WR_TEXT ( NBUF, BUF, 'sotid_type.h', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      END  PROGRAM  SOTID_CONFIGURE
