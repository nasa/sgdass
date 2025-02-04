      SUBROUTINE DO_STAVELC ( CPARNAM, NPARMS, SPOOL_L1, LPARMS, &
     &                        XYZ_CNST, XYZ_CNSB, FL_XYZ, &
     &                        UEN_CNST, UEN_CNSB, FL_UEN, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_STAVELC PROGRAM SPECIFICATION
!
! 1.1 Apply constraints on either velocity or station positions.
!     Two kind of constrains are applied: constrians on XYZ components of the
!     positions of velocities and on UEN components.
!
! 1.2 REFERENCES:
!
! 2.  DO_STAVELC INTERFACE
!
! 2.1 Parameter File
      INCLUDE    'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4  IUER
      INTEGER*4  NPARMS
      LOGICAL*1  SPOOL_L1
      CHARACTER  CPARNAM*9, LPARMS(NPARMS)*20, PAR_NAME*20
      INTEGER*2  XYZ_CNSB(STA_BIT_WORDS), UEN_CNSB(STA_BIT_WORDS)
      REAL*8     XYZ_CNST(3), UEN_CNST(3)
      LOGICAL*2  FL_XYZ, FL_UEN
!
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'prfil.i'
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   IP, IPS, IPA(3), ICNS_STA(3), ICNS_VEL(3), L_STA, IER
      INTEGER*2   NN, J1, J2, J3, J4, J5, J6, J7, J8, I_STA, LOC_DEBUG
      CHARACTER   COMP_XYZ(3)*2, COMP_UEN(3)*2
      DATA      ( COMP_XYZ(NN), NN=1,3 ) / 'X ', 'Y ', 'Z ' /
      DATA      ( COMP_UEN(NN), NN=1,3 ) / 'U ', 'E ', 'N ' /
      CHARACTER   CNS_NAM*16, C_STA(MAX_STA)*8, OUT*4096, STR*20, STR1*20
!
      REAL*8      UEN__XYZ(3,3), XYZ__UEN(3,3)
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      LOGICAL*4   FVEL, TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      LOGICAL*2   FL_J1_XYZ, FL_J1_UEN, KBIT
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4   LTM_DIF, I_LEN, ADD_CLIST
!
! 4.  HISTORY
!   MWH  920512  Created, based on do_atm
!   pet  980205  Declared the sigma of the constrain in solve.i instead of
!                hard-coded value. Write down information about the type
!                of applied constraint.
!   pet  2000.11.24  Totally re-wrote.
!   pet  2002.09.19  Again completely re-wrote. Changed internal logic: the
!                    new version puts equations of constraitns in CNSTROBJ,
!                    while the old version modified normal matrix directly.
!   pet  2003.07.07  Fixed a bug: the prbous version tryed to set 0-th bit
!                    of FL_J1_XYZ when a station was not found in STASUP_CHR
!                    list.
!   pet  2021.10.22  Added printing into the spool file the list of stations
!                    participated in the constraint
!
! 5.  DO_STAVELC PROGRAM STRUCTUREw
!
!   Set up the constraint:  it is hard-wired here . . .
!
!
      LOC_DEBUG = 0
      FVEL = .FALSE.
      IF ( CPARNAM .EQ. 'VELOCITY ' ) FVEL = .TRUE.
!
! --- Constraints on XYZ coordinates
!
      ICNS_VEL(1) = 0
      ICNS_VEL(2) = 0
      ICNS_VEL(3) = 0
!
      ICNS_STA(1) = 0
      ICNS_STA(2) = 0
      ICNS_STA(3) = 0
!
      L_STA = 0
      DO 410 J1=1,NUMSTA
         IPS = 0
         FL_J1_XYZ = .FALSE.
         FL_J1_UEN = .FALSE.
         IF ( ISTASP .GT. 0 ) THEN
!
! ----------- Search the J1-th station in the exception list
!
              IPS = LTM_DIF ( 0, INT4(ISTASP), STASUP, ISITN_CHR(J1) )
              IF ( IPS .GT. 0 ) THEN
!
! ---------------- Set flags: whether the J1-th station was found
! ---------------- in the exception list XYZ and/or UEN
!
                   FL_J1_XYZ = KBIT ( XYZ_CNSB, INT2(IPS) )
                   FL_J1_UEN = KBIT ( UEN_CNSB, INT2(IPS) )
              END IF
         END IF
         IF ( LOC_DEBUG > 0 ) THEN
              WRITE ( 6, 210 ) J1, ISITN_CHR(J1), KBIT( IUEN, INT2(1) ), FL_J1_XYZ, FL_J1_UEN, FVEL
 210          FORMAT ( 'DO_STAVELC: ',I3, 2X, A, ' Constr_iuen: ', L1, 1X, L1, 1X, L1, ' Vel_cnstr: ', L1 )
         END IF
         IF ( KBIT( IUEN, INT2(1) ) ) THEN
!
! =========== UEN parameterization of station positions or velocities
! =========== estimation
!
              IF ( (       FL_XYZ .AND. .NOT. FL_J1_XYZ ) .OR. &
     &             ( .NOT. FL_XYZ .AND.       FL_J1_XYZ )      ) THEN
!
! ---------------- XYZ- constraints were imposed for the J1-th station.
! ---------------- Find array IPA of the index of the parameter in the list.
!
                   IPA(1) = LTM_DIF ( 0, NPARMS, LPARMS, ISITN_CHR(J1)// &
     &                                ' '//COMP_UEN(1)//CPARNAM )
                   IPA(2) = LTM_DIF ( 0, NPARMS, LPARMS, ISITN_CHR(J1)// &
     &                                ' '//COMP_UEN(2)//CPARNAM )
                   IPA(3) = LTM_DIF ( 0, NPARMS, LPARMS, ISITN_CHR(J1)// &
     &                                ' '//COMP_UEN(3)//CPARNAM )
!
! ---------------- Find matrix of the transfromation from the crust fixed
! ---------------- reference frame XYZ to the local topocentric system UEN
!
                   CALL UEN_ROT ( VSITEC(1,J1), XYZ__UEN )
                   CALL TM83    ( XYZ__UEN,     UEN__XYZ )
!
                   DO 420 J2=1,3
                      IF ( XYZ_CNST(J2) .GT. 1.D-30 .AND. IPA(J2) .GT. 0 ) THEN
                           IER = -1
                           I_STA = ADD_CLIST ( MAX4_SIT, L_STA, C_STA, ISITN_CHR(J1), IER )
!
! ------------------------ Put information about constrint in CNSTROBJ
!
                           CALL ERR_PASS ( IUER, IER )
                           IF ( FVEL ) THEN
                                ICNS_VEL(J2) = ICNS_VEL(J2) + 1
                                CNS_NAM = 'VEL_'//COMP_XYZ(J2)
                                CALL ADDCNS_NAM ( CNS_NAM, ICNS_VEL(J2), &
     &                               'Velocity components', 'meter/year', 0.0D0, &
     &                               UEN_CNST(J2), TRUE_L4, CNSTROBJ, IER )
                             ELSE IF ( .NOT. FVEL ) THEN
                                ICNS_STA(J2) = ICNS_STA(J2) + 1
                                CNS_NAM = 'STA_'//COMP_XYZ(J2)
                                CALL ADDCNS_NAM ( CNS_NAM, ICNS_STA(J2), &
     &                               'Station position components', 'meter', &
     &                               0.0D0, UEN_CNST(J2), TRUE_L4, CNSTROBJ, &
     &                               IER )
                           END IF
!
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8621, IUER, 'DO_STAVELC', &
     &                              'Error in an attempt to put information '// &
     &                              'about '//CNS_NAM(1:I_LEN(CNS_NAM))// &
     &                              ' constraint' )
                                RETURN
                           END IF
!
! ------------------------ Put coeffecient of constraint equation in
! ------------------------ CNSTROBJ
!
                           DO 430 J3=1,3
                              CALL ERR_PASS ( IUER, IER )
                              IF ( FVEL ) THEN
                                   CALL ADDCNS_EQU ( CNS_NAM, ICNS_VEL(J2), &
     &                                  IPA(J3), UEN__XYZ(J2,J3), TRUE_L4, &
     &                                  CNSTROBJ, IER )
                                ELSE IF ( .NOT. FVEL ) THEN
                                   CALL ADDCNS_EQU ( CNS_NAM, ICNS_STA(J2), &
     &                                  IPA(J3), UEN__XYZ(J2,J3), TRUE_L4, &
     &                                  CNSTROBJ, IER )
                              END IF
!
                              IF ( IER .NE. 0 ) THEN
                                   CALL ERR_LOG ( 8622, IUER,'DO_STAVELC', &
     &                                 'Error in an attempt '// &
     &                                 'to put constraint '// &
     &                                  CNS_NAM(1:I_LEN(CNS_NAM))//' equation' )
                                   RETURN
                              END IF
 430                       CONTINUE
                      END IF
 420               CONTINUE
              END IF
!
! ----------- Constraints on UEN components of positions or velocities
!
              DO 440 J4=1,3 ! cycle over components
                 IF ( ( (       FL_UEN .AND. .NOT. FL_J1_UEN ) .OR. &
     &                  ( .NOT. FL_UEN .AND.       FL_J1_UEN )     ) .AND. &
     &                UEN_CNST(J4) .GT. 1.D-30                            ) THEN
!
                      PAR_NAME = ISITN_CHR(J1)//' '//COMP_UEN(J4)//CPARNAM
                      IP = LTM_DIF ( 0, NPARMS, LPARMS, PAR_NAME )
                      IF ( IP .LE. 0 ) GOTO 440
                      I_STA = ADD_CLIST ( MAX4_SIT, L_STA, C_STA, ISITN_CHR(J1), IER )
!
! ------------------- Put information about constraint in CNSTROBJ
!
                      CALL ERR_PASS ( IUER, IER )
                      IF ( FVEL ) THEN
                           ICNS_VEL(J4) = ICNS_VEL(J4) + 1
                           CNS_NAM = 'VEL_'//COMP_UEN(J4)
                           CALL ADDCNS_NAM ( CNS_NAM, ICNS_VEL(J4), &
     &                         'Velocity components', 'meter/year', 0.0D0, &
     &                          UEN_CNST(J4), TRUE_L4, CNSTROBJ, IER )
                         ELSE IF ( .NOT. FVEL ) THEN
                           ICNS_STA(J4) = ICNS_STA(J4) + 1
                           CNS_NAM = 'STA_'//COMP_UEN(J4)
                           CALL ADDCNS_NAM ( CNS_NAM, ICNS_STA(J4), &
     &                         'Station position components', 'meter', &
     &                          0.0D0, UEN_CNST(J4), TRUE_L4, CNSTROBJ, IER )
                      END IF
!
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8623, IUER, 'DO_STAVELC', &
     &                         'Error in an attempt to put information '// &
     &                         'about '//CNS_NAM(1:I_LEN(CNS_NAM))// &
     &                         ' constraint' )
                           RETURN
                      END IF
!
! ------------------- Put coefficients of the consrtaint equation in CNSTROBJ
!
                      CALL ERR_PASS ( IUER, IER )
                      IF ( FVEL ) THEN
                           CALL ADDCNS_EQU ( CNS_NAM, ICNS_VEL(J4), IP, 1.0D0, &
     &                          TRUE_L4, CNSTROBJ, IER )
                         ELSE IF ( .NOT. FVEL ) THEN
                           CALL ADDCNS_EQU ( CNS_NAM, ICNS_STA(J4), IP, 1.0D0, &
     &                          TRUE_L4, CNSTROBJ, IER )
                      END IF
!
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8624, IUER, 'DO_STAVELC', &
     &                         'Error in an attempt to put constraint '// &
     &                          CNS_NAM(1:I_LEN(CNS_NAM))//' equation' )
                           RETURN
                      END IF
                 END IF
 440          CONTINUE
            ELSE
!
! =========== XYZ parameterization of station positions or velocities
! =========== estimation
!
              DO 450 J5=1,3
!
! -------------- XYZ constraints
!
                 IF ( ( (       FL_XYZ .AND. .NOT. FL_J1_XYZ ) .OR. &
     &                  ( .NOT. FL_XYZ .AND.       FL_J1_XYZ )     ) .AND. &
     &                XYZ_CNST(J5) .GT. 1.D-30                            ) THEN
!
                      PAR_NAME = ISITN_CHR(J1)//' '//COMP_XYZ(J5)//CPARNAM
                      IP = LTM_DIF ( 0, NPARMS, LPARMS, PAR_NAME )
                      IF ( IP .LE. 0 ) GOTO 450
                      I_STA = ADD_CLIST ( MAX4_SIT, L_STA, C_STA, ISITN_CHR(J1), IER )
                      IF ( LOC_DEBUG > 0 ) THEN
                           WRITE  ( 6, 220 ) J1, ISITN_CHR(J1), J3, FVEL
 220                       FORMAT ( 'DO_STAVELC: ',I3, 2X, A, ' icmp: ', I1, ' vel_cns: ', L1 )
                      END IF
!
! ------------------- Put information about constrint in CNSTROBJ
!
                      CALL ERR_PASS ( IUER, IER )
                      IF ( FVEL ) THEN
                           ICNS_VEL(J5) = ICNS_VEL(J5) + 1
                           CNS_NAM = 'VEL_'//COMP_XYZ(J5)
                           CALL ADDCNS_NAM ( CNS_NAM, ICNS_VEL(J5), &
     &                         'Velocity components', 'meter/year', 0.0D0, &
     &                          XYZ_CNST(J5), TRUE_L4, CNSTROBJ, IER )
                         ELSE IF ( .NOT. FVEL ) THEN
                           ICNS_STA(J5) = ICNS_STA(J5) + 1
                           CNS_NAM = 'STA_'//COMP_XYZ(J5)
                           CALL ADDCNS_NAM ( CNS_NAM, ICNS_STA(J5), &
     &                         'Station position components', 'meter', &
     &                          0.0D0, XYZ_CNST(J5), TRUE_L4, CNSTROBJ, &
     &                          IER )
                      END IF
!
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8625, IUER, 'DO_STAVELC', &
     &                         'Error in an attempt to put information '// &
     &                         'about '//CNS_NAM(1:I_LEN(CNS_NAM))// &
     &                         ' constraint' )
                           RETURN
                      END IF
!
! ------------------- Put the coefficient of consrtaint equations in CNSTROBJ
!
                      CALL ERR_PASS ( IUER, IER )
                      IF ( FVEL ) THEN
                           CALL ADDCNS_EQU ( CNS_NAM, ICNS_VEL(J5), IP, 1.0D0, &
     &                          TRUE_L4, CNSTROBJ, IER )
                         ELSE IF ( .NOT. FVEL ) THEN
                           CALL ADDCNS_EQU ( CNS_NAM, ICNS_STA(J5), IP, 1.0D0, &
     &                          TRUE_L4, CNSTROBJ, IER )
                      END IF
!
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8626, IUER, 'DO_STAVELC', &
     &                         'Error in an attempt to put '// &
     &                          CNS_NAM(1:I_LEN(CNS_NAM))//' constraint equation' )
                           RETURN
                      END IF
                 END IF
 450          CONTINUE
!
              IF ( (       FL_UEN .AND. .NOT. FL_J1_UEN ) .OR. &
     &             ( .NOT. FL_UEN .AND.       FL_J1_UEN )      ) THEN
!
! ---------------- UEN constraints
!
                   IPA(1) = LTM_DIF ( 0, NPARMS, LPARMS,ISITN_CHR(J1)// &
     &                                ' '//COMP_XYZ(1)//CPARNAM )
                   IPA(2) = LTM_DIF ( 0, NPARMS, LPARMS,ISITN_CHR(J1)// &
     &                                ' '//COMP_XYZ(2)//CPARNAM )
                   IPA(3) = LTM_DIF ( 0, NPARMS, LPARMS,ISITN_CHR(J1)// &
     &                                ' '//COMP_XYZ(3)//CPARNAM )
!
! ---------------- Find matrix of the transfromation from the the local
! ---------------- topocentric system UEN to the crust fixed reference
! ---------------- frame XYZ
!
                   CALL UEN_ROT ( VSITEC(1,J1), XYZ__UEN )
                   DO 460 J6=1,3
                      IF ( ( (       FL_UEN .AND. .NOT. FL_J1_UEN ) .OR. &
     &                       ( .NOT. FL_UEN .AND.       FL_J1_UEN )     ) .AND. &
     &                         IPA(J6) .GT. 0                             .AND. &
     &                         UEN_CNST(J6) .GT. 1.D-30                 ) THEN
                           IER = -1
                           I_STA = ADD_CLIST ( MAX4_SIT, L_STA, C_STA, ISITN_CHR(J1), IER )
!
! ------------------------ Put information about constrint in CNSTROBJ
!
                           CALL ERR_PASS ( IUER, IER )
                           IF ( FVEL ) THEN
                                ICNS_VEL(J6) = ICNS_VEL(J6) + 1
                                CNS_NAM = 'VEL_'//COMP_UEN(J6)
                                CALL ADDCNS_NAM ( CNS_NAM, ICNS_VEL(J6), &
     &                               'Velocity components', 'meter/year', 0.0D0, &
     &                               UEN_CNST(J6), TRUE_L4, CNSTROBJ, IER )
                             ELSE IF ( .NOT. FVEL ) THEN
                                ICNS_STA(J6) = ICNS_STA(J6) + 1
                                CNS_NAM = 'STA_'//COMP_UEN(J6)
                                CALL ADDCNS_NAM ( CNS_NAM, ICNS_STA(J6), &
     &                               'Station position components', 'meter', &
     &                               0.0D0, UEN_CNST(J6), TRUE_L4, CNSTROBJ, &
     &                               IER )
                           END IF
!
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8627, IUER, 'DO_STAVELC', &
     &                              'Error in an attempt to put information '// &
     &                              'about '//CNS_NAM(1:I_LEN(CNS_NAM))// &
     &                              ' constraint' )
                                RETURN
                           END IF
!
! ------------------------ Put coefficients of constraint equation in
! ------------------------ CNSTROBJ
!
                           DO 470 J7=1,3
                              CALL ERR_PASS ( IUER, IER )
                              IF ( FVEL ) THEN
                                   CALL ADDCNS_EQU ( CNS_NAM, ICNS_VEL(J6), &
     &                                  IPA(J7), XYZ__UEN(J6,J7), TRUE_L4, &
     &                                  CNSTROBJ, IER )
                                ELSE IF ( .NOT. FVEL ) THEN
                                   CALL ADDCNS_EQU ( CNS_NAM, ICNS_STA(J6), &
     &                                  IPA(J7), XYZ__UEN(J6,J7), TRUE_L4, &
     &                                  CNSTROBJ, IER )
                              END IF
!
                              IF ( IER .NE. 0 ) THEN
                                   WRITE ( 6, * ) ' J1=',J1,' J7=',J7, &
     &                                            ' IPA(J7) = ',IPA(J7)
                                   CALL ERR_LOG ( 8628, IUER,'DO_STAVELC', &
     &                                 'Error in an attempt '// &
     &                                 'to put constraint '// &
     &                                  CNS_NAM(1:I_LEN(CNS_NAM))//' equation' )
                                   RETURN
                              END IF
 470                       CONTINUE
                      END IF
 460               CONTINUE
              END IF
         END IF
 410  CONTINUE
      IF ( SPOOL_L1 ) THEN
!
! -------- Writing the list of stations in spool-file
!
           IF ( CPARNAM == 'COMPONENT' ) THEN
                WRITE ( 23, '(A,I4,A)' ) 'STA_POS: ', L_STA, ' stations '// &
     &                                   'participated in constraints to their positions: '
              ELSE IF ( CPARNAM == 'VELOCITY' ) THEN
                WRITE ( 23, '(A,I4,A)' ) 'STA_VEL: ', L_STA, ' stations '// &
     &                                   'participated in constraints to their velocities: '
           END IF
           IF ( L_STA .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL SORT_CH ( L_STA, C_STA )
!
! -------------- Cleaning the output line with the list of stations
!
                 CALL CLRCH ( OUT )
                 DO 480 J8=1,L_STA
!
! ----------------- Add the J8-th station to the list of stations for further
! ----------------- printing
!
                    IP  = (J8-1)*9 + 1
                    OUT = OUT(1:IP)//C_STA(J8)
 480             CONTINUE 
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sublines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
                 WRITE ( 23, '(A)' ) ' '
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_STAVELC  #!#
