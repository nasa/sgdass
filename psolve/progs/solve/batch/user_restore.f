      SUBROUTINE USER_RESTORE ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine USER_RESTORE is for restoring the list of user global      *
! *   parameters for continuation of global run. It extracts parameters  *
! *   list from CGM, compares it with parameter list which it generates  *
! *   on the basis of parameters setup saved in CGM ( which knows        *
! *   nothing about user parameters ) and the difference is interpreted  *
! *   as global parameters. They are written in USRGxx file.             *
! *                                                                      *
! *   2006.01.17  pet  Fixed bug: KGLOBAL should be .TRUE., not .FALSE.  *
! *                                                                      *
! *  ### 18-MAR-2002  USER_RESTORE  v1.3 (c) L. Petrov  30-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbp.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'plist.i'
      INCLUDE   'precm.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INTEGER*4  IUER
      CHARACTER  LPARM_GET(M_GPA)*(L__GPA), LPARM_USG(M_GPA)*(L__GPA)
      CHARACTER  STR*80, FNAME*128
      LOGICAL*2  KSHORT, KGLOBAL
      LOGICAL*4  LEX
      INTEGER*4  LUN, IOS, J1, J2, J3, IP
      INTEGER*4  L_USG, NPARM_GET, IX2T3(M_GPA)
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, ILEN, LTM_DIF
!
! --- Get a list of parameters in the form which NORML expects to get it
! --- We instruct GET_NAMES not to read USRGxx file
!
      CALL SOCOM_EXT()
!
      KSHORT  = .TRUE.
      KGLOBAL = .TRUE.
      KGLOBONLY = .TRUE.
      KUSER_PART = .FALSE.  ! Temporarily betray user parameters
      CALL GET_NAMES ( LPARM_GET, INT2(20), M_GPA, NPARM_GET, KSHORT, &
     &                 KGLOBAL )
      KUSER_PART = .TRUE.   ! But then again fall in love with them
!
      L_USG = 0
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
!
! --- Now check: does USRG exist?
!
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- Yeah! It exists. Let's read it.
!
           LUN =GET_UNIT ()
           OPEN ( UNIT=LUN, FILE=FNAME, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 5641, IUER, 'USER_RESTORE', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open file '//FNAME )
                RETURN
           END IF
           DO 410 J1=1,M_GPA*2
              READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) THEN
                   GOTO 810
                ELSE IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IOS, STR )
                   CALL ERR_LOG ( 5642, IUER, 'USER_RESTORE', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in reading file '//FNAME )
                   RETURN
              END IF
!
              IF ( J1 .GT. 1 ) THEN
!
! ---------------- We read it and add it to the list LPARM_USG
!
                   L_USG = L_USG + 1
                   LPARM_USG(L_USG) = STR(1:20)
              END IF
 410       CONTINUE
 810       CONTINUE
           CLOSE ( UNIT=LUN )
      END IF
!
! --- Now create the cross reference table between the array of parameters
! --- saved in CGM ( PARMS_NAMES, PARM_NUM ) and parameter list which was built
! --- according to parameters setup.
!
      CALL CXEPAR_OPT20 ( CPARM_NAMES, IX2T3, PARM_NUM, LPARM_GET, NPARM_GET )
      DO 420 J2=1,PARM_NUM
         IF ( IX2T3(J2) .EQ. 0 ) THEN
!
! ----------- This parameter was in the CGM, but not in the list built
! ----------- according to parameters setup. Therefore, it is a user parameter
!
! ----------- But stop. Let us check, whether this parameter is already in 
! ----------- the list in order to avoid double counting
!
              IF ( L_USG > 0 ) THEN
                   IP = LTM_DIF ( 1, L_USG, LPARM_USG, CPARM_NAMES(J2) )
                 ELSE 
                   IP = 0
              END IF
              IF ( IP == 0 ) THEN
!
! ---------------- Yes, this parameter was NOT in the list. Add it.
!
                   L_USG = L_USG + 1
                   LPARM_USG(L_USG) = CPARM_NAMES(J2)
              END IF
         END IF
 420  CONTINUE
!
! --- Now time came to write down the list of user global parameter into a file
!
      LUN =GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FNAME, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5643, IUER, 'USER_RESTORE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open file '//FNAME )
           RETURN
      END IF
!
      WRITE ( LUN, * ) L_USG
      DO 430 J3=1,L_USG
         WRITE ( LUN, '(A)' ) LPARM_USG(J3)//' G'
 430  CONTINUE
      CLOSE ( UNIT=LUN )
!
      RETURN
      END  !#!  USER_RESTORE  #!#
