      SUBROUTINE UPDATE_USERP()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  UPDATE_USERP PROGRAM SPECIFICATION
!
! 1.1 Update the file containing user-defined global parameters
!
! 1.2 REFERENCES:
!
! 2.  UPDATE_USERP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: adder
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 kexist
      INTEGER*2 I
      INTEGER*4  IOS
      integer*2 iglob,num_glob,num_user_part
      character*(NAME_SIZE) fn1,fn2
      CHARACTER*22 LGLOB(M_GPA), CDUM
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH   921202 Created
!   JMG   951227 Extensively rewritten.
!   pet   1999.10.20  Corrected a bug in the line just after reading the
!                     first record: ierr was in the previous version instead
!                     of ios
!
! 5.  UPDATE_USERP PROGRAM STRUCTURE
!
! Update user-defined global parameter list file
!
!
! ----- Open up USRG file, and read it in.
!
        FN1 = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
        CALL BIN_EXIST ( FN1, KEXIST )
        NUM_GLOB=0
        IF ( KEXIST ) THEN
             OPEN ( 66, FILE=FN1, IOSTAT=IOS )
             CALL FERR ( INT2(IOS), "Update_userp: Opening "//fn1, INT2(0), &
     &            INT2(0) )
             READ ( 66, *, IOSTAT=IOS ) NUM_GLOB
             IF ( NUM_GLOB .LT. 0 ) NUM_GLOB = 0
             IF ( IOS .NE. 0 ) NUM_GLOB = 0
             DO IGLOB=1,NUM_GLOB
                READ(66,'(A22)') LGLOB(IGLOB)
             ENDDO
             CLOSE ( 66, IOSTAT=IOS )
             CALL FERR( INT2(IOS), "Update_userp: Closing "//fn1, INT2(0), &
     &            INT2(0) )
        ENDIF
!
! ----- Open up USRP file and read it
!
        FN2 = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
        OPEN ( 67, FILE=FN2, IOSTAT=IOS )
        CALL FERR ( INT2(IOS), "Update_userp:  Opening "//fn2, INT2(0), &
     &       INT2(0) )
        READ ( 67, *, IOSTAT=IOS ) NUM_USER_PART
        IF ( NUM_USER_PART .LT. 0 ) IOS = 1
        CALL FERR ( INT2(IOS), "Update_userp: Reading "//fn2, INT2(0), INT2(0) )
!
! ----- Now read in the arc-supplied partials, one at a time.
!
        DO I=1,NUM_USER_PART
           READ ( 67, '(A22)' ) CDUM
!
! -------- If global parameter, see if the i-th arc-supplied partial is in
! -------- the list of global partials (contributed by previous arcs)
!
           if ( CDUM(22:22) .EQ. 'G') then
                DO IGLOB=1,NUM_GLOB
                   IF ( CDUM .EQ. LGLOB(IGLOB) ) GOTO 110
                END DO
!
! ------------- No match.  Add global parameter to end of list.
!
                NUM_GLOB=NUM_GLOB+1
                IF ( NUM_GLOB .GT. M_GPA ) THEN
                     CALL FERR ( INT2(491), "Update_userp: Too many "// &
     &                   "user_partials!", INT2(0), INT2(0) )
                     RETURN
                ENDIF
                LGLOB(NUM_GLOB)=CDUM
110             CONTINUE
           ENDIF
        ENDDO
        CLOSE ( 67, IOSTAT=IOS )
        CALL FERR( INT2(ios), "Update_userp: Closing "//FN2, INT2(0), INT2(0) )
!
! ----- Write out new usrg file.
!       ~~~~~~~~~~~~~~~~~~~~~~~~
!
        OPEN(66,FILE=FN1,iostat=IOS)
        CALL FERR( INT2(ios), "Update_userp: Opening "//FN1, INT2(0), INT2(0) )
        WRITE(66,'(I5)',IOSTAT=IOS) NUM_GLOB
        CALL FERR( INT2(ios), "Update_userp: Writing "//FN1, INT2(0), INT2(0) )
        DO I=1,NUM_GLOB
           WRITE(66,'(A)',IOSTAT=IOS) LGLOB(I)
           CALL FERR( INT2(IOS), "Update_userp: Writing "//FN1, INT2(0), &
     &          INT2(0) )
        ENDDO
        CLOSE ( 66, IOSTAT=IOS )
        CALL FERR( INT2(ios), "Update_userp: Closing "//FN1, INT2(0), INT2(0) )
!
      RETURN
      END  !#!  UPDATE_USERP  #!#
