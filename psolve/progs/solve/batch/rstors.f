      SUBROUTINE RSTORS ( IPASS, ARCNUM, SOLTY2, RESTRT, ETIME0, ETIMP0, &
     &                    SCNOUT, KCORL, IEOPLL, LENCNT, LENARC, &
     &                    CFNAME_ORIGINAL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RSTORS PROGRAM SPECIFICATION
!
! 1.1 Determine whether we are to recover, and if so, recover the
!     state of the program.
!
! 1.2 REFERENCES:
!
! 2.  RSTORS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER SCNOUT*1, CFNAME_ORIGINAL*(*)
      LOGICAL*2 KCORL
      INTEGER*2 IEOPLL, LENCNT, LENARC
!
! KCORL - TRUE if covariances are to be output
! SCNOUT - TRUE if output is to go to screen
! IEOPLL - Earth orientation plot flags
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*1 SOLTY2
      INTEGER*2 ARCNUM,IPASS
      INTEGER*4 ETIME0,ETIMP0
      LOGICAL*2 RESTRT
!
! ARCNUM - Number of arc to start with
! IPASS - Pass number; 1 = forward, 2 = back
! RESTRT - TRUE if we are to recover
! SOLTY2 - Solution type of current pass (forward or back)
! ETIME0 - Beginning processing time of current arc
! ETIMP0 - Beginning processing time of current pass
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: cfspos,sarst,stimer,saves
!
! 3.  LOCAL VARIABLES
!
      CHARACTER ANS*1, BUFF*100
      INTEGER*2 IBUFF(50)
!
      CHARACTER  STR1*8, STR2*8, DBNAME_MES*16, STR*160, WORK_DIR*160, &
     &           CNTR_FILE*160, FINAM_OLD*128, FINAM_NEW*128
      INTEGER*4  MAT_E, JRND_BLOCKS, JBLOCKS, IB
      EQUIVALENCE ( IBUFF, BUFF )
      LOGICAL*4  LEX
      INTEGER*4  MIND
      INTEGER*8  LEN8_BYTES, LEN8_BLOCKS
      PARAMETER  ( MIND=32 )
      INTEGER*4  LIND, IND(2,MIND), IP, I40
      LOGICAL*2  KBIT
      INTEGER*4, EXTERNAL :: LINDEX, ILEN, I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   MWH  900206  Fix bug when restarting at very beginning of back solution
!
!   PET  980204  Substitute call prgogram of COPYQ by call of subroutine
!                copy_file
!
!   PET  990112  Added support of TRAIN variable, updated comments. Added
!                printing the session number and name when SOLVE starts
!                processing in recovery mode
!
!   PET  990419  Added check of consistency of the current and saved length
!                of the control file and arcfile before recovering
!
!   PET 1999.05.10  Added an additional check before trying to restore solution:
!                   the program checks: whether the name of the control file
!                   which is now running and the name of the control file which
!                   has been saved after saving solution is the same.
!                   If not, rstors will not make attempts to recover.
!
!   PET 2000.03.30  Changed the logic for comparison the name of the current
!                   control file and the saved control file when we check
!                   whether restart is possible: only the main part of the path
!                   is compared, but the preceeding path is truncated.
!                   Thus, if the control run was launched from the current
!                   directory without specification of the path of comntrol
!                   file or the run has been launched from a remote directory
!                   with spesification of full path -- these file names are
!                   considered as identical.
!
!   pet 2000.09.28  Fix bug when restarting at very beginning of back solution:
!                   the previous version didn't allow to make the second
!                   attempt.
!
!   pet 2007.08.10  Added support of PRE_IP(2) bit 7 which eliminates &
!                   dialogue with a user. If rstart is possible, Sovle will &
!                   restart, it it is not possivle it will start anew.
!
!   pet 2007.08.31  Got rid of archaic GONOGO variable
!
! 5.  RSTORS PROGRAM STRUCTURE
!
! --- Read in glbfil
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 ( 'RC' )
!
! --- If .NOT. kmored then don't recover
!
      RESTRT = KMORED
      IF ( .NOT. RESTRT ) THEN
           IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                WRITE ( 6, * ) 'Restart flag in the scracth area was not set'
                WRITE ( 6, * ) 'Solve starts solution from the very beginning'
           END IF
      END IF
!
! --- Set the file name where status will be written
!
      CALL CLRCH ( WORK_DIR )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) .EQ. 0 ) THEN
           WORK_DIR = SOLVE_WORK_DIR
      END IF
!
! --- Form the name of the file where control file is saved
!
      IP = I_LEN(WORK_DIR)
      IF ( WORK_DIR(IP:IP) .NE. '/' ) WORK_DIR(IP+1:IP+1) = '/'
      CNTR_FILE = WORK_DIR(1:I_LEN(WORK_DIR))//'CNTR'//PRE_LETRS
!
      LIND = 0
      INQUIRE ( FILE=CNTR_FILE, EXIST=LEX )
      IF ( LEX ) THEN
!
! ------ Yes, the file with saved control file exists
!
         OPEN ( UNIT=40, FILE=CNTR_FILE, STATUS='OLD', IOSTAT=I40 )
         IF ( I40 .NE. 0 ) THEN
              GOTO 810
         END IF
!
! ------ Read a line from there
!
         READ ( UNIT=40, IOSTAT=I40, FMT='(A)' ) STR
         IF ( I40 .NE. 0 ) THEN
              CLOSE ( UNIT=40 )
              GOTO 810
         END IF
         CLOSE ( UNIT=40 )
!
! ------ Parse the line onto words
!
         CALL EXWORD ( STR, MIND, LIND, IND, ' ', -3 )
!
! ------ Add the third word: # in order to mark that SOLVE is running but the
! ------ the file with name of the control file is not yet updated
!
         IF ( LIND .GE. 3 ) THEN
              STR(IND(2,2)+2:)='#'
            ELSE
              STR(I_LEN(STR)+2:)='#'
         END IF
!
! ------ Open CNTR_FILE in order to re-write a line there
!
         OPEN ( UNIT=40, FILE=CNTR_FILE, STATUS='UNKNOWN', IOSTAT=I40 )
         IF ( I40 .NE. 0 ) THEN
              GOTO 810
         END IF
!
! ------ Write an update line there
!
         WRITE ( UNIT=40, IOSTAT=I40, FMT='(A)' ) STR(1:I_LEN(STR))
         IF ( I40 .NE. 0 ) THEN
              CLOSE ( UNIT=40 )
              GOTO 810
         END IF
         CLOSE ( UNIT=40 )
      END IF
!
      IF ( LIND .GE. 2  .AND.  RESTRT ) THEN
!
! -------- Now we check: if the control file from saved solution and the current
! -------- control file name are different then restoration is not possible
!
           CALL CLRCH ( FINAM_OLD )
           CALL CLRCH ( FINAM_NEW )
!
! -------- But we compare only the main part of the name without path.
! -------- Thus, first extract the main part of the name from the fill name
!
           IB = LINDEX ( CFNAME_ORIGINAL, '/' ) + 1
           FINAM_NEW = CFNAME_ORIGINAL(IB:)
!
           IB = LINDEX ( STR(IND(1,2):IND(2,2)), '/' ) + 1
           IF ( IB .GT. ILEN(STR(IND(1,2):IND(2,2))) ) IB = 1
           FINAM_OLD = STR(IND(1,2)+IB-1:IND(2,2))  
!
! -------- Now compare pathless filenames
!
           IF ( FINAM_OLD .NE. FINAM_NEW ) THEN
                RESTRT = .FALSE.
                IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                     WRITE ( 6, * ) 'The scracth area kept the previous control file name'
                     WRITE ( 6, * ) 'different from the current: '
                     WRITE ( 6, * ) 'Old control file: '//FINAM_OLD(1:I_LEN(FINAM_OLD))
                     WRITE ( 6, * ) 'New control file: '//FINAM_NEW(1:I_LEN(FINAM_NEW))
                     WRITE ( 6, * ) 'Restart is not possible'
                     WRITE ( 6, * ) 'Solve starts solution from the very beginning'
                END IF
           END IF
      END IF
 810  CONTINUE
!
! --- Restart negotiaion
!
      IF ( RESTRT  .AND.  LENCNT .NE. LENCNT_SAVED ) THEN
           WRITE  ( 6, 110 ) LENCNT, LENCNT_SAVED
 110       FORMAT ( 1X,'Alas, recovering is not possible since length of ', &
     &                 'the parsed part '/ &
     &              1X,'of the control file was changed:'/ &
     &              1X,'It is  ',I4,' lines now'/ &
     &              1X,'It was ',I4,' lines when solution was saved' )
 !
           IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                WRITE ( 6, '(A)' ) ' Solve is forced to start solution '// &
     &                             'from the very beginning'
                RESTRT = .FALSE.
              ELSE 
 710            CONTINUE
                WRITE  ( 6, 120 )
 120            FORMAT ( 1X,'Will we start to make our solution anew  ', &
     &                      '(Yes/No) ? '$ )
                READ ( 5, '(A1)' ) ANS
                CALL CASEFOLD ( ANS )
                IF ( ANS .EQ. 'N' ) THEN
                     WRITE ( *, * ) 'Stopping...'
                     CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
                   ELSE IF ( ANS .EQ. 'Y' ) THEN
                     RESTRT = .FALSE.
                     WRITE ( 6, * ) 'Start solution from the very beginning'
                   ELSE
                     GOTO 710
                END IF
           END IF
      END IF
      IF ( RESTRT  .AND.  LENARC .NE. LENARC_SAVED ) THEN
           WRITE  ( 6, 130 ) LENARC, LENARC_SAVED
 130       FORMAT ( 1X,'Alas, recovering is not possible since length of ', &
     &                 'the arc file was changed:'/ &
     &              1X,'It is  ',I4,' lines now'/ &
     &              1X,'It was ',I4,' lines when solution was saved' )
           IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                WRITE ( 6, '(A)' ) ' Solve is forced to start solution '// &
     &                             'from the very beginning'
                RESTRT = .FALSE.
              ELSE 
 720            CONTINUE
                WRITE  ( 6, 120 )
                READ ( 5, '(A1)' ) ANS
                CALL CASEFOLD ( ANS )
                IF ( ANS .EQ. 'N' ) THEN
                     WRITE ( *, * ) 'Stopping...'
                     CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
                  ELSE IF ( ANS .EQ. 'Y' ) THEN
                     RESTRT = .FALSE.
                     WRITE ( 6, * ) 'Start solution from the very beginning'
                  ELSE
                     GOTO 720
                END IF
           END IF
      END IF
!
      IF ( RESTRT                  .AND. &
     &     ISLTY2 .EQ. 'F'         .AND. &
     &     TRAIN  .NEQV. TRAIN_CGM       ) THEN
!
! -------- It is prohibited to change TRAIN mode in forward run (since CGM
! -------- is ordered in TRAIN mode, but not ordered in NO TRAIN mode)
!
           IF ( TRAIN  .AND.  .NOT. TRAIN_CGM ) THEN
                STR1 = 'TRAIN   '
                STR2 = 'NO TRAIN'
              ELSE IF ( .NOT. TRAIN  .AND.  TRAIN_CGM ) THEN
                STR1 = 'NO TRAIN'
                STR2 = 'TRAIN'
           END IF
!
           WRITE  ( 6, 140 ) STR1, STR2
 140       FORMAT ( 1X,'Alas, recovering is not possible since we are in ', &
     &                  A, ' mode '/ &
     &              1X,'but saved CGM was written in ',A,' mode'/ )
!
           IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                WRITE ( 6, '(A)' ) ' Solve is forced to start solution '// &
     &                             'from the very beginning'
                RESTRT = .FALSE.
              ELSE 
 730            CONTINUE
                WRITE  ( 6, 120 )
                READ ( 5, '(A1)' ) ANS
                CALL CASEFOLD ( ANS )
                IF ( ANS .EQ. 'N' ) THEN
                     WRITE ( *, * ) 'Stopping...'
                     CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
                  ELSE IF ( ANS .EQ. 'Y' ) THEN
                     RESTRT = .FALSE.
                     WRITE ( 6, * ) 'Start solution from the very beginning'
                  ELSE
                     GOTO 730
                END IF
           END IF
         ELSE
!
! -------- If a recover is possible, but user said go, then start from scratch
!
           IF ( RESTRT ) THEN
                IF ( KBIT ( PRE_IP(2), INT2(7) ) ) THEN
                     WRITE ( 6, '(A)' ) ' Solve is restarting'
                     CONTINUE
                   ELSE 
!
! ------------------ Ask a user, whether he wants to restart
!
  740                CONTINUE
                     WRITE ( 6, '(A,$)' ) 'Do you want to recover ? '// &
     &                                    '(Yes/No/Abort) '
                     READ  ( 5, '(A1)'  ) ANS
                     CALL CASEFOLD ( ANS )
                     IF ( ANS .EQ. 'N' ) THEN
                          WRITE ( 6, FMT='(A)' ) '  Not recovering'
                          RESTRT = .FALSE.
                        ELSE IF ( ANS .EQ. 'A' ) THEN
                          CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
                        ELSE IF ( ANS .EQ. 'Y' ) THEN
                          CONTINUE
                          WRITE ( 6, * ) 'Restarting...'
                        ELSE
                          GOTO 740
                     END IF
                END IF
           ENDIF ! restrt
      ENDIF ! restart negotication logic
!
! --- If a recover was asked for and it is possible, then do it
! --- (implicit else)
!
      IF ( .NOT. RESTRT ) RETURN ! Nothing to do: go home
!
! --- Recovering logic
!
      WRITE  ( 6, 150 ) PARCNM, NARCS, SAVED_ARCNAME_MES
 150  FORMAT ( 'Recover after processing the ', I5, &
     &         '-th session out of (',I5,'): ',A )
!
! --- Recovery logic, if built and copied, then recover is clean
!
      IF ( BUILT  .AND.  .NOT. COPIED ) THEN
           DBNAME_MES = SAVED_ARCNAME_MES
           CALL SAVES ( RESTRT, STIME0, STIMP0, SCNOUT, KCORL, IEOPLL, &
     &                  DBNAME_MES, LENCNT, LENARC )
        ELSE IF ( .NOT. BUILT ) THEN
!@           CALL EOF_SPOOL ( PSPPOS, 'M' )
           IF ( IEOPLL .NE. 0 ) CALL EOF_SPLLK ( EOPLPOS, PRE_SCR_DIR, &
     &                                           EOPL_BASE, EOPL_LU, 'M' )
           IF ( ISLTY2.EQ.'B' .AND. KCORL ) CALL EOF_CVRF ( PCVPOS,  'M' )
!
! -------- Handle case of restart at beginning of back solution
!
           IF ( (IARCNM .LT. PARCNM)  .AND.  (IARCRC .LT. PARCRC)  .AND. &
     &          (ISLTY2 .EQ. 'B' )                                       ) THEN
!
                IF ( IARCNM .GT. 0 ) THEN
                     IARCNM = IARCNM-1
                     IARCRC = IARCRC-1
                END IF
              ELSE
                IARCNM = PARCNM
                IARCRC = PARCRC
                IIPASS = PIPASS
                ISLTY2 = PSLTY2
           ENDIF
!
           STIME0   = PTIME0
           STIMP0   = PTIMP0
           SLAST    = PLAST
           ISARRC   = PSARRC
           CWRMS(1) = PWRMS(1)
           CWRMS(2) = PWRMS(2)
           CFACT(1) = PFACT(1)
           CFACT(2) = PFACT(2)
           CNPARAM  = PNPARAM
           CNCSUM   = PNCSUM
           CKCSUM   = PKCSUM
           CSHARE   = PCSHARE
           FS_FULL(1) = .FALSE.
           FS_FULL(2) = .FALSE.
           FS_FULL(3) = .FALSE.
           RECVR    = 1
!
           CALL USE_GLBFIL ( 'OWC' )
!
! -------- Write previous SARFxx last record to SARFxx
!
           CALL SARST ( ISARRC, INT2(1) )
!
           IF ( ISLTY2 .EQ. 'F'  .AND.  .NOT. SLAST ) THEN
              IF ( TRAIN ) THEN
!
! ---------------- Copying stuff from CGMBxx to CGMFxx  in train mode
!
!@                   JBLOCKS = JRND_BLOCKS(MAT_E(MAX_PAR,TGLBLS)*REALL_WORDS) + &
!@     &                       JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
                  LEN8_BYTES  = 8*(3*M_GPA + INT8(TGLBLS)*INT8(TGLBLS+1)/2)
                  LEN8_BLOCKS = (LEN8_BYTES+INT8(255))/INT8(256) + &
     &                          JBLOCKS + JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
                  CALL COPY_FILE ( PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS, &
     &                             PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS, &
     &                             'Q', INT(LEN8_BLOCKS,KIND=4) )
              END IF
           ENDIF
      ENDIF
!
! --- Restore state
!
      IPASS  = IIPASS
      ARCNUM = IARCNM
      SOLTY2 = ISLTY2
      ETIME0 = STIME0
      ETIMP0 = STIMP0
      CALL STIMER ( ETIME0 )
      CALL CFSPOS ( IARCRC )
!
! --- We are done
!
      RETURN
      END  !#!  RSTORS   #!#
