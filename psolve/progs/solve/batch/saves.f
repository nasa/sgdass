      SUBROUTINE SAVES ( KMORE, ETIME0, ETIMP0, SCNOUT, KCORL, IEOPLL, &
     &                   ARCNAME_MES, LENCNT, LENARC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SAVES PROGRAM SPECIFICATION
!
! 1.1 Save the state of the program so we can recover cleanly.
!
! 1.2 REFERENCES:
!
! 2.  SAVES INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 SCNOUT, ARCNAME_MES
      INTEGER*4 ETIMP0,ETIME0,SARREC
      LOGICAL*2 KMORE,KCORL
      INTEGER*2 IEOPLL, LENCNT, LENARC
!
! ETIME0 - Beginning processing time of current arc
! ETIMP0 - Beginning processing time of current pass
! KCORL  - True if we are to output covariances
! KMORE  - True if there is more data to process
! SARREC - Position of the last record in SARFxx
! SCNOUT - Screen output flag
! IEOPLL - Earth orientation plot flag
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces,rstors
!       CALLED SUBROUTINES: sarst
!
! 3.  LOCAL VARIABLES
!
      CHARACTER BUFF*100, TEMP*100
      INTEGER*2 IBUFF(50)
      INTEGER*2 IL2, CREATE_CGMF
      INTEGER*8  LEN8_BYTES, LEN8_BLOCKS
      INTEGER*4 JRND_BLOCKS,JBLOCKS,MAT_E
!
      EQUIVALENCE (IBUFF,BUFF)
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   pet   980204  Put call of copy_file instead of call of the program COPYQ
!   pet   980724  Corrected abug: previous version created temporary CGM only
!                 if the first arc was processed, but not the first saved arc!
!   pet   990112  Added support of NO TRAIN mode
!   pet   990418  Added saving the name of the last database processed
!   pet   990419  Added saving lenght (in lines) of the control file and
!                 arc file
!
!CCC
!
! 5.  SAVES PROGRAM STRUCTURE
!
!  SET UP GLBFIL
!
      CALL USE_GLBFIL ( 'OR' )
!
! --- Save variables associated with the arc just finished
! --- That haven't been saved already
!
! --- Elapsed time
!
      STIME0 = ETIME0
      STIMP0 = ETIMP0
!
! --- And the end of the sarfile sarfXX
!
      CALL SARST ( SARREC, INT2(0) )
      ISARRC = SARREC
!
! --- Set BUILT TRUE, indicating we have completed an arc
!
      BUILT  = .TRUE.
      COPIED = .FALSE.
!
! --- Set KMORED (resztart is pissiple) so if we are done,
! --- we won't try to recover
!
      KMORED = KMORE
!
! --- Save session name
!
      SAVED_ARCNAME_MES = ARCNAME_MES
!
! --- Save lenght of the control file and arc file
!
      LENCNT_SAVED = LENCNT
      LENARC_SAVED = LENARC
!
! --- Write glbcm
!
      CALL USE_GLBFIL ( 'W' )
!
! --- Copy the output cgm if we are in a forward pass after the first saved arc
!
      IF ( KMORE ) THEN
           IF ( ISLTY2.EQ.'F' .AND. .NOT. SLAST ) THEN
              IF ( IARCNM .EQ. 1   .OR.   IARCNM .LE. SAVING_RATE  ) THEN
!
! ---------------- We do it eighter after the first arc in the arc list or
! ---------------- after the first arc to be saved
!
                   TEMP = 'SCRATCHB'
                   IL2 = CREATE_CGMF ( TEMP, ' ', NRMFL_PARMS, 'U', ' ' )
                   IF ( IL2 .NE. 0 ) THEN
                        CALL FERR ( INT2(9101), 'BATCH: saves errors from '// &
     &                      'CREATE_CGMF', INT2(0), INT2(0) )
                   END IF
              ENDIF
              IF ( TRAIN ) THEN
!
! ---------------- Copying stuff from CGMFxx to CGMBxx
!
!@                 JBLOCKS = JRND_BLOCKS(MAT_E(MAX_PAR,TGLBLS)*REALL_WORDS) + &
!@     &                     JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
                   LEN8_BYTES  = 8*(3*M_GPA + INT8(TGLBLS)*INT8(TGLBLS+1)/2)
                   LEN8_BLOCKS = (LEN8_BYTES+INT8(255))/INT8(256) + &
     &                           JBLOCKS + JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
                   CALL COPY_FILE ( PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS, &
     &                            PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS, &
     &                            'Q', INT(LEN8_BLOCKS,KIND=4) )
              ENDIF
           ENDIF ! islty2
!
! -------- Copy perishables variables
!
           PARCNM = IARCNM
           PONAMC = ONAMCG
           PSLTY2 = ISLTY2
           PIPASS = IIPASS
!
! -------- Mark the current position in spool file and return it
!
           CALL EOF_SPOOL ( PSPPOS, 'G' )
!
! -------- Close also some other files
!
           IF ( IEOPLL .NE. 0 ) THEN
                CALL EOF_SPLLK ( EOPLPOS, PRE_SCR_DIR, EOPL_BASE, EOPL_LU, 'G' )
           END IF
           IF ( ISLTY2 .EQ. 'B'  .AND.  KCORL ) THEN
                CALL EOF_CVRF ( PCVPOS, 'G' )
           END IF
!
           PARCRC    = IARCRC
           PTIME0    = STIME0
           PTIMP0    = STIMP0
           PLAST     = SLAST
           PSARRC    = ISARRC
           PWRMS(1)  = CWRMS(1)
           PWRMS(2)  = CWRMS(2)
           PFACT(1)  = CFACT(1)
           PFACT(2)  = CFACT(2)
           PNPARAM   = CNPARAM
           PNCSUM    = CNCSUM
           PKCSUM    = CKCSUM
           PCSHARE   = CSHARE
           TRAIN_CGM = TRAIN
!
! -------- Set COPIED TRUE, indicating we have a backup copy of all
! -------- crucial variables and data
!
           COPIED = .TRUE.
      ENDIF
!
! --- Write and close GLBCM
!
      CALL USE_GLBFIL ( 'WC' )
!
      RETURN
      END  !#!  SAVES  #!#
