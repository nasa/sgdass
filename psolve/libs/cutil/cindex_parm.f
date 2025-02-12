      SUBROUTINE CINDEX_PARM ( PARMTYP, PARMIDX, LPARM, NPARAMS, END_IDX, &
     &                         WHO, KWHO, STA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! character version of index_parm
!
! 1.  INDEX_PARM PROGRAM SPECIFICATION
!
! 1.1 Set up cross-referencing between parameter sublists and
!       the complete list.
!
! 1.2 REFERENCES:
!
! 2.  INDEX_PARM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4     NPARAMS
      CHARACTER*(*) LPARM(nparams)
      CHARACTER*(*) PARMTYP, STA
      LOGICAL*2 KWHO
!
! IWDS - Length of parameter names
! KWHO - True if we are to put list of parameter names in WHO
! NPARAMS - Total number of parameters
! NPARM - List of all parameter names
! PARMTYP - Parameter type (e.g. EOP, AT1, etc.)
! STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4    PARMIDX(M_GPA), END_IDX
      CHARACTER*20 WHO(*)
!
! END_IDX - Number of parameters of the specified type
! PARMIDX - Cross reference to parameter numbers (from parmidx to nparm)
! WHO - Array of parameter names of specified type
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr,do_atm,do_clk
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I,  K, IARCS, IGLBLS
      INTEGER*2  LENGTH
      INTEGER*4  INUM
      INTEGER*2  TRIMLEN
      INTEGER*4  INDEX
      CHARACTER  STA_X*10, STA_Y*10, STA_Z*10, STA_U*10, STA_E*10, STA_N*10, &
     &           STA_A0_BIG*10, STA_A0_SML*10, STA_C0_BIG*10, STA_C0_SML*10, &
     &           STA_NG*10, STA_EG*10
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   KDB   09/18/90  Was indx_parm under proc subdirectory.  Moved to cutil
!                   for use by adjst as well
!   MWH   05/24/91  Modify to accommodate new clock/atmosphere parameterization
!   dsm   05/06/94  Modify to accommodate new gradient parameterization
!   pet   10/06/97  Modify to accommodate new EOP parameterization
!   pet   2002.09.17  Added support of queary parameters GRE for east gradients
!                     and GRN for north gradients
!   pet   2003.08.15  Move out of loop computing temporary strings with
!                     concatenation in order to alleviate a bug in 
!                     HP FORTRAN90 -- this lousy compiler allocated dynamic
!                     memody for a temporary string, but then did not 
!                     deallocate it, what caused sever mereory leakage.
!
! 5.  INDEX_PARM PROGRAM STRUCTURE
!
!   initialize parameter sublist
!
      INTEGER*2 INT2_ARG
!
! --- Set parameters. HP FOTRAN90 has a bug whcih caused a memory leackage
! --- when string concatenation is used inside loop
!
      IF ( LEN(STA) .GE. 8 ) THEN
           STA_X = STA(1:8)//' X'
           STA_Y = STA(1:8)//' Y'
           STA_Z = STA(1:8)//' Z'
           STA_U = STA(1:8)//' U'
           STA_E = STA(1:8)//' E'
           STA_N = STA(1:8)//' N'
           STA_A0_BIG = STA(1:8)//'A0'
           STA_A0_SML = STA(1:8)//'a0'
           STA_C0_BIG = STA(1:8)//'C0'
           STA_C0_SML = STA(1:8)//'c0'
           STA_NG = STA(1:8)//'NG'
           STA_EG = STA(1:8)//'EG'
      END IF
!
      DO I=1,M_GPA
          PARMIDX(I)=0
      ENDDO
      I=0
      LENGTH=TRIMLEN(PARMTYP)
!
!   decide on what to do by looking at what's in PARMTYP
!
! First handle station parameters
!
      IF(PARMTYP(1:3) .eq. 'STA') THEN
          I = 1
          Do K = 1, NPARAMS
             IF ( LPARM(K)(12:20) .EQ. 'COMPONENT' ) THEN
                  PARMIDX(I) = K
                  I = I + 1
             END IF
!
             CALL CHIN ( LPARM(K)(11:16), INUM )
             IF ( LPARM(K)(17:20) .EQ. '-COO'       .AND. &
     &            ( LPARM(K)(10:10).EQ. 'X' .OR.LPARM(K)(10:10) .EQ. 'Y' .OR.LPARM(K)(10:10) .EQ. &
     &           'Z'     )  .AND.INUM .GT. 0  .AND.  INUM .LT. 999999 ) THEN
!
                  PARMIDX(I) = K
                  I = I + 1
             END IF
          End do
!
! Next handle site velocity parameters
!
      Else IF(PARMTYP(1:3) .eq. 'VEX') THEN
          I = 1
          Do K = 1, NPARAMS
              IF(((INDEX(LPARM(K),STA_X) .NE. 0 )  .or. &
     &            (INDEX(LPARM(K),STA_U) .NE. 0 ) ).and. &
     &           INDEX(LPARM(K),'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      Else IF(PARMTYP(1:3) .eq. 'VEY') THEN
          I = 1
          Do K = 1, NPARAMS
              IF(((INDEX(LPARM(K),STA_Y).NE.0).or. &
     &            (INDEX(LPARM(K),STA_E).NE.0)).and. &
     &           INDEX(LPARM(k),'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      Else IF(PARMTYP(1:3) .eq. 'VEZ') THEN
          I = 1
          Do K = 1, NPARAMS
              IF(((INDEX(LPARM(k),STA_Z).NE.0).or. &
     &            (INDEX(LPARM(K),STA_N).NE.0)).and. &
     &           INDEX(LPARM(K),'VELOCITY').NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
!
! Next handle source parameters
!
      Else IF(PARMTYP(1:3) .eq. 'SOU') THEN
          I = 1
          Do K = 1, NPARAMS
              IF((INDEX(LPARM(k),'RIGHT A').NE.0) .or. &
     &            (INDEX(LPARM(k),'DECLINA').NE.0)) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
!
! Next handle baseline clock parameters
!
      Else IF(PARMTYP(1:3) .eq. 'BCL') THEN
          I = 1
          Do K = 1, NPARAMS
            IF(lparm(k)(9:9).eq.'-'.and.lparm(k)(20:20).eq.'C') then
               PARMIDX(I) = K
               I = I + 1
            End if
          End do
!
! Handle nutation parameters
!
      Else IF(PARMTYP(1:3) .eq. 'NUT') THEN
          I = 1
          Do K = 1, NPARAMS
              IF((INDEX(LPARM(k),'NUT.').NE.0) .or. &
     &            (INDEX(LPARM(k),'NUTATI').NE.0)) THEN
                  PARMIDX(I) = K
                  IF(KWHO) WHO(I)=LPARM(k)
                  I=I + 1
              End if
          End do
!
! --- Handle earth orientation offset parameters
!
      ELSE IF ( PARMTYP(1:3) .EQ. 'EOP' ) THEN
          I=1
          Do K=1, NPARAMS
              IF(((INDEX(LPARM(k),'UT1').NE.0) .or. &
     &            (INDEX(LPARM(k),'WOBBLE').NE.0)) .and. &
     &            (lparm(k)(10:10).eq.'0')) THEN
                  PARMIDX(I)=K
                  IF(KWHO) WHO(I)=LPARM(k)
                  I=I + 1
              End if
          End do
!
! --- Handle earth orientation rate parameters
!
      ELSE IF ( PARMTYP(1:3) .EQ. 'EOR' ) THEN
          I=1
          DO K=1, NPARAMS
             IF( ( INDEX ( LPARM(K), 'UT1' )    .NE.  0 .OR. &
     &             INDEX ( LPARM(K), 'WOBBLE' ) .NE.  0     ) .AND. &
     &           ( LPARM(K)(10:10)              .EQ. '1 '   )       ) THEN
!
                  PARMIDX(I)=K
                  IF(KWHO) WHO(I)=LPARM(K)
                  I=I + 1
             END IF
!
             IF ( INDEX ( LPARM(K),'WGRate'   ) .NE. 0 .OR. &
     &            INDEX ( LPARM(K),'UT1GRate' ) .NE. 0     ) THEN
!
                  PARMIDX(I)=K
                  IF(KWHO) WHO(I)=LPARM(K)
                  I=I + 1
             END IF
          END DO
!
! --- Handle atmosphere parameters
!
      Else IF(PARMTYP(1:3).EQ.'AT1') THEN
          I=1
          DO K=1,NPARAMS
              IF((INDEX(LPARM(k),STA_A0_BIG) .NE. 0 ) .OR. &
     &           (INDEX(LPARM(k),STA_A0_SML) .NE. 0 )      ) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
      ELSE IF ( PARMTYP(1:3) .EQ. 'GRA' ) THEN
!
! ------- Handle gradient parameters
!
          I=1
          DO K=1,NPARAMS
             IF ( ( INDEX ( LPARM(K), STA_NG ) .NE. 0 ) .OR. &
     &            ( INDEX ( LPARM(K), STA_EG ) .NE. 0 )      ) THEN
                  PARMIDX(I)=K
                  IF ( KWHO ) WHO(I)=LPARM(K)
                  I=I+1
             END IF
          ENDDO
      ELSE IF ( PARMTYP(1:3) .EQ. 'GRE' ) THEN
!
! ------- Handle east atmopshere gradient parameter
!
          I=1
          DO K=1,NPARAMS
             IF ( INDEX ( LPARM(K), STA_EG ) .NE. 0 ) THEN
                  PARMIDX(I)=K
                  IF ( KWHO ) WHO(I)=LPARM(K)
                  I=I+1
             END IF
          ENDDO
      ELSE IF ( PARMTYP(1:3) .EQ. 'GRN' ) THEN
!
! ------- Handle north atmopshere gradient parameter
!
          I=1
          DO K=1,NPARAMS
             IF ( INDEX ( LPARM(K), STA_NG ) .NE. 0 ) THEN
                  PARMIDX(I)=K
                  IF ( KWHO ) WHO(I)=LPARM(K)
                  I=I+1
             END IF
          ENDDO
!
! Handle clock parameters
!
      Else IF(PARMTYP(1:3).EQ.'CL1') THEN
          I=1
          DO K=1,NPARAMS
              IF((INDEX(LPARM(K),STA_C0_BIG).NE.0) .OR. &
     &           (INDEX(LPARM(K),STA_C0_SML).NE.0)) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (X)
!
      Else IF(PARMTYP(1:3).EQ.'STX') THEN
          I=1
          DO K=1,NPARAMS
              IF(INDEX(LPARM(k),STA_X).NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Y)
!
      Else IF(PARMTYP(1:3).EQ.'STY') THEN
          I=1
          DO K=1,NPARAMS
              IF(INDEX(LPARM(k),STA_Y).NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Z)
!
      Else IF(PARMTYP(1:3).EQ.'STZ') THEN
          I=1
          DO K=1,NPARAMS
              IF(INDEX(LPARM(k),STA_Z).NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle the subset containing all arc parameters
!
      Else IF(PARMTYP(1:3) .eq. 'LOC') THEN
          Do K=1, IARCS
              PARMIDX(K)=K
          End do
          I=K + 1
!
! --- Handle the subset containing all global parameters
!
      Else IF(PARMTYP(1:3) .eq. 'GLB') THEN
          Do K=1, IGLBLS
              PARMIDX(K)=IARCS + K
          End do
          I=K + 1
!
! --- Handle the entire set of parameters
!
      ELSE IF ( PARMTYP(1:3) .EQ. 'ALL' ) THEN
          DO K=1, NPARAMS
              PARMIDX(K)=K
          END DO
          I=K + 1
!
! --- Handle an individual parameter
!
      Else !THEN what's specified is a parameter
          Do I=1, LENGTH
              IF(PARMTYP(I:I) .eq. '_') THEN
                  PARMTYP(I:I)=' '
              End if
          End do
          I=1
          Do K=1, NPARAMS
              IF(INDEX(LPARM(k),PARMTYP(1:length)).NE.0) THEN
                  PARMIDX(I)=K
                  I=I + 1
              End if
          End do
      END IF
      END_IDX=I - 1
!
      RETURN
      END  !#!  CINDEX_PARM  #!#
