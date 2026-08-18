      SUBROUTINE CINDEX_PARM_o ( PARMTYP, PARMIDX, LPARM, NPARAMS, &
     &                           END_IDX, WHO, KWHO, STA)
      IMPLICIT NONE
! Character version of INDEX_PARM_o
!
! 1.  INDEX_PARM_O PROGRAM SPECIFICATION
!
! 1.1 Set up cross-referencing between parameter sublists and
!       the complete list.
!
! 1.2 REFERENCES:
!
! 2.  INDEX_PARM_O INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NPARAMS
      character*(*) LPARM(NPARAMS)
      CHARACTER*(*) PARMTYP,STA
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
      INTEGER*4 PARMIDX(M_GPA), END_IDX
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
      INTEGER*4 I,J,K
      INTEGER*2 R,LINE,IARCS,IGLBLS,LENGTH, &
     &    TrimLen,INDEX
      CHARACTER  STA_A1_BIG*10, STA_A1_SML*10, STA_C1_BIG*10, STA_C1_SML*10 
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   KDB   09/18/90  Was indx_parm under proc subdirectory.  Moved to cutil
!                   for use by adjst as well
!
! 5.  INDEX_PARM_O PROGRAM STRUCTURE
!
!   initialize parameter sublist
!
      IF ( LEN(STA) .GE. 8 ) THEN
           STA_A1_BIG = STA(1:8)//'A1'
           STA_A1_SML = STA(1:8)//'a1'
           STA_C1_BIG = STA(1:8)//'C1'
           STA_C1_SML = STA(1:8)//'c1'
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
              IF(INDEX(LPARM(K),'COMPONENT').NE.0) THEN
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
              IF((INDEX(LPARM(K),'RIGHT A').NE.0) .or. &
     &            (INDEX(LPARM(K),'DECLINA').NE.0)) THEN
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
              IF((INDEX(LPARM(K),'NUT.').NE.0) .or. &
     &            (INDEX(LPARM(K),'NUTATI').NE.0)) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
!
! Handle earth orientation parameters
!
      Else IF(PARMTYP(1:3) .eq. 'EOP') THEN
          I = 1
          Do K = 1, NPARAMS
              IF(((INDEX(LPARM(K),'UT1').NE.0) .or. &
     &            (INDEX(LPARM(K),'WOBBLE').NE.0)) .and. &
     &            (Lparm(k)(10:10).eq.'0')) THEN
                  PARMIDX(I) = K
                  IF(KWHO) WHO(I) =LPARM(k)
                  I = I + 1
              End if
          End do
!
! Handle atmosphere parameters
!
      Else IF(PARMTYP(1:3).EQ.'AT1') THEN
          I=1
          DO K=1,NPARAMS
!@              IF((INDEX(LPARM(K),STA(1:8)//'A1').NE.0) .OR. &
!@     &          (INDEX(LPARM(K),STA(1:8)//'a1').NE.0)) THEN
              IF((INDEX(LPARM(K),STA_A1_BIG ).NE.0 )  .OR. &
     &           (INDEX(LPARM(K),STA_A1_SML ).NE.0 ) )   THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle clock parameters
!
      Else IF(PARMTYP(1:3).EQ.'CL1') THEN
          I=1
          DO K=1,NPARAMS
!@              IF(INDEX(LPARM(K),STA(1:8)//'c1').NE.0) THEN
              IF(INDEX(LPARM(K),STA_C1_SML).NE.0) THEN
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=LPARM(k)
                I=I+1
              ENDIF
          ENDDO
!
! Handle the subset containing all arc parameters
!
      Else IF(PARMTYP(1:3) .eq. 'LOC') THEN
          Do K = 1, IARCS
              PARMIDX(K) = K
          End do
          I = K + 1
!
! Handle the subset containing all global parameters
!
      Else IF(PARMTYP(1:3) .eq. 'GLB') THEN
          Do K = 1, IGLBLS
              PARMIDX(K) = IARCS + K
          End do
          I = K + 1
!
! Handle the entire set of parameters
!
      Else IF(PARMTYP(1:3) .eq. 'ALL') THEN
          Do K = 1, NPARAMS
              PARMIDX(K) = K
          End do
          I = K + 1
!
! Handle an individual parameter
!
      Else !THEN what's specified is a parameter
          Do I = 1, LENGTH
              IF(PARMTYP(I:I) .eq. '_') THEN
                  PARMTYP(I:I) = ' '
              End if
          End do
          I = 1
          Do K = 1, NPARAMS
              IF(INDEX(LPARM(K),PARMTYP(1:length)).NE.0) THEN
                  PARMIDX(I) = K
                  I = I + 1
              End if
          End do
      End if
      END_IDX = I - 1
      RETURN
      END
