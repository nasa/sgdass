      SUBROUTINE XDDER ( ARR1, ARR2 )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
      INCLUDE 'addcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'plist.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INTEGER*2 system2_sh,ierr
      CHARACTER  CNAME*(NAME_SIZE)
!
! --- This holds the name of the temporary CGM.
! --- this will be         /solve/work_files/CGMTxx
!
      character CTMP*(NAME_SIZE)
!
! --- This holds the name of the back CGM
!                      /solve/work_files/CGMBxx
!
      CHARACTER CBAK*(NAME_SIZE)
      CHARACTER LCMD*(2*NAME_SIZE+15)
!
      CHARACTER CURPARM*20
      INTEGER*4 JA, JB, JS, FILDES
      INTEGER*8 NELEM, MAT_E4
      LOGICAL*2 KBIT, STACM
      INTEGER*4 REMNUM, REMPARM(M_GPA)
      INTEGER*2 I, J, K, IM, IY, ID, IT, &
     &          CURIY, CURIM, CURID, IDIRECT(BLOCK_WORDS)
      REAL*8    ARR1(*), ARR2(*)
      INTEGER*2 IND
      COMMON / SAVCGM / FILDES, IDIRECT
      INTEGER*2 ISAME(M_GPA)
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  J1, J2, IUER
      INTEGER*4, EXTERNAL :: I_LEN
!
!   MWH  920113  Expanded to handle 1536 parameters (enlarge buffer read
!                 from ADDER)
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   jmg  960610  Speed up.  (Read cgm directly into final array if there are
!                no new global parameters for the current arc.)
!   jmg  961126  Get rid of unused varialbes.
!   jmg          Cleaned up
!   pet  970305  Added stuff for support B1B3D case. Petifized comments
!   pet  971202  Added logic for bypassing deselected station
!   pet  990409  Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!
      JS=1+1*M_GPA
      JB=1+2*M_GPA
      JA=1+3*M_GPA
!
      NELEM = MAT_E4 ( M_GPA, NPARMF )
      ARR1(1:NELEM) = 0.0
!
! --- Fill ARR1 with normal equations & residuals from the CGM
!
      IF ( CGMINN(1:1).NE.' ') THEN
           CNAME=CGMINN
           CALL ACS_CGMFIL ( CNAME, 'O' )
           IF ( NPARMF .EQ. NPARMC ) THEN
                CALL USE_CGMF_MAT ( ARR1, NPARMC, 'R' )
             ELSE
                CALL USE_CGMF_MAT ( ARR2, NPARMC, 'R' )
!
! ------------- Reformat CGM. In a rsult ARR1 will contains only those
! ------------- elements which are in this arc in ARCPE order
!
                CALL REFORMAT ( ARR1(JA), ARR2(JA), ARR1(JB), ARR2(JB), &
     &                          IXCTF, NPARMC )
           ENDIF
           CALL ACS_CGMFIL ( CNAME, 'C' )
      ENDIF
!
! --- Fill EMA matrix ARR2 with FROM THING (ARC OR CGM)
! --- if bit 8 in IBATCH is set, then is of CGM form
!
      CNAME=THINGN
      IF ( ARORCG .EQ. 'ARC' ) THEN
           STACM=KBIT( PRE_IBATCH, INT2(8))
!!           CALL CLRCH ( CNAME )
           CALL ACS_ARCFIL   ( CNAME, STACM, 'O' )
           IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD .OR. &
     &          FAST_MODE .EQ. F__B3D       ) THEN
!
! ------------- Reading information from ARC file in solve format
!
                CALL USE_ARCF_MAT ( ARR2, NPARMT, 'R' )
             ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------- Reading fields of B1B3DOBJ and B3DOBJ objects. B1B3D case
!
                IUER=-1
                CALL RDNOR_B1B3D ( CNAME, B3DOBJ, B1B3DOBJ, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8571, -1, 'XDDER', &
     &                   'Error during reading file '//CNAME(1:I_LEN(CNAME))// &
     &                   ' with temporary '// &
     &                   'data structure for B1B3D algorithm while database '// &
     &                    B3DOBJ%DBNAME_MES//' was processing' )
                     STOP 'ADDER: Abnormal termination'
                ENDIF
!
! ------------- Form array ISAME -- dummy reference. It is done to deceive
! ------------- AMATX_ADDER: we don't need refer elements since AD_W00 contains
! ------------- parameters just in order which CGM has after reodering
!
                DO 410 J1=1,B3DOBJ%N_GLO
                   ISAME(J1)=J1
 410            CONTINUE
!
! ------------- Update CGM
!
                CALL AMATX_ADDER ( ARR1(JA), %VAL(B1B3DOBJ%AD_W00), ARR1(JB), &
     &               %VAL(B1B3DOBJ%AD_Z00), ISAME, INT2(B3DOBJ%N_GLO), ADORSB, &
     &               REMNUM, REMPARM )
           END IF
           CALL ACS_ARCFIL   ( CNAME, STACM, 'C' )
        ELSE
           CALL ACS_CGMFIL   ( CNAME, 'O' )
           CALL USE_CGMF_MAT ( ARR2, NPARMT, 'R' )
           CALL ACS_CGMFIL   ( CNAME, 'C' )
      ENDIF
!
      IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD .OR. &
     &     FAST_MODE .EQ. F__B3D       ) THEN
!
! -------- Update CGM matrix to destination matrix ARR1
!
           CALL AMATX_ADDER ( ARR1(JA), ARR2(JA), ARR1(JB), ARR2(JB), IXTTF, &
     &                  NPARMT, ADORSB, REMNUM, REMPARM )
      END IF
!
! --- Write the result out
!
      IND=INDEX ( CGMINN, 'CGMF' )
!
      CNAME = IONAM
      CALL ACS_CGMFIL ( CNAME, 'O' )
!
! --- If some parameters have disappeared as a result of SUBTRACTing
! --- an arc, handle that here.  The normal matrix has already been
! --- fixed up in AMATX_ADDER.  Here, we will fix the station and source
! --- flags to turn off any parameters which no longer appear in
! --- the normal equations.
!
      IF ( REMNUM.GT.0 ) THEN
         CALL USE_CGMF_COM ( 'R' )
         DO J1=REMNUM,1,-1
            CURPARM = CPARM_NAMES(REMPARM(J1))
            PARM_NUM=PARM_NUM-1
            DO J2=REMPARM(J1),PARM_NUM
               CPARM_NAMES(J2)=CPARM_NAMES(J2+1)
            ENDDO
!
            DO J=1,NUMSTA
               IF ( CURPARM(1:8) .EQ. ISITN_CHR(J) ) THEN
                  IF ( CURPARM(12:20) .EQ. 'COMPONENT' .OR. &
     &                 CURPARM(12:19) .EQ. 'VELOCITY'       ) THEN
                       IF ( VSITED(J).EQ.0 ) THEN
                            DO K=1,3
                               CALL SBIT( LSITEC(1,K), J, INT2(0) )
                               CALL SBIT( LSITEV(1,K), J, INT2(0) )
                            ENDDO
                       ENDIF
                    ELSE IF( VSITED(J) .GT. 0 ) THEN
                       READ ( CURPARM(11:16), '(3I2)' ) CURIY, CURIM, CURID
                       CALL MDYJL(IM,ID,IY,IT,VSITED(J) )
                       IF ( CURIY .EQ. IY  .AND. CURIM .EQ. IM  .AND. &
     &                      CURID .EQ. ID                             ) THEN
                            DO K=1,3
                               CALL SBIT( LSITEC(1,K), J, INT2(0) )
                               CALL SBIT( LSITEV(1,K), J, INT2(0) )
                            ENDDO
                       ENDIF
                  ENDIF
               ENDIF
            ENDDO
!
            DO J=1,NUMSTR
               IF ( CURPARM(1:8) .EQ. ISTRN_CHR(J) ) THEN
                    DO K=1,2
                       CALL SBIT( LSTAR(1,K), J, INT2(0) )
                       CALL SBIT( LPROP(1,K), J, INT2(0) )
                    ENDDO
                 ENDIF
              ENDDO
           ENDDO
           CALL PARCN()
           CALL USE_CGMF_COM ( 'W' )
      ENDIF
!
      CALL USE_CGMF_MAT ( ARR1, NPARMF, 'W' )
      CALL ACS_CGMFIL   ( CNAME, 'C' )
      RETURN
!
500   CONTINUE
      WRITE ( *, * ) "ADDER(xdder): Error executing ",lcmd
      STOP
      END  !#!  XDDER  #!#
