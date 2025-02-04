      SUBROUTINE COMP ( VS, NPARMC, ISTA, TYPEPR, LABEL, UNITS, LSITEF, &
     &                  AP_XYZ, PTYPE, MAT, M_SAV, L_SAV, ADR_SAV, VAL_SAV, &
     &                  LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT NONE
!
! 1.  COMP PROGRAM SPECIFICATION
!
! 1.1 Print out station parameters (coordinats or velocities) in UEN
!     coordinate system.
!
! 1.2 REFERENCES:
!
! 2.  COMP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER TYPEPR*(*), LABEL(MAX_PWC_EPS)*(*), UNITS*(*), PTYPE*(*)
      REAL*8    VS(3,MAX_PWC_EPS), AP_XYZ(3), MAT(*)
      INTEGER*4 NPARMC
      INTEGER*2 ISTA, LSITEF(STA_BIT_WORDS,3)
      INTEGER*4 M_SAV
      INTEGER*4 LBUF_LEN, IPTR, PAGEWID
      CHARACTER LBUF(LBUF_LEN)*120
!
! AP_XYZ - a priori geocentric coordinates
! ISTA - Station number
! LABEL - Date or parm type for inclusion in parameter name
! LSITEF - Flag indicating which sites are turned on
! NPARMC - Parameter number before that about to be printed
! PTYPE - Parameter type label ('Comp' or 'Velo')
! TYPEPR - General parameter type ('global' or ' ')
! UNITS - Units to be printed with parameter values
! VS - Parameter adjustments in XYZ coordinate frame
!
! 2.3 OUTPUT Variables: None
!
      INTEGER*4  L_SAV
      ADDRESS__TYPE :: ADR_SAV(M_SAV)
      REAL*8     VAL_SAV(M_SAV)
!
! MAT     -- Vector of _estimates + covranice matrix
! L_SAV   -- Number of saved values
! ADR_SAV -- Array of addresses of saved values
! VAL_SAV -- Array of values of saved values
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
      REAL*8            MAT1(3,3)
      COMMON / ROTEMA / MAT1
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: utility libraries
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 NP(3,MAX_PWC_EPS), NPARM
      INTEGER*2 I, J, K, L, ICMP(3), ILU, &
     &          SNUM, II, TRIMLEN
      INTEGER*8 INDX8
      LOGICAL*2 KBIT
      REAL*8    AL(3,3), BL(3), SIGUEN(3,MAX_PWC_EPS), &
     &          SCSIGUEN(3,MAX_PWC_EPS)
      REAL*8    AUEN(3,3,MAX_PWC_EPS), BUEN(3,MAX_PWC_EPS), &
     &          VSUEN(3,MAX_PWC_EPS), VSXYZ(3), AXYZ(3,3), BXYZ(3)
      INTEGER*4 JA, JB, JS
!
      DATA ICMP/2HU ,2HE ,2HN /
!
! 4.  HISTORY
!  WHO  WHEN       WHAT
!
!  pet  12-MAR-99  Updated comments. Added actual arguments M_SAV, L_SAV,
!                  ADR_SAV, VAL_SAV and calls ADD_SAV in order to keep changes
!                  of covariance matrix and be able to restore it.
!  pet 1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                  eliminated common block adj_buf
!  pet 2005.03.30  Forced saving mot only spoiled elements of MAT(JA+...), &
!                  but also MAT(JB+...)
!
! 5.  COMP PROGRAM STRUCTURE
!
! Initialization
!
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS = M_GPA
!
      SNUM = 1
      IF ( PSITED(ISTA) .NE. 0  .AND.  PTYPE .NE. 'Velo' ) SNUM = PWCNUM(1)
      IF ( PTYPE .EQ. 'Velo' ) LABEL(1) = 'Velo  '
      ILU = TRIMLEN(UNITS)
!
      DO II = 1,SNUM
         NPARM=NPARMC + II - SNUM
         DO I=1,3
            DO J=1,3
               AL(J,I)=0.0D0
               AUEN(J,I,ii)=0.0D0
            ENDDO
!
            BL(I)=0.0D0
            BUEN(I,II)=0.0D0
            VSUEN(I,II)=0.0D0
            NP(I,II)=0
!
! --------- Determine which of the three components were estimated
!
            IF ( KBIT(LSITEF(1,I), ISTA) ) THEN
                 NPARM    = NPARM + SNUM
                 NP(I,II) = NPARM
            ENDIF
         ENDDO
!
! ------ Extract the portions of the solve matrices that we need
!
         DO I=1,3
            IF ( NP(I,II) .NE. 0 ) THEN
                 DO J=1,3
                    IF ( NP(J,II) .NE. 0 ) THEN
                         AL(J,I) = MAT(JA+INDX8(NP(J,II),NP(I,II)))
                    ENDIF
                 ENDDO
!
                 BL(I) = MAT(JB+NP(I,II))
            ENDIF
         ENDDO
!
! ------ Produce rotation matrix, using a priori values
!
         CALL UEN_ROT ( AP_XYZ, MAT1 )
!
! ------ Now apply the rotation matrix to calculate the UEN parameters
!
         DO I=1,3
            DO J=1,3
               BUEN  (I,II) = BUEN(I,II)  + MAT1(I,J)*BL(J)
               VSUEN (I,II) = VSUEN(I,II) + MAT1(I,J)*VS(J,II)
!
               DO K=1,3
                  DO L=1,3
                     AUEN(I,J,II) = AUEN(I,J,II)+MAT1(I,K)*AL(K,L)*MAT1(J,L)
                  ENDDO
               ENDDO
            ENDDO
!
            IF ( AUEN(I,I,II) > 1.D-30 ) THEN
                 SIGUEN(I,II)   = SQRT(AUEN(I,I,II))
               ELSE 
                 SIGUEN(I,II)   = 0.0D0
            END IF
            SCSIGUEN(I,II) = SIGUEN(I,ii)*WRMS(3)
         ENDDO
      ENDDO
!
      DO I = 1,3        ! Run over U,E,N.
         DO II = 1,SNUM
!
! ------ If in batch, printing globals, and global parm, then print it
! ------ else if not in batch, then print parm
!
         IF ( KSPOOL ) THEN
              WRITE ( 23, 1000 )  (ISITN(K,ISTA),K=1,4), &
     &                (MONUMENTS(K,ISTA),K=1,5), ICMP(I), LABEL(II), &
     &                 VSUEN(I,II)*1000.D0, UNITS, &
     &                 BUEN(I,II)*1000.D0, UNITS, SIGUEN(I,II)*1000.D0, &
     &                 UNITS, SCSIGUEN(I,II)*1000.D0, UNITS, TYPEPR
 1000         FORMAT ( 5X,"  ",4A2,1X,5A2,1X,A2,A6,2X, &
     &                 F16.2 ' ',A5, &
     &                 F12.3,' ',A5,' ', &
     &                 F15.3,' ',A5,' ', &
     &                 F15.3,' ',A5,' ',A6 )
         END IF
!
         IF ( KSCREEN ) THEN
              IPTR=IPTR+1
              WRITE ( LBUF(IPTR), 3500 ) (ISITN(K,ISTA),K=1,4), &
     &                (MONUMENTS(K,ISTA),K=1,5), &
     &                ICMP(I), LABEL(II), VSUEN(I,II)*1000.0D0, UNITS, &
     &                BUEN(I,II)*1000.D0, UNITS, &
     &                SCSIGUEN(I,II)*1000.D0, UNITS(1:ILU)
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
 3500         FORMAT ( 5X,"  ",4A2,1X,5A2,1X,A2,A4,F16.2, &
     &                 ' ',A5,F8.2,' ',A5,F7.2,A)
         ENDIF
       ENDDO
      END DO ! Run over U,E,N
!
! --- Now move the uen covariance and adjustments into A and B
!
      DO II=1,SNUM
         DO I=1,3
            IF ( NP(I,II) .NE. 0 ) THEN
                 DO J=1,I
                    IF ( NP(J,II) .NE. 0 ) THEN
                         AXYZ(J,I) = MAT(JA+INDX8(NP(J,II),NP(I,II)))
                         CALL ADD_SAVE ( M_SAV, L_SAV, ADR_SAV, VAL_SAV, &
     &                                   MAT(JA+INDX8(NP(J,II),NP(I,II))) )
                         MAT(JA+INDX8(NP(J,II),NP(I,II))) = AUEN(J,I,II)
                    ENDIF
                 ENDDO
!
                 BXYZ(I) = MAT(JB+NP(I,II))
                 CALL ADD_SAVE ( M_SAV, L_SAV, ADR_SAV, VAL_SAV, &
     &                           MAT(JB+NP(I,II)) )
                 MAT(JB+NP(I,II)) = BUEN(I,II)
            ENDIF
            VSXYZ(I) = VS(I,II)
            VS(I,II) = VSUEN(I,II)
         ENDDO
!
         IF ( PTYPE .EQ. 'Comp' ) THEN
              IF ( KPOSELL ) THEN
                   CALL ERRCMP_POS ( VS, NPARMC, LSITEF, ISTA, MAT )
              END IF
!
              DO I=1,3
                 IF ( NP(I,II) .NE. 0 ) THEN
                      DO J=1,I
                         IF ( NP(J,II) .NE. 0 ) THEN
                              CALL ADD_SAVE ( M_SAV, L_SAV, ADR_SAV, VAL_SAV, &
     &                                        MAT(JA+INDX8(NP(J,II),NP(I,II))) )
                              MAT(JA+INDX8(NP(J,II),NP(I,II))) = AXYZ(J,I)
                         ENDIF
                      ENDDO
!
                      CALL ADD_SAVE ( M_SAV, L_SAV, ADR_SAV, VAL_SAV, &
     &                                MAT(JB+NP(I,II)) )
                      MAT(JB+NP(I,II)) = BXYZ(I)
                 ENDIF
!
                 VS(I,II) = VSXYZ(I)
              ENDDO
          ENDIF
      ENDDO
!
      RETURN
      END  !#!  COMP  #!#
