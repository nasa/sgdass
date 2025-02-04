      SUBROUTINE COMPMIN ( VS, NPARMC, ISTA, TYPEPR, LABEL, UNITS, LSITEF, &
     &                     AP_XYZ, PTYPE, MAT, LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COMPMIN PROGRAM SPECIFICATION
!
! 1.1 Print out station parameters for epoch minimizing sigmas, in UEN coordinate system.
!
! 1.2 REFERENCES:
!
! 2.  COMPMIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TYPEPR,LABEL,UNITS,ptype
      REAL*8 VS(3),AP_XYZ(3),mat(*)
      INTEGER*4   NPARMC
      INTEGER*2   ISTA,LSITEF(STA_BIT_WORDS,3)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
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
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
      REAL*8 MAT1(3,3)
      COMMON/ROTEMA/MAT1
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: utility libraries
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,L,ICMP(3), ilu, trimlen
      INTEGER*4 NP(3), NPARM
      INTEGER*8, EXTERNAL :: INDX8
      INTEGER*4, EXTERNAL :: INDX4
      LOGICAL*2 KBIT
      REAL*8 AL(3,3),BL(3),XYZ(3),SIGUEN(3), &
     &   SCSIGUEN(3)
      REAL*8 AUEN(3,3),BUEN(3), &
     &   VSUEN(3),vsxyz(3),axyz(3,3),bxyz(3)
!
      DATA ICMP/2HU ,2HE ,2HN /
!
! 4.  HISTORY
!  WHO  WHEN       WHAT
!  pet 1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                  eliminated common block adj_buf
!
! 5.  COMP PROGRAM STRUCTURE
!
! Initialization
!
      ilu=trimlen(units)
      NPARM=NPARMC
      DO I=1,3
        DO J=1,3
          AL(J,I)=0.0D0
          AUEN(J,I)=0.0D0
        ENDDO
        BL(I)=0.0D0
        BUEN(I)=0.0D0
        VSUEN(I)=0.0D0
        NP(I)=0
!
! determine which of the three components were estimated
!
        IF(KBIT(LSITEF(1,I),ISTA)) THEN
           NPARM=NPARM + 1
           NP(I)=NPARM
        ENDIF
      ENDDO
!
! Extract the portions of the solve matrices that we need
!
      DO I=1,3
         IF ( NP(I) .NE. 0 ) THEN
              DO J=1,3
                 IF ( NP(J) .NE. 0 ) THEN
                      AL(J,I) = MAT ( INDX4( (J-1)*2+1, (I-1)*2+1 ) )
                 ENDIF
              ENDDO
              BL(I) = VS(I)
         ENDIF
      ENDDO
!
! Produce rotation matrix, using a priori values
!
      CALL UEN_ROT(AP_XYZ,MAT1 )
!
! Now apply the rotation matrix to calculate the UEN parameters
!
      DO I=1,3
        DO J=1,3
          BUEN(I)=BUEN(I)+MAT1(I,J)*BL(J)
          VSUEN(I)=VSUEN(I)+MAT1(I,J)*VS(J)
          DO K=1,3
            DO L=1,3
              AUEN(I,J)=AUEN(I,J)+MAT1(I,K)*AL(K,L)*MAT1(J,L)
            ENDDO
          ENDDO
        ENDDO
        SIGUEN(I)=SQRT(AUEN(I,I))
        SCSIGUEN(I)=SIGUEN(I)*WRMS(3)
      ENDDO
!
      DO I = 1,3          !Run over U,E,N.
!
! ------ if in batch, printing globals, and global parm, then print it
! ------ else if not in batch, then print parm
!
         IF ( KSPOOL ) THEN
              WRITE ( 23, 1000 ) (ISITN(K,ISTA),K=1,4), &
     &              ( MONUMENTS(K,ISTA), K=1,5 ), &
     &              ICMP(I), PTYPE, VSUEN(I)*1000.D0,UNITS, &
     &              BUEN(I)*1000.D0, UNITS, SIGUEN(I)*1000.D0, UNITS, &
     &              SCSIGUEN(I)*1000.D0, UNITS, TYPEPR
 1000         FORMAT ( 5X,"  ",4A2,1X,5A2,1X,A2,A4,4X, &
     &                 F16.2 ' ',A5, &
     &                 F12.3,' ',A5,' ', &
     &                 F15.3,' ',A5,' ', &
     &                 F15.3,' ',A5,' ',A6)
         END IF
!
         IF ( KSCREEN ) THEN
              IPTR=IPTR+1
              WRITE ( LBUF(IPTR), 3500 ) (ISITN(K,ISTA),K=1,4), &
     &               ( MONUMENTS(K,ISTA),K=1,5), &
     &               ICMP(I), PTYPE, VSUEN(I)*1000.0D0, UNITS, &
     &               BUEN(I)*1000.D0, UNITS, &
     &               SCSIGUEN(I)*1000.D0, UNITS(1:ILU)
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
 3500         FORMAT ( 5X, "  ",4A2,1X,5A2,1X,A2,A4,F16.2, &
     &                ' ',A5,F8.2,' ',A5,F7.2,A)
         ENDIF
      END DO       ! Run over U,E,N
!
      RETURN
      END   !#!  COMPMIN  #!#
