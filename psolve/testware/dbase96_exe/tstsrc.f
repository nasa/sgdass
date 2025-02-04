      PROGRAM TSTSRC
!
!  purpose
!
!  Modifications:
!   BA  95.09.19  Removed unused variables.
!   BA  95.10.19  Maximum number of radio sources changed from 100 to
!                 300 (current Solve maximum).
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 LUI,LUO,IOBS,IXFER_LIM,IMPAR(5),IINIT, &
     &   IVERO,KERR, &
     &   NDO(3)
      REAL*8 STAR_DATA(2,300)
      CHARACTER*64 LTEXT_STAR(2)
      CHARACTER*50 HIST_MESSAGE
      CHARACTER*10 CHR_LKOUT,CHR_LKYNM,BLANK_LKYNM
      CHARACTER*80 LFIO_CHR,LFTO_CHR
!
      DATA BLANK_LKYNM /'          '/
!
      LUI=IMPAR(1)
      LUO=IMPAR(2)
      IF(LUI.EQ.0) LUI=5
      IF(LUO.EQ.0) LUO=6
!
      CALL DATSV()
!
      LTEXT_STAR(1)(1:27) = '1950 star values - testing '
      LTEXT_STAR(2)(1:27) = '2000 star values - testing '
      DO IINIT = 28,64
        LTEXT_STAR(1)(IINIT:IINIT) = ' '
        LTEXT_STAR(2)(IINIT:IINIT) = ' '
      END DO
!
      WRITE(6,'("GIVE ME AN INPUT DATA BASE NAME ",$)')
      READ(5,'(A10)') CHR_LKYNM
      WRITE(6,'("GIVE ME AN OUTPUT DATA BASE NAME ",/, &
     &  "   (0 for same as input) ",$)')
      READ(5,'(A10)') CHR_LKOUT
      IF (CHR_LKOUT(1:1) .EQ. '0') CHR_LKOUT(1:4) = 'SAME'
!
      LFTO_CHR(1:4) = 'SAME'
!
      IXFER_LIM = 32740
!      Get values to be entered into the data base
!
      CALL KAI( INT2(1), INT2(0), INT2(0), INT2(1), CHR_LKYNM, INT2(0), &
     &     BLANK_LKYNM, LFTO_CHR, IVERO, LFIO_CHR, KERR )
      WRITE(6, &
     &     "(' KERR FROM KAI 1 IS ',I2,' FOR DB ',A10)")KERR,CHR_LKYNM
      IF(KERR.NE.0) GO TO 1000
!
      CALL MVREC( INT2(1), INT2(1), INT2(1), KERR )
!
! 1.
!
        CALL GETR( 'STAR2000', star_data, INT2(2), INT2(300), INT2(1), ndo, &
     &       kerr )
!
      IF (KErr.NE.0 .AND. KErr .NE. 2) THEN
        CALL GETR( 'STARCRDS', star_data, INT2(2), INT2(300), INT2(1), ndo, &
     &       kerr )
      END IF
      IF (KERR .NE. 0 .AND. KErr .NE. 2) THEN
        WRITE(6, &
     &    '("ERROR ",I7," GETTING STAR2000")')KERR
        CALL FINIS( INT2(1) )
        GO TO 1000
      END IF
!
      CALL FINIS( INT2(0) )
!
      if (CHR_LKYNM(2:3).EQ.'85') then ! using $85SEP05AE (OJ287 is record #1)
        WRITE(6,'("PLEASE ENTER A NEW RA FOR SOURCE 1 ")')
        READ(5,*) STAR_DATA(1,1)
        WRITE(6,'("PLEASE ENTER A NEW DEC FOR SOURCE 1 ")')
        READ(5,*) STAR_DATA(2,1)
      else ! $90SEP04AE used ( OJ287 is record #25)
        WRITE(6,'("PLEASE ENTER A NEW RA FOR SOURCE 1 ")')
        READ(5,*) STAR_DATA(1,25)
        WRITE(6,'("PLEASE ENTER A NEW DEC FOR SOURCE 1 ")')
        READ(5,*) STAR_DATA(2,25)
      end if
!
!     Now, do the actual data base changes
!
      CALL KAI( INT2(2), INT2(0), INT2(0), INT2(1), CHR_LKYNM, INT2(0), &
     &     CHR_LKOUT, LFTO_CHR, IVERO, LFIO_CHR, KERR )
      WRITE(6, &
     &     "(' KERR FROM KAI IS ',I2,' FOR DB ',A10)")KERR,CHR_LKYNM
      IF(KERR.NE.0) GO TO 1000
!
      WRITE(6,"('HISTORY MESSAGE ? ',$)")
      READ(5,"(A50)") HIST_MESSAGE
      CALL PHIST( INT2(25), HIST_MESSAGE )
!
!     Adds - work with 1950
!
      CALL ADDR( INT2(1), 'STARCRDS......', LTEXT_STAR(1), INT2(2), ndo(2), &
     &     INT2(1) )
!
!     Ready to change the info in rec 1
!
      CALL MVREC( INT2(1), INT2(1), INT2(1), KERR )
!
!     Now put out the new values and the new number of values (6)
!
      CALL PUTR( 'STARCRDS......', STAR_DATA, INT2(2), ndo(2), INT2(1) )
!
!     write out record 1
!
      CALL WRIDR()
!
!     Write out as much of the rest of the data base as the user wants
!
      KERR = 0
      IOBS = 1
      DO WHILE(KERR .EQ. 0 .AND. IOBS.LE.IXFER_LIM)
        CALL MVREC( INT2(2), INT2(1), INT2(1), KERR )
!
        IF(KERR .EQ. 0) THEN
          CALL WRIDR()
          CALL MVREC( INT2(3), INT2(1), INT2(1), KERR )
          CALL WRIDR()
        END IF
        IOBS = IOBS + 1
      END DO
!
      CALL FINIS( INT2(2) )
 1000 CONTINUE
      END
