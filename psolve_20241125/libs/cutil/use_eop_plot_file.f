      SUBROUTINE use_eop_plot_file(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     This is a routine which opens or closes the file of high frequency
!     adjustemnts for use in MDLPL.
!
      INCLUDE 'solve.i'
!
      CHARACTER*(*) STRING
      CHARACTER  FILDIR*128
!
!     STRING - CHARACTER STRING WITH ONE CHARACTER PER OPERATION
!              O - OPEN
!              C - CLOSE
!
!     OUTPUT Variables:none
!
!     COMMON BLOCKS
      INCLUDE 'precm.i'
!
!     LOCAL VARIABLES
      INTEGER*2 COUNT,MCOUNT,TRIMLEN,IL,IDUM
      INTEGER*4 IERR, EOFPOS,I4DUM,dum,fc_lseek,offset,whence,fd
      INTEGER*4 UNIT_TO_FILDESC
      CHARACTER*63 FNAME
      CHARACTER*1 TOKEN
      CHARACTER*150 errstr
      LOGICAL*2 eof
!
!     HISTORY
!     JWR :97.03.20: Crafted from use_spllf
!
      FNAME=PRE_SCR_DIR(:PRE_SD_LEN)//'EOPP'//PRE_LETRS
      IL=TRIMLEN(FNAME)
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
        TOKEN=STRING(COUNT:COUNT)
1       CONTINUE
!
!       Open
!
        IF(TOKEN.EQ.'O') THEN
          OPEN(EOPL_LU,FILE=FNAME(1:IL),IOSTAT=IERR,STATUS='UNKNOWN')
          IF(IERR.NE.0) THEN
            call ferr( INT2(224), errstr, INT2(0), INT2(0) )
            GO TO 1
          ENDIF
!
!       CLOSE
        ELSE IF(TOKEN.EQ.'C') THEN
          CLOSE(EOPL_LU,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE ( errstr, "('Error ',I7,' closing, ',A)" ) IERR, FNAME(1:IL)
            call ferr( INT2(229), errstr, INT2(0), INT2(0) )
            GO TO 1
          END IF
!
!       UNKOWN CONTROL
        ELSE
          WRITE(errstr, &
     &    "('Unknown USE_EOP_PLOT_FILE access control: ',A)") TOKEN
          call ferr( INT2(232), errstr, INT2(0), INT2(0) )
          GO TO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END
