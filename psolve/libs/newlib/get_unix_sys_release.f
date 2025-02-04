      SUBROUTINE GET_UNIX_SYS_RELEASE(ISYSTEM_RELEASE)
!
!     Gets the HP-UX system release number (e.g., the 10 in B.10.20).
!     If this is a linux machine or the system release information produced
!       by the uname -r call appears wrong, return an error code instead.
!
      IMPLICIT   NONE
!
      INTEGER*2 ISYSTEM_RELEASE 
!
      CHARACTER*255 UNAME_CON,UNAME_OUTBUF,EMESSAGE
      INTEGER*2 MXOUT,NMOUT,KERR
      INTEGER*2 TRIMLEN,UNAME_OUT_LEN
      INTEGER*2 IDOT_COUNT,ICT,IPT_DOT1,IPT_DOT2
      INTEGER*4 IERR4, I4P0
      DATA I4P0 /0/
!
! 2.9 PROGRAMMER - KDB 1/12/2005 
!
      ISYSTEM_RELEASE = -100
#ifdef LINUX
      ISYSTEM_RELEASE = -999
#else
!     Get the system release output 
      UNAME_CON = 'uname -r'
      MXOUT = 1
      CALL READ_SYSTEMF(UNAME_CON, MXOUT, UNAME_OUTBUF, NMOUT, EMESSAGE, KERR)
      UNAME_OUT_LEN = TRIMLEN(UNAME_OUTBUF)
!     Now parse the output and either extract the release number or identify that
!     there is an invalid release format.  A valid release format will be:
!        B.<number>.<other code> 
      IDOT_COUNT = 0
      DO ICT =1,UNAME_OUT_LEN
        IF (UNAME_OUTBUF(ICT:ICT).EQ.'.') THEN
          IDOT_COUNT = IDOT_COUNT + 1
          IF (IDOT_COUNT.EQ.1) IPT_DOT1 = ICT
          IF (IDOT_COUNT.EQ.2) IPT_DOT2 = ICT
        ENDIF
      END DO
      IF (UNAME_OUTBUF(1:2).EQ.'B.') THEN
        IF (IDOT_COUNT.EQ.2) THEN
          READ(UNAME_OUTBUF(IPT_DOT1+1:IPT_DOT2-1),*,IOSTAT=IERR4,ERR=199) &
     &         ISYSTEM_RELEASE
 199      IF (IERR4.NE.I4P0) THEN
            ISYSTEM_RELEASE = -1
	  ENDIF
        ELSE
          ISYSTEM_RELEASE = -2
        ENDIF
      ELSE
        ISYSTEM_RELEASE = -3
      ENDIF
!
      IF (ISYSTEM_RELEASE.LE.0) THEN
        WRITE(6,"('GET_UNIX_SYS_RELEASE: ERROR ',I10)") ISYSTEM_RELEASE
        WRITE(6,"('  output from uname -r is ',a)") UNAME_OUTBUF
      END IF
#endif
      RETURN
      END
