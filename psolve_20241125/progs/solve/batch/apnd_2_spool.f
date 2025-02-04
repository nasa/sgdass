      SUBROUTINE APND_2_SPOOL ( IWARNING, BUF_WARNING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  APND_2_SPOOL PROGRAM SPECIFICATION
!
! 1.1 Append the progress and control files to the end of the spool file.
!     Also append mod files and other parameter files (mwh - 931102)
!
! 1.2 REFERENCES:
!
! 2.  APND_2_SPOOL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
! 2.3 OUTPUT Variables: None
!
!  IWARNING, BUF_WARNING - error code and message for errors which warrant
!      flagging, but not until the actual Solve processing is complete
!
       INTEGER*2 IWARNING
       CHARACTER*248 BUF_WARNING
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'batcm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'ba2cm.i'
      INCLUDE 'pvers.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces
!       CALLED SUBROUTINES: cfspos,cfreadall
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2    M_OUT
      PARAMETER  ( M_OUT=17 )
      CHARACTER  FNAME*(NAME_SIZE), OUTFILE(M_OUT)*(NAME_SIZE)
      CHARACTER  ERRSTR*72, HEADER(M_OUT)*33
      CHARACTER  STRNG*8192, CIB*80
      INTEGER*2  TRIMLEN, LEN, IERR, CFREADALL, I
      LOGICAL*4  LEX
      INTEGER*2  MAXVEROUT,NVEROUT,KVERR
      CHARACTER  VERCOM*200, VEROUT*255, MSGVEROUT*255
      INTEGER*4  IOS
      DATA HEADER /'Episodic motion control file    ,', &
     &             'Piece-wise station position file,', &
     &             'High-frequency eop control file ,', &
     &             'Pressure loading control file   ,', &
     &             'Station position mod file       ,', &
     &             'Station velocity mod file       ,', &
     &             'Source position mod file        ,', &
     &             'Earth Orientation mod file      ,', &
     &             'Axis Offset mod file            ,', &
     &             'Nutation series mod file        ,', &
     &             'Nutation model mod file         ,', &
     &             'Eccentricity file               ,', &
     &             'Mean gradient file              ,', &
     &             'Weight file(1)                  ,', &
     &             'Weight file(2)                  ,', &
     &             'Weight file(3)                  ,', &
     &             'Weight file(4)                  ,'  /
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!   MWH  900720  Include comment lines from the control file
!   JLR  921215  replace 1J with I4P1 variable
!   kdb  961010  Trap sporadic error (suspected emptying or
!                deletion of XPND control file.)
!   pet  970610  Forced to put mod files etc. in spool file only if KFULLOUT
!                option were in effect ($OUTPUT  MINIMUM  NO)
!   pet  990109  Allowed absolute path in mod file names, that means that if
!                the file names start from "/" than no path name will be put
!                before. Otherwise path name which is kept in PRE_SAV_DIR
!                will be added before the name.
!   pet  990407  Improved comments
!   pet  990624  Forced to write down names of both input and output CGM
!   kdb  990623  Lengthen string variable (strng) used to print lines to
!                spoolfile from 132 to 255 characters to accomodate the site
!                weight file.
!   pet  1999.10.15  Added the 13-th output file: eccentricity file
!   pet  2000.09.22  Added the 14-th output file: mean gradient fiel
!   pet  2003.08.20  Straitened up logic
!   pet  2004.05.24  Fixed a bug: in the past the mode files were appended &
!                    when both "MOD_FILES YES" and "MINUMUM NO" were specified &
!                    in the $OUTOUT section of the control file. In the new &
!                    version files are appended if "MOD_FILES YES" regardless &
!                    whether "MINUMUM NO" is specified.
!   pet  2011.10.02  Lengthen string variable (strng) used to print lines to
!                    spoolfile from 255 characters to 8192 characters.
!
! 5.  APND_2_SPOOL PROGRAM STRUCTURE
!
!CCC
!
! --- Append the progress and control files to the end of the spool file.
!
      CALL USE_GLBFIL('OR' )
      CALL USE_COMMON('OR' )
      CALL USE_SPOOL ('O' )
!
! --- Deal with the mod files, etc., first
!
      IF ( MODOUTFLG ) THEN
           OUTFILE(1)  = ESMMAP
           OUTFILE(2)  = PWCMAP
           OUTFILE(3)  = HFEOPCAL
           OUTFILE(4)  = PLODCALF
           OUTFILE(5)  = STAMAP
           OUTFILE(6)  = VELMAP
           OUTFILE(7)  = SRCMAP
           OUTFILE(8)  = EOPMAP
           OUTFILE(9)  = AXOMAP
           OUTFILE(10) = NTSMAP
           OUTFILE(11) = NTMMAP
           OUTFILE(12) = ECCMAP
           OUTFILE(13) = MGRMAP
           OUTFILE(14) = WEIGHT_FILE(1)
           OUTFILE(15) = WEIGHT_FILE(2)
           OUTFILE(16) = WEIGHT_FILE(3)
           OUTFILE(17) = WEIGHT_FILE(4)
           DO I=1,M_OUT
              IF ( OUTFILE(I) .NE. 'NONE' .AND. OUTFILE(I) .NE. ' ' )  THEN
                   IF ( OUTFILE(I)(1:1) .EQ. '/' ) THEN
                        FNAME = OUTFILE(I)
                      ELSE
                        FNAME = PRE_SAV_DIR(1:PRE_SV_LEN)//OUTFILE(I)
                   END IF
!
                   IF ( I.EQ.12 ) FNAME = OUTFILE(I)
                   WRITE ( 23, '("1",2X,A,1X,A/)' ) HEADER(I), FNAME
                   OPEN ( 28, FILE=FNAME, IOSTAT=IOS )
                   ERRSTR = 'BATCH(apnd_2_spool) Opening '//HEADER(I)//' '// &
     &                       FNAME
                   CALL FERR ( INT2(IOS), ERRSTR, INT2(0), INT2(0) )
!
                   READ ( 28, '(A)', IOSTAT=IOS, END=213 ) STRNG
                   ERRSTR = 'BATCH(apnd_2_spool) Reading '//HEADER(I)//' '// &
     &                       FNAME
                   CALL FERR ( INT2(IOS), ERRSTR, INT2(0), INT2(0) )
!
                   DO WHILE (.TRUE.)
                      LEN = TRIMLEN ( STRNG )
                      IF ( LEN .LE. 0 ) THEN
                           LEN   = 1
                           STRNG = ' '
                      ENDIF
                      WRITE ( 23, '(A)'                      ) STRNG(1:LEN)
                      READ  ( 28, '(A)', IOSTAT=IOS, END=213 ) STRNG
                      ERRSTR = 'BATCH(apnd_2_spool): Reading '// &
     &                          HEADER(I)//' '//FNAME
                      CALL FERR ( INT2(IOS), ERRSTR, INT2(0), INT2(0) )
                   ENDDO
              END IF ! outfile
!
  213         CONTINUE
              WRITE ( 23, '(1X)' )
              CLOSE ( 28, IOSTAT=IOS )
              ERRSTR = 'BATCH(apnd_2_spool): Closing '//HEADER(I)//' '// &
     &                  FNAME
              CALL FERR ( INT2(IOS), ERRSTR, INT2(0), INT2(0) )
           ENDDO ! i
      ENDIF
!
! --- Now write out the versions of all the programs used in this run
!
      WRITE ( 23, '("PROGRAM VERSIONS:")' )
      WRITE ( 23, '("*****************")' )
      CALL USE_PVERS ( 'ORC' )
      DO I=1,NUM_PROGS
         IF ( PROG_DATES(I)(1:1) .NE. ' ' ) THEN
              WRITE ( 23, '(A," Ver. ",A," ",A)' ) PROG_NAMES(I), PROG_DATES(I), &
     &                                             PROG_COMMENTS(I)
         ENDIF
      ENDDO
!
      WRITE ( 23, '(1X)' )
      IF ( KTESTV ) THEN
           WRITE ( 23, '("The following special versions were used:")' )
           OPEN ( 10, FILE=PRE_SAV_DIR(:PRE_SV_LEN)//'SOLMOD', IOSTAT=IOS )
           CALL FERR ( INT2(IOS), "BATCH(apnd_2_spool) Opening SOLMOD file", &
     &                 INT2(0), INT2(0) )
99         CONTINUE
           READ ( 10, '(A80)', END=102, IOSTAT=IOS ) CIB
           CALL FERR ( INT2(IOS), "BATCH(apnd_2_spool) Reading SOLMOD file", &
     &                 INT2(0), INT2(0) )
           IF ( CIB(4:5) .EQ. PRE_LETRS  .AND.  CIB(1:1) .NE. '*' ) THEN
                WRITE ( 23, '(A)' ) CIB
           ENDIF
           GOTO 99
!
 102      CONTINUE
          CLOSE ( 10 )
          WRITE ( 23, '(1X)' )
      ENDIF
!
! --- Deal with the control file first
!
      WRITE ( 23, '("1",2X,"Control file ",A/)' ) CFNAME
      CALL CFSPOS ( 1 )
      LEN = CFREADALL ( STRNG )
!
! --- Check for a sporadic problem in which the expanded control file
! --- is suspected to disappear.
! --- First check for the actual existence of the file on disk
!
      INQUIRE ( FILE=CFNAME, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
!
! -------- Control file seems missing.
!
           IWARNING = -3
           BUF_WARNING = "APND_2_SPOOL: XPND control file MISSING "// &
     &                   "before copy into spoolfile"
      ENDIF
!
! --- Then check for the emptying of the file to which the XPND control file
! --- lu is open.
!
      IF ( LEN .EQ. -1 ) THEN
           IWARNING = -4
           BUF_WARNING = "XPND control file EOF! before copy into spoolfile"
        ELSE
!
! -------- Finally do control file copy (if the file exists, of course!)
!
           DO WHILE ( LEN .NE. -1 )
              IF ( LEN .LE. 0 ) THEN
                   LEN=1
                   STRNG=' '
              ENDIF
!
              WRITE ( 23, '(A)' ) STRNG(1:LEN)
              LEN = CFREADALL(STRNG)
           ENDDO
      END IF
!
! --- Deal with the progress file
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PRGF'//PRE_LETRS
      WRITE ( 23, '("1",2X,"Progress file ",A/)' ) FNAME
!
      OPEN ( 28, FILE=FNAME, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), 'Opening progress file APND_2_SPOOL', INT2(0), &
     &            INT2(0) )
      READ ( 28, '(A)', IOSTAT=IOS, END=212 ) STRNG
      CALL FERR ( INT2(IOS), 'Reading progress file 1 APND_2_SPOOL', INT2(0), &
     &            INT2(0) )
      DO WHILE ( .TRUE. )
         LEN = TRIMLEN(STRNG)
         IF ( LEN .LE. 0 ) THEN
              LEN   = 1
              STRNG = ' '
         ENDIF
!
         WRITE ( 23, '(A)' ) STRNG(1:LEN)
         READ  ( 28, '(A)', IOSTAT=IOS, END=212 ) STRNG
         CALL FERR ( INT2(IOS), 'Reading progress file 2 APND_2_SPOOL', &
     &               INT2(0), INT2(0) )
      ENDDO
212   CONTINUE
      CLOSE ( 28, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), 'Closing progress file APND_2_SPOOL', INT2(0), &
     &            INT2(0) )
!
! --- Write name of input or output CGM file
!
      WRITE ( 23, '("1",2X,"CGM Summary")' )
      WRITE ( 23, '(A,A)' ) '  Input  CGM: ',INAMCG
      WRITE ( 23, '(A,A)' ) '  Output CGM: ',ONAMCG
!
! --- Close everything up and split
!
      CALL USE_SPOOL  ( 'C' )
      CALL USE_COMMON ( 'C' )
      CALL USE_GLBFIL ( 'C' )
!
      RETURN
      END  !#!  APND_2_SPOOL  #!#
