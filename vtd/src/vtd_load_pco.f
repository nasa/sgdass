      SUBROUTINE VTD_LOAD_PCO ( VTD, ANT_CLASS, DIR_BTX, L_PCO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_LOAD_PCO  checks all files in the directory DIR_BTX.   *
! *   It identifies files with antenna phase offsets in binary format    *
! *   BINTEX, of a given class and match them against either station or  *
! *   near zone objects. For all matches it reads files with antenna     *
! *   phase offsets and loads them in phase center offset objects.       *
! *                                                                      *
! *   Routine works in two modes.                                        *
! *                                                                      *
! *     When ANT_CLASS .EQ. VTD__SPACE or VTD__GNSS, it searches for     *
! *     files with space-born antennas offsets. This file name has       *
! *     format SSS_FFFFFFFF_UUUUUUUU.btx , where SSS is the satellite    *
! *     name, where                                                      *
! *     FFFFFFFF -- date "valid from"  in YYYYMMDD format,               * 
! *     UUUUUUUU -- date "valid until" in YYYYMMDD format, and           *
! *     .btx is extension.                                               *
! *                                                                      *
! *     For every near-zone object it first checks the three-letter IGS  *
! *     satellite code. If it matches, it checks dates against MJD_BEG.  *
! *     If MJD_BEG, converted to YYYYMMDD format is within "valid from"  *
! *     and "valid until", that file is parsed, new object with phase    *
! *     center offset that holds its contents is created, and the index  *
! *     of that objects is inserted in the near zone object.             *
! *                                                                      *
! *     When ANT_CLASS .EQ. VTD__GROUND, it searches for files with      *
! *     ground-based antennas offsets. This file name has format         *
! *     TTTTTTTT.btx, where TTTTTTT is the IGS registered GNSS antenna   *
! *     type. Since IGS GNSS antenna types are allowed to have character *
! *     / inside, that character is replaced with character @ in the     *
! *     file name.                                                       *
! *                                                                      *
! *     For every site it check its antenna type against the every file  *
! *     in DIR_BTX directory. If the file name matches, it reads that    *
! *     file, creates a new new object with phase center offset that     *
! *     holds its contents, and inserts the index of that object.        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * ANT_CLASS ( CHARACTER ) -- Antenna class. Supported classes:         *
! *                            VTD__SPACE for space-born atennas and     *
! *                            VTD__GROUND for ground antennas, mainly   *
! *                            designed for GNSS observations.           *
! * DIR_BTX   ( CHARACTER ) -- Directory with phase offset files in      *
! *                            BINTEX format.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * L_PCO     ( INTEGER*4 ) -- The number of phase center offset files   *
! *                            that have been read and processed.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 03-SEP-2025   VTD_LOAD_PCO   v1.1 (d) L. Petrov  18-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  DIR_BTX*(*), ANT_CLASS*(*)
      INTEGER*4  L_PCO, IUER
      CHARACTER  FILNAM*128, DATE_VALID_FROM*24, &
     &           DATE_VALID_UNTIL*24, C_FIL(VTD__M_PCO)*128, &
     &           DATE_BEG*19, STR*20
      INTEGER*4  LUN, SIZ, IS, LEV, ID, IL, L_FIL, IND_FIL, &
     &           J1, J2, J3, J4, J5, J6, J7, IER
      INTEGER*8  DIR_DESC(16)
      CHARACTER, EXTERNAL :: GET_UTC_CDATE*19, MJDSEC_TO_DATE*30 
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, LTM_DIF, LINDEX
!
      L_PCO = 0
      L_FIL = 0
!
! --- Scan directory with phase center offset files
!
      LEV = 0
      DO 410 J1=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_BTX, FILNAM )
         IF ( LEV == 0 ) GOTO 810
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 2961, IUER, 'VTD_LOAD_PCO', 'Error in '// &
     &            'reading directory '//TRIM(DIR_BTX)// &
     &            ' -- '//FILNAM(1:I_LEN(FILNAM))//' where files '// &
     &            'with antenna phase offset variations in binary '// &
     &            'bintex format' )
              RETURN 
         END IF
         IL=ILEN(FILNAM) 
         IF ( IL < 4 ) GOTO 410
         IF ( FILNAM(IL-3:IL) == '.btx' ) THEN
              L_FIL = L_FIL + 1
              IF ( L_FIL > VTD__M_PCO )  THEN
                   CALL ERR_LOG ( 2962, IUER, 'VTD_LOAD_PCO', 'Too many '// &
     &                 'files with antenna offset varations in binary bintex '// &
     &                 'format were found in directory '//TRIM(DIR_BTX)// &
     &                 ' -- more than '//TRIM(STR)//'. Please increase VTD__M_PCO '// &
     &                 'parameters or remove unnecessry files in '//DIR_BTX )
                   RETURN 
              END IF
              C_FIL(L_FIL) = FILNAM
         END IF 
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 )  THEN
!
! -------- No files were found? Nothing to do.
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Sort the files
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
      IF ( ANT_CLASS == VTD__SPACE .OR. ANT_CLASS == VTD__GNSS_SAT ) THEN
!
! -------- Case of space born antenna
!
! -------- Transform the observation start date to YYYYMMDD format
!
           DATE_BEG = MJDSEC_TO_DATE ( VTD%MJD_BEG, VTD%TAI_BEG, IER )
           DATE_BEG = DATE_BEG(1:4)//DATE_BEG(6:7)//DATE_BEG(9:10)
!
! -------- Cycle over all NZO objects of GNSS type 
!
           DO 420 J2=1,VTD%L_NZO
              IF ( VTD%NZO(J2)%OBJ_TYPE == VTD__GNSS_SAT ) THEN
!
! ---------------- Check the oject name and observation start date 
! ---------------- against file names in BINTEX format
!
                   IND_FIL = 0
                   DO 430 J3=1,L_FIL
                      ID = LINDEX ( C_FIL(J3), '/' ) 
                      IF ( C_FIL(J3)(ID+1:ID+3) == VTD%NZO(J2)%NAME(1:3) ) THEN
!
! ------------------------ The object name matches. Good, keep going
!
                           IF ( DATE_BEG(1:8) .GE. C_FIL(J3)(ID+5:ID+12) .AND. &
     &                          DATE_BEG(1:8) .LE. C_FIL(J3)(ID+14:ID+21)      ) THEN
!
! ----------------------------- Dates "valid from" and "valid until" match
! ----------------------------- as well. Bingo!
!
                                IND_FIL = J3
                           END IF
                      END IF
 430               CONTINUE 
                   IF ( IND_FIL > 0 ) THEN
!
! --------------------- We found that file. Read it. The counter of 
! --------------------- phase center offset objects, VTD%L_PCO, is incremented.
! --------------------- It contents is put into VTD%PCO(VTD%L_PCO)
!
                        CALL ERR_PASS ( IUER, IER )
                        CALL VTD_READ_BINTEX ( C_FIL(IND_FIL), VTD, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 2964, IUER, 'VTD_LOAD_PCO', &
     &                           'Error in reading with antenna offset '// &
     &                           'from a file varations in binary bintex '// &
     &                           'format '//C_FIL(IND_FIL) )
                            RETURN 
                        END IF
!
! --------------------- Set the index of phase center offset object
!
                        VTD%NZO(J2)%IND_PCO = VTD%L_PCO
!
! --------------------- And augment the counter of created phase center offset
! --------------------- objects
!
                        L_PCO = L_PCO + 1
                   END IF
              END IF
 420       CONTINUE 
        ELSE IF ( ANT_CLASS == VTD__GROUND ) THEN
!
! -------- Case of a ground antenna
!
! -------- Cycle over stations
!
           DO 440 J4=1,VTD%L_STA
              IND_FIL = 0
              IL = ILEN(VTD%STA(J4)%ANTEX_TYPE)
              IF ( IL > 0 ) THEN
!
! ---------------- Generate a modified antenna type
!
                   STR = VTD%STA(J4)%ANTEX_TYPE
                   DO 450 J5=1,IL
                      IF ( STR(J5:J5) == '@' ) THEN
                           STR(J5:J5) = '/'
                      END IF
 450               CONTINUE 
!
! ---------------- Check for files with the modified antenna files
!
                   DO 460 J6=1,L_FIL
                      ID = LINDEX ( C_FIL(J6), '/' ) 
                      IF ( C_FIL(J6)(ID+1:ID+IL)//'.btx' == STR(1:IL)//'.btx' ) THEN
!
! ------------------------ Bingo! We found such a file
!
                           IND_FIL = J6
                      END IF
 460               CONTINUE 
              END IF
!
              IF ( IND_FIL > 0 ) THEN
!
! ---------------- We found that file. Read it. The counter of 
! ---------------- phase center offset objects, VTD%L_PCO, is incremented.
! ---------------- It contents is put into VTD%PCO(VTD%L_PCO)
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_READ_BINTEX ( C_FIL(IND_FIL), VTD, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2964, IUER, 'VTD_LOAD_PCO', &
     &                      'Error in reading with antenna offset '// &
     &                      'from a file varations in binary bintex '// &
     &                      'format '//C_FIL(IND_FIL) )
                       RETURN 
                   END IF
!
! ---------------- Set the index of phase center offset object
!
                   VTD%STA(J4)%IND_PCO = VTD%L_PCO
!
! ---------------- And augment the counter of created phase center offset
! ---------------- objects
!
                   L_PCO = L_PCO + 1
              END IF
 440       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_PCO  !#!#
