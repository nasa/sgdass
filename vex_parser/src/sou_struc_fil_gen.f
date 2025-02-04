      SUBROUTINE SOU_STRUC_FIL_GEN ( FIL_IMG_LST, FIL_GLO_SOU, SOU_IVS, &
     &                N_SOU, DATE_OBS, BND_IDS, FIL_CNF, IMOD, IUER )
!
! ******************************************************************************
! *                                                                            *
! *  Routine SOU_STRUC_FIL_GEN generates a structure control file that will    *
! *  be used to compute the sources structure contribution to phase and group  *
! *  delay.                                                                    *
! *  N.B: - It is assumed for all sources, the delay is computed from the      *
! *         source in the form of delta function, i.e. Usage = DEL_COMP.       *
! *       - The routine either returns the last image observed or the image    *
! *         closest to the observation time.                                   *
! *       - The Band ID's are to be arranged from high frequency to low, and   *
! *         only takes, 'XC', and 'XS'                                         *
! *                                                                            *
! *  INPUT:                                                                    *
! *        FIL_IMG_LST  =  Image list file                 { CHAR }            *
! *                        File with paths to all source images.               *
! *                                                                            *
! *        FIL_GLO_SOU  =  Source Position File            { CHAR }            *
! *                        This file contains positions and uncertainties of   *
! *                        over 28k objects, given their RFC ID and IVS name.  *
! *                        N.B: - use either glo.src or source.names           *
! *                                                                            *
! *        SOU_RFC      =  Array of source RFC ID's        { CHAR }            *
! *                                                                            *
! *        N_SOU        =  No. of sources                  { INT }             *
! *                                                                            *
! *        DATE_OBS     =  Observation date                { CHAR }            *
! *                        YYYY:MM:DD                                          *
! *                                                                            *
! *        BND_IDS     =  2 Frequency Bands               { CHAR }            *
! *                                                                            *
! *        IMOD         =  Routine mode                    { INT, OPT }        *
! *                        This defines the type of output the user wishes to  *
! *                        derive from the routine.                            *
! *                        Default, IMOD = 1                                   *
! *                        = 1 -- The last image observed is returned.         *
! *                        = 2 -- The image observed closest to observation is *
! *                               returned.                                    *
! *                                                                            *
! *        IUER         =  Error Handler                   { INT, OPT }        *
! *                        If IUER=0 no error message will be printed, even    *
! *                        in the event of an error. However, for  other       *
! *                        possible values, i.e. IUER=-1,-2, & -3, the error   *
! *                        message will print to screen. For the latter case,  *
! *                        i.e. IUER=-3, after printing the program will       *
! *                        terminate.                                          *
! *                        Default, IUER = -1                                  *
! *                                                                            *
! *  OUTPUT:                                                                   *
! *            FIL_CNF  =  Structure Control File          { ASCII FILE }      *
! *                                                                            *
! *  ### 17-NOV-2020  SOU_STRUC_FIL_GEN  v1.0 (c)  N. Habana  17-NOV-2020 ###  *
! *  ### 18-NOV-2020  SOU_STRUC_FIL_GEN  v1.1 (c)  N. Habana  18-NOV-2020 ###  *
! *    -  Included the printing of 2 Bands to the intake.                      *
! *                                                                            *
! ******************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   N_SOU, IMOD, IUER, IER
      CHARACTER   FIL_IMG_LST*(*), FIL_GLO_SOU*(*) 
      CHARACTER   SOU_RFC(N_SOU)*16, SOU_IVS(N_SOU)*16
      CHARACTER   FIL_CNF*128, BND_IDS*2
      CHARACTER   DELIM*5
      INTEGER*4   MP, MIND
      INTEGER*4   MAXL_STRING
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'_'//'/' ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)
      INTEGER*4   NP, LIND, IND(2,MIND)
      INTEGER*4   MJD_OBS, IMG_MJD, IMG_TAI
      CHARACTER   DATE_OBS*10, BND*1
      CHARACTER   USAGE*8, DATE_BEG*19, DATE_END*19
      REAL*8      PXL_CNTR_X, PXL_CNTR_Y
      CHARACTER   IMG_DATE*10, IMG_DATE_2*10, IMG_BND*1, IMG_SOU*16 
      INTEGER*4   J0, J1, J2, J3, J4, J5
      LOGICAL*1   FL_SOU
      INTEGER*4,  EXTERNAL :: ILEN, ADD_CLIST, LTM_DIF
      CHARACTER,  EXTERNAL :: MJDSEC_TO_DATE*30
!
! --- Check the Mode
!
      IF ( (IMOD .NE. 1) .OR. (IMOD .NE. 2) ) THEN 
         IMOD = 1
      END IF
!
! --- Read image list file to BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_IMG_LST, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1930, IUER, 'SOU_STRUC_FIL_GEN',              &
     &            'Error in reading input file: '//FIL_IMG_LST )
           RETURN
      END IF
!
! --- Check if the input Band is correct.
!
      IF ( (BND_IDS .EQ. 'XS') .OR. (BND_IDS .EQ. 'SX') ) THEN
         BND_IDS = 'XS'        ! Ensure the naming order is correct
      ELSEIF ( (BND_IDS .EQ. 'XC') .OR. (BND_IDS .EQ. 'CX') ) THEN
         BND_IDS = 'XC'
      ELSE
           CALL ERR_LOG ( 1932, IUER, 'SOU_STRUC_FIL_GEN',              &
     &            'Input Bands should be XS or XC, not '//BND_IDS )
           RETURN
      END IF
!   
! --- Convert the IVS names to J2000 names.
!
      CALL SOUIVS_TO_SOURFC (FIL_GLO_SOU, N_SOU, SOU_IVS, SOU_RFC, IUER)
!
! --- Predefine some values print 
!
      USAGE    = 'DEL_COMP'
      DATE_BEG = '1970.01.01_00:00:00'
      DATE_END = '2020.01.01_00:00:00'
      PXL_CNTR_X = 0.D0
      PXL_CNTR_Y = 0.D0
!
! --- Open output file
!
      OPEN ( UNIT = 11, FILE = TRIM(FIL_CNF), STATUS = 'UNKNOWN' )
!
! --- Write the Header.
!
      WRITE ( 11, 101 )
      WRITE ( 11, 102 )
!
! --- Go through the source list
!
      DO 310 J0 = 1, N_SOU
!
! ----- Go through the Bands
!
        DO 510 J2 = 1, 2   
!
! ------ Flag to check if we have found the source in the file
!
         FL_SOU = .FALSE.
!
! ------ Define the Band to view
!
         BND = BND_IDS(J2:J2)
!         
! ------ Check image file for matching directory
!
         DO 410 J1 = 1, NP
!
            IF ( ILEN(BUF(J1)) == 0 ) GOTO 410 ! Skip Empty Lines
!
! --------- Get the latest available image in the band we want.
!
            IF ( IMOD == 1 ) THEN
!
! ------------ Extract words from the read line
!
               CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------------ Label these extracted words
!
               IMG_SOU  = BUF(J1)(IND(1,2):IND(2,2))
               IMG_BND = BUF(J1)(IND(1,4):IND(2,4))
!
! ------------ Is this the source we are looking for
!
               IF ( SOU_RFC(J0) .EQ. IMG_SOU ) THEN
!
! --------------- Is this the Band we want
!
                  IF ( BND .EQ. IMG_BND) THEN
!
! ------------------ Is this the last image
! ------------------ N.B: - It's the last image if the next image 
!                           doesn't match source name and band 
!                           criteria. 
!                         - If this is the last line then criteria have 
!                           been met.
!
                     IF ( J1 .EQ. NP ) THEN
                        FL_SOU = .TRUE. 
                        WRITE ( 11, 103 ) SOU_IVS(J0), BND, USAGE,     &
     &                      DATE_BEG, DATE_END, PXL_CNTR_X,             &
     &                      PXL_CNTR_Y, TRIM(BUF(J1))
                     ELSE
!
! --------------------- If next source doesn't match, then we print out
!                       current line..
!
                        IF ( IMG_SOU .NE. BUF(J1+1)(8:17) ) THEN
                           FL_SOU = .TRUE. 
                           WRITE ( 11, 103 ) SOU_IVS(J0), BND, USAGE,  &
     &                         DATE_BEG, DATE_END, PXL_CNTR_X,          &
     &                         PXL_CNTR_Y, TRIM(BUF(J1))
!
! --------------------- If next source matches but bands don't match,
!                       then we print out to configuration file
!
                        ELSEIF ( (IMG_SOU .EQ. BUF(J1+1)(8:17)) .AND.   &
     &                          (IMG_BND .NE. BUF(J1+1)(30:30)) ) THEN
                           FL_SOU = .TRUE. 
                           WRITE ( 11, 103 ) SOU_IVS(J0), BND, USAGE,  &
     &                         DATE_BEG, DATE_END, PXL_CNTR_X,          &
     &                         PXL_CNTR_Y, TRIM(BUF(J1))
                        END IF
                     END IF
                  END IF     
               END IF
!
! --------- Get image closest to date we want.
!
            ELSE 
!
! ------------ Extract words from the read line
!
               CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------------ Label these extracted words
!
               IMG_SOU  = BUF(J1)(IND(1,2):IND(2,2))
               IMG_BND = BUF(J1)(IND(1,4):IND(2,4))
               IMG_DATE = BUF(J1)(IND(1,5):IND(2,5))//':'//             &
     &                    BUF(J1)(IND(1,6):IND(2,6))//':'//             &
     &                    BUF(J1)(IND(1,7):IND(2,7))
!
! ------------ Hold the next date for reference, as long as you are not
!              on the last line.
!
               IF ( J1 .NE. NP ) THEN
                  IMG_DATE_2 = BUF(J1+1)(32:35)//':'//                  &
     &                         BUF(J1+1)(37:38)//':'//                  &
     &                         BUF(J1+1)(40:41)
               END IF
!
! ------------ Is this the source we are looking for?
!
               IF ( SOU_RFC(J0) .EQ. IMG_SOU ) THEN
!
! --------------- Do the Bands Match up?
!
                  IF ( BND .EQ. IMG_BND ) THEN
!
! ------------------ Find the closest date before passing the IMG DATE
!
                     IF ( DATE_OBS .LT. IMG_DATE) THEN
!
! --------------------- If this is the last line, then print to file
!
                        IF ( J1 .EQ. NP ) THEN
                           FL_SOU = .TRUE. 
                           WRITE ( 11, 103 ) SOU_IVS(J0), BND, USAGE,  &
     &                         DATE_BEG, DATE_END, PXL_CNTR_X,          &
     &                         PXL_CNTR_Y, TRIM(BUF(J1))
                        ELSE
!
! ------------------------ If the next source is not what we are 
!                          looking at print the current info.
!
                           IF ( IMG_SOU .NE. BUF(J1+1)(8:17) ) THEN
                              FL_SOU = .TRUE. 
                              WRITE ( 11, 103 ) SOU_IVS(J0), BND,      &
     &                           USAGE, DATE_BEG, DATE_END, PXL_CNTR_X, &
     &                           PXL_CNTR_Y, TRIM(BUF(J1))
!
! ------------------------ If next source matches but bands don't match,
!                          then we print out to configuration file
!
                           ELSEIF ((IMG_SOU .EQ. BUF(J1+1)(8:17)) .AND. &
     &                            (IMG_BND .NE. BUF(J1+1)(30:30))) THEN
                              FL_SOU = .TRUE. 
                              WRITE ( 11, 103 ) SOU_IVS(J0), BND,      &
     &                           USAGE, DATE_BEG, DATE_END, PXL_CNTR_X, &
     &                           PXL_CNTR_Y, TRIM(BUF(J1))
!
! ------------------------ If next source matches, bands match, but the 
!                          date is greater. Then print to file.
!

                           ELSEIF((IMG_SOU .EQ. BUF(J1+1)(8:17))  .AND. &
     &                            (IMG_BND .EQ. BUF(J1+1)(30:30)).AND. &
     &                            (DATE_OBS .GT. IMG_DATE_2) ) THEN
                               FL_SOU = .TRUE. 
                              WRITE ( 11, 103 ) SOU_IVS(J0), BND,      &
     &                           USAGE, DATE_BEG, DATE_END, PXL_CNTR_X, &
     &                           PXL_CNTR_Y, TRIM(BUF(J1))
                           END IF
                        END IF
                     END IF
                  END IF
               END IF 
            END IF
 410     CONTINUE
!
! ------ The source was not found.
!
         IF ( .NOT. FL_SOU ) THEN
!%%NOKH%%!           CALL ERR_LOG ( 1931, -1, 'SOU_STRUC_FIL_GEN',                &
!%%NOKH%%!     &             'WARNING: No image was found for '//SOU_IVS(J0)//    &
!%%NOKH%%!     &             ' in '//BND//' Band' )
            WRITE (6,*) 'WARNING: No image was found for '//             &
     &         TRIM(SOU_IVS(J0))//' in '//BND//' Band'

         END IF
 510   CONTINUE
 310  CONTINUE

!     
! --- Write footer
!
      WRITE ( 11, 101 )
! --- 
 101  FORMAT ( 'STRUC_CONTROL  Format version of 2007.03.18' )
!
 102  FORMAT (                                                          &
     &   '# ',                                                        / &
     &   '#  Control file for computation of source structure '         &
     &   'contribution to',                                           / &
     &   '#  phase and group delay.',                                 / &
     &   '# ',                                                        / &
     &   '#  ONLY Geodetic sources of 2007.04.26',                    / &
     &   '# ',                                                        / &
     &   '# ',                                                        / &
     &   '#  The file consists of one type of record:',               / &
     &   '#  1) Source  definition',                                  / &
     &   '# ',                                                        / &
     &   '#  Format of source definition records',                    / &
     &   '# ',                                                        / &
     &   '#   1:3   A3    Record definition. Must be SOU',            / &
     &   '#   6:15  A10   Source name',                               / &
     &   '#  18:18  A1    Band identifier',                           / &
     &   '#  21:28  A8    Usage. Supported keywords: ',               / &
     &   '#               MAP_FITS -- delay is used computed by '       &
     &   'Fourier transform of the map ',                             / &
     &   '#               DEL_COMP -- delay is computed from the '      &
     &   'source model in ',                                          / &
     &   '#                           the form of delta function '      &
     &   '(Clean components)',                                        / &
     &   '#               GAU_COMP -- delay is computed from the '      &
     &   'source model in ',                                          / &
     &   '#                           the form of Gaussian components'/ &
     &   '#               NONE     -- no source mape is used',        / &
     &   '#  31:49  A19   Begin date of the map validity',            / &
     &   '#  51:69  A19   End date of the map validity'               / &
     &   '#  72:79  F8.2  X pixel coordinate of the reference point '   &
     &   'with respect to the phase center',                          / &
     &   '#  81:88  F8.2  Y pixel coordinate of the reference point '   &
     &   'with respect to the phase center',                          / &
     &   '#  91:    A     File with the source map in fits format. '    &
     &   'The file name may ',                                        / &
     &   '#               contain macro strings with defied in the '    &
     &   'macro definition section',                                  / &
     &   '#               with leading dollar sign, using the same '    &
     &   'syntax as shell ',                                          / &
     &   '#               environment variables.',                    / &
     &   '# ',                                                        / &
     &   '# ',                                                        / &
     &   '# ',                                                        / &
     &   '#  Who        When        What',                            / &
     &   '#  L. Petrov  2020.11.18  Created',                         / &
     &   '# ',                                                        / &
     &   '# ',                                                        / &
     &   '#    Source      B  Usage     Date_Begin           '          &
     &   'Date_End               Pixel_Center     File_name'          / &
     &   '# ' )
!
 103  FORMAT ( 'SOU','  ', A10,'  ', A1,'  ', A8,'  ', A19,' ',         &
     &   A19,'  ',F8.2,'  ',F8.2,'  ', A )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE !#!
