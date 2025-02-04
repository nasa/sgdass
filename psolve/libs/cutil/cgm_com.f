      SUBROUTINE CGM_COM ( IDIRECT, FNAME, FILDES, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CGM_COM PROGRAM SPECIFICATION
!
! 1.1 Low level read and write of CGM common blocks
!
! 1.2 REFERENCES:
!
! 2.  CGM_COM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDIRECT(*)
      INTEGER*4 FILDES
      CHARACTER*(*) FNAME,STRING
!
! FILDES  - File descriptor of file to be read or written
! FNAME   - Name of file to be read or written
! IDIRECT - Contains information about size of chunks to read or write
! STRING  - Type of access requested ('R'=read; 'W'=write)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_23.i'
      INCLUDE 'socom_28.i'
      INCLUDE 'osocom.i'
      INCLUDE 'oosocom.i'
      INCLUDE 'ooprfil.i'
      INCLUDE 'prfil.i'
      INCLUDE 'oprfil.i'
      INCLUDE 'prfil_255.i'
      INCLUDE 'plist.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 OLDCOM, FL_PREOCT2002
      CHARACTER ME*7
      INTEGER*4 J1, IOS, I4
      INTEGER*2 IY, ID, IM, I, J
      INTEGER*4  NPARM
      INTEGER*4 JL,JBLOCKS, JCT
      DATA      ME / 'CGM_COM' /
      INTEGER*2  INT2_ARG
!
! I,J - Loop indices
! JBLOCKS - Number of blocks to be read or written
! JL - File position at which to read or write
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  911220  Fix up monument names from CGMs: AUSTINTX = 7271
!                NOTOX, WIDE85_3, VLBA85_3 = <blank>
!   KDB  951024  Update for 32 site update (affected by socom changes and
!                change in jparfil_words, which just jumped to integer*4).
!                Also fix error (failure to set pwcsizep when reading
!                   cgms with 239 or 255 block parfil formats):
!                   increase ipar2 array by one word to account for the word
!                   skipped to place vsited on a real*8 boundary.
!   kdb  951207  Integer*4 number of observations.
!   pet  971202  Added setting CGM_TYPE variable in socom as true
!   pet  980703  Added more verbosity in error messages
!   pet  990106  Added support of variable TRAIN from GLBC4. Replaced old
!                archaic call PARMSG by new GET_NAMES
!   pet  2002.09.26  Added support of  atransforation of 570-blocks long
!                    parfile to the 1074 blocks long parfil
!   pet  2002.10.08  Added code which fixes parameter list of the CGM's created
!                    before October 2002
!   :2002.12.19:jwr: TRUE__L2 and FALSE__L2 introduced for -i2 removal
!   pet  2017.11.11  Transformed to INTEGER*4  and support of large files > 4Gb
!
! 5.  CGM_COM PROGRAM STRUCTURE
!
1     CONTINUE
      OLDCOM = .FALSE.
!
! --- Check for illegal string
!
      IF ( LEN(STRING) .GT. 1 ) THEN
           CALL FILE_REPORT ( FNAME, ME, 'Too many characters in control '// &
     &         'string: '//STRING )
           STOP '(cgm_com) Abnormal termination'
        ELSE IF ( STRING .NE. 'R'  .AND.  STRING .NE. 'W' ) THEN
           CALL FILE_REPORT(FNAME,ME,'ILLEGAL OP' )
           STOP '(cgm_com) Abnormal termination'
      ENDIF
!
! --- SOCOM i/o
!
      JL=IDIRECT(1)
      JBLOCKS=IDIRECT(2)-IDIRECT(1)
10    CONTINUE
      IF ( JBLOCKS .LT. JSOCOM_BLOCKS   .AND.  STRING .EQ. 'W' ) THEN
           WRITE ( 6, * ) ' IDIRECT(1) = ',IDIRECT(1)
           WRITE ( 6, * ) ' IDIRECT(2) = ',IDIRECT(2)
           WRITE ( 6, * ) ' IDIRECT(3) = ',IDIRECT(3)
           WRITE ( 6, * ) ' IDIRECT(4) = ',IDIRECT(4)
           WRITE ( 6, * ) ' IDIRECT(5) = ',IDIRECT(5)
           WRITE ( 6, * ) ' IDIRECT(6) = ',IDIRECT(6)
           WRITE ( 6, * ) ' JBLOCKS = ',JBLOCKS
           WRITE ( 6, * ) ' JSOCOM_BLOCKS = ',JSOCOM_BLOCKS
!@           WRITE ( 6, * ) ' IDIRECT(9) = ',IDIRECT(IDIRECT(1))
           CALL FILE_REPORT ( FNAME, ME, 'SOCOM size mismatch' )
           STOP '(cgm_com) Abnormal termination'
      ENDIF
!
! --- Read in socom, converting older versions to the current version.
! --- There are five versions to worry about:
!         oisocom   in osocom.i   (19 blocks)
!         ooisocom  in oosocom.i  (22 blocks)
!         isocom_23 in socom_23.i (23 blocks)
!         isocom_28 in socom_28.i (28 blocks)
!         isocom    in socom_33.i (33 blocks)
!         socom_33  in socom.i    (46 blocks) (current)
!
      FL_PREOCT2002 = .TRUE.
      IF ( JBLOCKS .LT. JSOCOM_BLOCKS  .AND. STRING .EQ. 'R' ) THEN
           OLDCOM = .TRUE.
           DO I4=1,JSOCOM_WORDS
              ISOCOM(I4) = 0
           ENDDO
           IF ( JBLOCKS .LT. 22 ) THEN
                CALL USE_FILE ( FNAME, FILDES, OISOCOM,   JBLOCKS, JL, STRING )
              ELSE IF ( JBLOCKS .LT. 23 ) THEN
                CALL USE_FILE ( FNAME, FILDES, OOISOCOM,  JBLOCKS, JL, STRING )
              ELSE IF ( JBLOCKS .LT. 28 ) THEN
                OLDCOM = .FALSE.
                CALL USE_FILE ( FNAME, FILDES, ISOCOM_23, JBLOCKS, JL, STRING )
              ELSE IF ( JBLOCKS .LT. 33 ) THEN
                OLDCOM = .FALSE.
                CALL USE_FILE ( FNAME, FILDES, ISOCOM_28, JBLOCKS, JL, STRING )
              ELSE IF ( JBLOCKS .LT. 46 ) THEN
                OLDCOM = .FALSE.
                CALL USE_FILE ( FNAME, FILDES, ISOCOM,    JBLOCKS, JL, STRING )
           ENDIF
!
! -------- If socom read is the oldest socom (< 22 blocks),
! -------- convert it to 22 block socom format
!
           if ( jblocks .lt. 22 ) then
                do i=1,1665
                   ooiso1(i) = oiso1(i)
                enddo
                do i=1,66
                   ooiso2(i) = oiso2(i)
                enddo
                do i=1,8
                   oolaxof(i) = olaxof(i)
                   do j=1,3
                      ooltide(i,j) = oltide(i,j)
                      oolsitec(i,j) = olsitec(i,j)
                     oolsitev(i,j) = olsitev(i,j)
                   enddo
                enddo
!
                do i=1,3
                   do j=1,3
                      oolrot(i,j) = olrot(i,j)
                   enddo
                enddo
!
                do i=1,10
                   do j=1,3
                      oolatm(i,j) = olatm(i,j)
                   enddo
                enddo
!
                do i=1,100
                   oolclk(i)=olclk(i)
                   ooiclsta(i) = oiclsta(i)
                enddo
!
                do i=1,48
                   do j=1,2
                      oolstar(i,j) = olstar(i,j)
                   enddo
                enddo
!
                do i=1,14
                   oofleps(i) = ofleps(i)
                   ooflpsi(i) = oflpsi(i)
                enddo
!
                do i=1,7
                   ooidpnut(i) = oidpnut(i)
                enddo
!
                oolrel = olrel
                ooiuen = oiuen
                oonfleps = onfleps
                oonflpsi = onflpsi
                oondpnut = ondpnut
!
                CALL COMMON_33_TO_46()
           endif
!
! -------- convert 22 block socom format to 23 block format
!
           if ( jblocks .lt. 23 ) then
                do i=1,1316
                   isocom_23(i) = ooisocom(i)
                enddo
                do i=1,923
                   iso3_23(i) = ooiso3(i)
                enddo
                do i=1,6
                   ut1ptb_23(i) = oout1ptb(i)
                   wobxxb_23(i) = oowobxxb(i)
                   wobyyb_23(i) = oowobyyb(i)
                enddo
!
                fcnper_23 = oofcnper
                ieopl_23 = ooieopl
                numstax_23 = oonumstax
                sol_avail_23 = oosol_avail
                interpolation_ut1_23 = oointerpolation_ut1
                interpolation_pm_23 = oointerpolation_pm
                bground_23 = oobground
                totsta_23 = oototsta
                old_clocks_23 = ooold_clocks
                old_atms_23 = ooold_atms
                do i=1,2
                   seocnst_23(i) = ooseocnst(i)
                enddo
                do i=1,16
                   sacnst_23(i) = oosacnst(i)
                   sccnst_23(i) = oosccnst(i)
                   elvcut_23(i) = ooelvcut(i)
                enddo
!
                do i=1,3
                   eopcons_23(i) = ooeopcons(i)
                   eoprcons_23(i) = ooeoprcons(i)
                enddo
!
                do i=1,48
                   do j=1,2
                      lprop_23(i,j) = oolprop(i,j)
                   enddo
                enddo
                CALL COMMON_33_TO_46()
           endif
!
! -------- convert 23 block socom format to 28 block format
!
           if ( jblocks .lt. 28 ) then
                do i=1,404
                   isocom_28(i) = isocom_23(i)
                enddo
                do i=1,735
                   iso4_28(i) = iso4_23(i)
                enddo
                do i=1,669
                   iso5_28(i) = iso5_23(i)
                enddo
                do i=1,517
                   iso6_28(i) = iso6_23(i)
                enddo
                do i=1,160
                   tatm_28(i) = tatm_23(i)
                enddo
                do i=1,10
                   do j=1,3
                      latm_28(i,j) = latm_23(i,j)
                   enddo
                enddo
                CALL COMMON_33_TO_46()
           endif
!
           IF ( JBLOCKS .LT. 33 ) THEN
!
! ------------- Convert from 28 block to 33 block socom format
! ------------- First transfer the things that haven't changed.
! ------------- Transfer large pieces wherever possible.
! ------------- However, some variables that haven't changed are
! ------------- treated specifically anyway because they are isolated
! ------------- between variables that have changed.
!
                do i=1,1908
                   iso1g_28_33(i) = iso1p_28_33(i)
                enddo
                do i=1,23
                   iso2g_28_33(i) = iso2p_28_33(i)
                enddo
                do i=1,9
                   iso3g_28_33(i) = iso3p_28_33(i)
                enddo
                do i=1,541
                   iso4g_28_33(i) = iso4p_28_33(i)
                enddo
                do i=1,136
                   iso5g_28_33(i) = iso5p_28_33(i)
                enddo
                do i=1,496
                   iso6g_28_33(i) = iso6p_28_33(i)
                enddo
                npold = npold_28
                iclmax = iclmax_28
                idbsel = idbsel_28
                ndb = ndb_28
                idcsel = idcsel_28
                constraint_bits = constraint_bits_28
                indl = indl_28
                bm_ref_cl = bm_ref_cl_28
                do i = 1,2
                   eop_style(i) = eop_style_28(i)
                   eopa1_choice(i) = eopa1_choice_28(i)
                   nrot_a1(i) = nrot_a1_28(i)
                   lgrad(i) = lgrad_28(i)
                enddo
!
! ------------ Now transfer things that have changed.
! ------------ The following things were enlarged thanks to an increase in the
! ------------ max_arc_sta parameter.
!
                do i =1,16
                   sacnst(i) = sacnst_28(i)
                   sccnst(i) = sccnst_28(i)
                   elvcut(i) = elvcut_28(i)
                   iclock(1,i) = iclock_28(1,i)
                   iclock(2,i) = 0
                   nsparm(i) = nsparm_28(i)
                   numatm(i) = numatm_28(i)
                   iatstr(i) = iatstr_28(i)
                   numclk(i) = numclk_28(i)
                   iclstr(i) = iclstr_28(i)
                   iblsel_g(1,i) = iblsel_28(1,i)
                   iblsel_g(2,i) = 0
                   wvmask(i) = wvmask_28(i)
                   numgrad(i) = numgrad_28(i)
                enddo
!
                do i =17,32
                   sacnst(i) = 0.0D0
                   sccnst(i) = 0.0D0
                   elvcut(i) = 0.0D0
                   iclock(1,i) = 0
                   iclock(2,i) = 0
                   nsparm(i) = 0
                   numatm(i) = 0
                   iatstr(i) = 0
                   numclk(i) = 0
                   iclstr(i) = 0
                   iblsel_g(1,i) = 0
                   iblsel_g(2,i) = 0
                   wvmask(i) = 0
                   numgrad(i) = 0
                enddo
!
! ------------ Iclsta was enlarged by the addition of a second dimension
!
                do i = 1,100
                   iclsta(1,i) = iclsta_28(i)
                   iclsta(2,i) = 0
                enddo
!
! ------------- NUMOBS and IDBEND have increased to handle integer*4 number of
! ------------- observations
!
                numobs = numobs_28
                do i = 1,15
                   idbend(i) = idbend_28(i)
                enddo
                CALL COMMON_33_TO_46()
                CALL COMMON_46_TO_58()
           END IF ! 28 block
           IF ( JBLOCKS .EQ. 33 ) THEN
                CALL COMMON_33_TO_46()
                CALL COMMON_46_TO_58()
           END IF
         ELSE IF ( JBLOCKS .EQ. 46 ) THEN
                CALL COMMON_46_TO_58()
         ELSE 
!
! ------ Read Normal 58 block long (not arcahic!) SOCOM.
!
         CALL USE_FILE ( FNAME, FILDES, ISOCOM, JBLOCKS, JL, STRING )
      ENDIF
      CGM_TYPE = .TRUE. ! Setting flag that socom corresponds to CGM (not to
!                       ! to single session solution
!
! --- PARFIL i/o
!
      JL=IDIRECT(2)
      IF ( IDIRECT(5) .GT. 0 ) THEN
           JBLOCKS = IDIRECT(5)-IDIRECT(2)
        ELSE
           JBLOCKS = IDIRECT(3)-IDIRECT(2)
      ENDIF
20    CONTINUE
      IF ( JBLOCKS .LT. JPARFIL_BLOCKS  .AND. STRING .EQ. 'W' ) THEN
           WRITE ( 6, * ) ' JBLOCKS = ',JBLOCKS
           WRITE ( 6, * ) ' JPARFIL_BLOCKS  = ',JPARFIL_BLOCKS
           CALL FILE_REPORT ( FNAME, ME, 'Parfil size mismatch' )
           STOP '(cgm_com) Abnormal termination'
      ENDIF
!
! --- Read or write the parfil.
! --- If reading an older parfil format, it must be converted to the current
! --- format. There are four formats to worry about:
!         oiparfil    in oprfil.i     (116 blocks)
!         ooiparfil   in ooprfil.i    (239 blocks)
!         iparfil_255 in prfil_255.i  (255 blocks)
!         iparfil     in prfil.i      (256 blocks) (current)
!
      IF ( JBLOCKS .LT. 239   .AND. STRING .EQ. 'R' ) THEN
!
! -------- Read 116 block parfil
!
           CALL USE_FILE ( FNAME, FILDES, OIPARFIL, JBLOCKS, JL, STRING )
           do jct=1,JPARFIL_WORDS
              iparfil(jct) = 0
           enddo
!
           do i=1,128
              vaxof(i) = ovaxof(i)
              vsited(i) = ovsited(i)
              do j=1,3
                 vsitec(j,i) = ovsitec(j,i)
                 vsitev(j,i) = ovsitev(j,i)
              enddo
              do j=1,4
                 isitn(j,i) = oisitn(j,i)
              enddo
              do j=1,5
                 monuments(j,i) = omonuments(j,i)
              enddo
           enddo
!
           do i=1,max_src
              do j=1,2
                 vstarc(j,i) = ovstarc(j,i)
              enddo
              do j=1,4
                 istrn(j,i) = oistrn(j,i)
              enddo
           enddo
!
           do i=1,3
              vtide(i) = ovtide(i)
           enddo
!
           do i=1,2
              do j= 1,6
                 vnut(i,j) = ovnut(i,j)
                 vnutop(i,j) = ovnutop(i,j)
              enddo
           enddo
!
           do i=1,16
              baro_cal(i) = obaro_cal(i)
              baro_height(i) = obaro_height(i)
           enddo
!
           do i=1,6
              do j=1,max_dbs
                 idbpsl(i,j) = oidbpsl(i,j)
              enddo
           enddo
!
           vatm = ovatm
           vrel = ovrel
           vprec = ovprec
           numsel = onumsel
           CALL PRFIL_256_TO_570 ( )
           CALL PRFIL_570_TO_1074 ( )
           CALL PRFIL_1074_to_1906 ( )
           GLO_FJDOBS_MIN = J2000__JD
           GLO_FJDOBS_MAX = J2000__JD
         ELSE IF ( OLDCOM .AND. STRING .EQ. 'R' ) THEN
!
! -------- Read 239 block parfil
!
           CALL USE_FILE ( FNAME, FILDES, OOIPARFIL, JBLOCKS, JL, STRING )
           DO JCT=1,JPARFIL_WORDS
              IPARFIL(JCT) = 0
           ENDDO
           DO I=1,20600
              IPAR1(I) = OOIPAR1(I)
           ENDDO
           DO I=1,16
              BARO_CAL(I) = OOBARO_CAL(I)
              BARO_HEIGHT(I) = OOBARO_HEIGHT(I)
           ENDDO
           DO I=1,11878
              IPAR2(I) = OOIPAR2(I)
           ENDDO
           CALL PRFIL_256_TO_570 ( )
           CALL PRFIL_570_TO_1074 ( )
           CALL PRFIL_1074_to_1906 ( )
           GLO_FJDOBS_MIN = J2000__JD
           GLO_FJDOBS_MAX = J2000__JD
        ELSE IF ( JBLOCKS .LT. 256  .AND.  STRING .EQ. 'R' ) THEN
!
! -------- Read 255 block parfil
!
           CALL USE_FILE ( FNAME, FILDES, IPARFIL_255, JBLOCKS, JL, STRING )
!
! -------- Zero out the parfil array for the current format
!
           DO JCT=1,JPARFIL_WORDS
              IPARFIL(JCT) = 0
           ENDDO
!
! -------- The only change between the 255 and 256 block parfils is an increase
! -------- in the sizes of baro_cal and baro_height.
! -------- Copy the part of parfil before these variables.
!
           DO I=1,20600
              IPAR1(I) = IPAR1_255(I)
           ENDDO
!
! --------- Copy the meaningful part of these variables
!
            DO I=1,16
               BARO_CAL(I) = BARO_CAL_255(I)
               BARO_HEIGHT(I) = BARO_HEIGHT_255(I)
            ENDDO
!
! --------- Copy the part of parfil after these variables (but don't bother
! --------- with the free (unused) array, izfree.
!
            DO I=1,11878
               IPAR2(I) = IPAR2_255(I)
            ENDDO
            CALL PRFIL_256_TO_570  ( )
            CALL PRFIL_570_TO_1074 ( )
            CALL PRFIL_1074_to_1906 ( )
            GLO_FJDOBS_MIN = J2000__JD
            GLO_FJDOBS_MAX = J2000__JD
         ELSE IF ( JBLOCKS .EQ. 256  .AND.  STRING .EQ. 'R' ) THEN
!
! -------- Read 256 block parfil
!
            CALL USE_FILE ( FNAME, FILDES, IPARFIL, JBLOCKS, JL, STRING )
            CALL PRFIL_256_TO_570  ( )
            CALL PRFIL_570_TO_1074 ( )
            CALL PRFIL_1074_to_1906 ( )
            GLO_FJDOBS_MIN = J2000__JD
            GLO_FJDOBS_MAX = J2000__JD
         ELSE IF ( JBLOCKS .EQ. 570  .AND.  STRING .EQ. 'R' ) THEN
!
! -------- Read 570 block parfil
!
            CALL USE_FILE ( FNAME, FILDES, IPARFIL, JBLOCKS, JL, STRING )
            CALL PRFIL_570_TO_1074 ( )
            GLO_FJDOBS_MIN = J2000__JD
            GLO_FJDOBS_MAX = J2000__JD
         ELSE IF ( JBLOCKS .EQ. 1074  .AND.  STRING .EQ. 'R' ) THEN
            CALL USE_FILE ( FNAME, FILDES, IPARFIL, JBLOCKS, JL, STRING )
            CALL PRFIL_1074_to_1906 ( )
         ELSE
!
! --------- Read or write current parfil (not archaic one)
!
            FL_PREOCT2002 = .FALSE.
            CALL USE_FILE ( FNAME, FILDES, IPARFIL, JBLOCKS, JL, STRING )
      ENDIF
!
! --- Plist I/O
!
      IF ( IDIRECT(5) .GT. 0 ) THEN
           JL = IDIRECT(5)
           JBLOCKS = IDIRECT(3) - IDIRECT(5)
!
           IF ( STRING .EQ. 'W' ) THEN
                IF ( TRAIN ) THEN
!
! ------------------ Create the list of parameters in TRAIN mode.
! ------------------ (parameter list if transfered via plist block directly
! ------------------  in non-TRAIN mode)
!
                     CALL GET_NAMES ( CPARM_NAMES, INT2(20), M_GPA, NPARM, &
     &                    TRUE__L2, TRUE__L2 )
                     PARM_NUM = NPARM
                  ELSE
                     CONTINUE 
                ENDIF
           ENDIF
!
           IF ( JL .EQ. 0  .AND.  STRING .EQ. 'R' ) THEN
                CALL GET_NAMES ( CPARM_NAMES, INT2(20), M_GPA, NPARM, &
     &               TRUE__L2, TRUE__L2 )
                IF ( NPARM .NE. NPARAM ) THEN
                     WRITE ( 6, * ) ' NPARM  =', NPARM
                     WRITE ( 6, * ) ' NPARAM =', NPARAM
                     WRITE ( 6, * ) '     JL =', JL
                   CALL FERR ( INT2(654), '(cgm_com) Parameter list mismatch', &
     &                  INT2(0), INT2(0) )
                     STOP '(cgm_com) Abnormal termination'
                ENDIF
             ELSE
               CALL USE_FILE ( FNAME, FILDES, PARM_NUM_I2, JBLOCKS, JL, STRING )
!%               write ( 6, * ) 'cgm_com(609) parm_num = ', parm_num, ' parm_num_i2= ', parm_num_i2, ' jblocks, jl = ', jblocks, jl    ! %%%%
         ENDIF
      ENDIF
!
! --- Some patches
!
      IF ( NPARAM == 0 .AND. NPARAM_I2 > 0 ) THEN
!
! -------- For compatibility with 32-bit Solve
!
           NPARAM = NPARAM_I2
      END IF
      IF ( PARM_NUM == 0 .AND. PARM_NUM_I2 > 0 ) THEN
!
! -------- For compatibility with 32-bit Solve
!
           PARM_NUM = PARM_NUM_I2
      END IF
      PARM_NUM_I2 = 0
      NPARAM_I2   = 0
!
      DO I=1,NUMSTA
         IF ( ISITN_CHR(I) .EQ. 'AUSTINTX' ) THEN
              LMONUMENTS(I) = '7271      '
         ENDIF
!
         IF ( ISITN_CHR(I) .EQ. 'NOTOX   '  .OR. &
     &        ISITN_CHR(I) .EQ. 'VLBA85 3'  .OR. &
     &        ISITN_CHR(I) .EQ. 'WIDE85 3'        ) THEN
!
              CALL CLRCH ( LMONUMENTS(I) )
         ENDIF
      ENDDO
!
! ???????????????
!
      IF ( FNAME(1:1) .EQ. ']' ) THEN
           DO I=1,2
              DO J=1,6
                 IF ( LLE ( FNAME(2:6), '86N1D') ) VNUTOP(I,J)=0.0D0
                 IF ( LLE ( FNAME(2:6), '87OJ9') ) VNUT(I,J)=0.0D0
              ENDDO
           ENDDO
      ENDIF
!
      IF ( FL_PREOCT2002           .AND. &
     &     STRING   .EQ. 'R'       .AND. &
     &     PARM_NUM .GT.  0        .AND. &
     &     PARM_NUM .LE.  M_GPA          ) THEN
!
! -------- Fixing parameter list. Pre October 2002 version of CGMs had
! -------- nothing in fields 17:20 of the parameters for post-episodic motion
! -------- of site position. Post October 2002 version had "-POS" in the field.
! -------- Besides, blanks are replaced with zeroes in the date field
!
           DO 410 J1=1,PARM_NUM
              IF ( ( CPARM_NAMES(J1)(9:10) .EQ. ' X' .OR. &
     &               CPARM_NAMES(J1)(9:10) .EQ. ' Y' .OR. &
     &               CPARM_NAMES(J1)(9:10) .EQ. ' Z'      ) .AND. &
     &               CPARM_NAMES(J1)(17:20) .EQ. '    '           ) THEN
                   READ ( CPARM_NAMES(J1)(11:16), FMT='(3I2)', IOSTAT=IOS) &
     &                    IY, IM, ID
                   IF ( IOS .NE. 0            ) GOTO 410
                   IF ( IY .LT. 0                   ) GOTO 410
                   IF ( IM .LE. 0  .OR.  IM .GT. 12 ) GOTO 410
                   IF ( ID .LE. 0  .OR.  ID .GT. 31 ) GOTO 410
!
                   CALL BLANK_TO_ZERO ( CPARM_NAMES(J1)(11:16) )
                   CPARM_NAMES(J1)(17:20) = '-POS'
              END IF
 410       CONTINUE
      END IF
!%  write ( 6, * ) 'cgm_com(666) numsta= ', numsta, ' nparam= ', nparam, ' nparam_i2= ', nparam_i2, ' parm_num = ', parm_num, ' parm_num_i2= ', parm_num_i2 ! %%%%
!
      RETURN
      END  !#!  CGM_COM  #!#
