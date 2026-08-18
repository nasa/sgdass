!
! This file is generated automatically by use_local.f from
!      template_file: /mk5/include/general.templ
!      local customization file: /mk5/local/solve.lcl
!
!      PLEASE DON'T EDIT THIS FILE!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Start of general.i
!
!     created by kdb, 970807  Last revision: 01-FEB-2000 10:54:38
!
!
! NOTE FOR PROGRAMMERS:
!
!      Lines which starts from & are meta-definitions and they are tied with
!            local customization file.
!      The value of the parameter is an installation-specific.
!      Format of meta-commands:
!      a) &V&  <param_name> -- definition of the name of non-character type
!      b) &C0& <param_name> -- definition of the name of character type.
!                               No restrictions on the length of the line
!                               (but the line should not be empty)
!      c) &C<max_length>& <param_name> -- definition of the name of character
!                                         type. But the value should not have
!                                         length more than <max_length>
!                                         characters.
!      Actual values are are taken from the local customization file and
!      the output include file is generated.
!
!
!     modifications
!     kdb 980307      Add EXPORT_DB_MASTER_DIR
!     kdb 980310      Consolidate EXPER_CAT parameter, which was in
!                     catalog_parameters.i and gsfcb.i.
!     kdb 980310      Move export_areas and export_key parameters from
!                     catalog_parameters.i.  (These are now needed by the
!                     last_versions applications program.)
!     Lanotte  980402 Update for accommodation of MATECGS preferences
!     pet      980808 Update for accommodation of USNO    preferences
!     Tomasi   981009 Update for accommodation of IRACNR preferences
!     kdb      981020 Change value of export_db_master_dir.
!     kdb      990504 Update for accommodation of VALENCIA preferences.
!     pet      990920 Update for accommodation of LEIPZIG  preferences
!     pet      2000.01.28  Added dummy values for GCCFA and KASHIMA
!     pet      2000.02.01  Add preferences for MPIFR (Bonn).
!     Calvin   2000.04.19  Add preferences for NRCAN (GSD/Canada).
!     pet      2000.05.24  Moved installation-specific customization to
!                          local file
!
!     max_bds - maximum number of block devices expected on the UNIX system
!
      INTEGER*2   MAX_BDS
      PARAMETER ( MAX_BDS = 300 )
!
!     export_db_master_dir - location of master directory for controlling
!                            the database export area.  (E.g., index files
!                            to the export area's databases go here.)
!
      CHARACTER*18 EXPORT_DB_MASTER_DIR
      PARAMETER ( EXPORT_DB_MASTER_DIR = '/datav/cddisa_vlbi' )
!
! --- Path to the catalog of experiments
!
      CHARACTER EXPER_CAT*9
      PARAMETER ( EXPER_CAT = '/dev/null' ) ! Local customization
!
!     Parameters that allow a set of directories to be set aside for
!     databases that are ready for export to other installations
!     (for example, by having the other installations ftp to the areas).
!     If EXPORT_AREAS is set to .true., this feature is turned on, and
!     database directories whose names contain the string given by
!     EXPORT_KEY  will be treated as special export directories.
!
      LOGICAL*2 EXPORT_AREAS
      PARAMETER ( EXPORT_AREAS = .TRUE. )
      CHARACTER*6 EXPORT_KEY
      PARAMETER ( EXPORT_KEY = 'cddisa' )
