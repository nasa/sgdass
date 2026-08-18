!
! >>>>> INCLUDE-BLOCK with description of data structures used by
!       vcat 
!
!       vcat.i  14-AUG -2003 11:53:28 v 1.1 Leonid Petrov  2020.06.08_13:53:41
!
        CHARACTER     VCAT__LABEL*50
        PARAMETER   ( VCAT__LABEL =    '# VCAT  Configuration file.  Version of 2020.06.08' )
        CHARACTER     VCAT__LABEL_V1*49
        PARAMETER   ( VCAT__LABEL_V1 = '# VCAT  Configuration file. Version of 2006.02.18'  )
!
        INTEGER*4  VCAT__MDIRS
        PARAMETER  ( VCAT__MDIRS = 8 )
        INTEGER*4  VCAT__UNDF, VCAT__LOADED
        PARAMETER  ( VCAT__UNDF   = 0 )
        PARAMETER  ( VCAT__LOADED = 292378124 )
!
	TYPE      VCAT__TYPE
           INTEGER*4  NREPS
           CHARACTER  GVF_REP_NAME(VCAT__MDIRS)*3
           CHARACTER  GVF_ENV_DIR(VCAT__MDIRS)*128
           CHARACTER  GVF_DB_DIR(VCAT__MDIRS)*128
           CHARACTER  VTD_CONF_SES_FILE*128
           CHARACTER  CONF_FILE*128
	   INTEGER*4  STATUS
        END TYPE  VCAT__TYPE
!
! <<<<< end of INCLUDE-BLOCK  vcat.i
!
