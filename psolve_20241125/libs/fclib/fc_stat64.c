#define _LARGEFILE_SOURCE 
#define _FILE_OFFSET_BITS 64 
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/*
# ************************************************************************
# *                                                                      *
# *   This routine returns the binary dat of the last moditication of    *
# *   a file and its size as INTEGER*8.                                  *
# *                                                                      *
# *   Usage:                                                             *
# *                                                                      *
# *   IS = FC_FSTAT64 ( FILENAME(1:I_LEN(FILENAME))/CHAR(0), UNIX_DATE,  *
# *                     SIZE_I8 )                                        *
# *                                                                      *
# * _________________________ Input parameters: ________________________ *
# *                                                                      *
# *                                                                      *
# *   FILENAME  ( CHARACTER ) -- Null-terminated string with the file    *
# *                              name.                                   *
# *                                                                      *
# * _________________________ Output parameters: _______________________ *
# *                                                                      *
# *                                                                      *
# * <FC_FSTAT64> ( INTEGER*4 ) -- Status. 0 -- success, non-zero means   *
# *                               the error. The error mesage can be     *
# *                               recovered if CALL GERROR ( STR ) is    *
# *                               **immediately** after FC_FSTAT64.      *
# *                               In the case of errorr UNIS_DATE=0,     *
# *                               SIZE_I8 = -1                           *
# *   UNIX_DATE  ( INTEGER*4 ) -- Date of last modification in UNIX      *
# *                              format.                                 *
# *   SIZE_I8   ( INTEGER*8 ) -- File size in bytes.                     *
# *                                                                      *
# *   Caveat: it was tested under HP-US in 32-bits mode, but it was not  *
# *           tested in 64-bits mode. It was tested in both 32-bits and  *
# *           64-bits mode under Linux.                                  *
# *                                                                      *
# *  ### 23-APR-2004    fc_stat8   v1.0 (c)  L. Petrov  23-APR-2004 ###  *
# *                                                                      *
# ************************************************************************
*/


#ifdef _NEEDED
long int fc_stat64_ ( fname, date_mod, size, flen )
#else
long int fc_stat64  ( fname, date_mod, size, flen )
#endif

 char *fname;
 int  *date_mod;
 long long *size;
 int  flen;

{
  struct stat stat_block;
  long is;

  is = stat64 ( fname, &stat_block );

  if ( is==0 )
     {
       *date_mod = stat_block.st_atime;
       *size = stat_block.st_size;
     }
    else
     {
       *date_mod = 0;
       *size = -1 ;

     }

  return (is);
}
