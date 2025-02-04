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
# *   IS = FILE_INFO ( FILENAME(1:I_LEN(FILENAME))//CHAR(0), UNIX_DATE, &*
# *                    SIZE_I8 )                                         *
# *   IF ( IS .NE. 0 ) THEN                                              *
# *        CALL GERROR ( STR )                                           *
# *        WRITE ( 6, * ) 'Error in FILE_INFO: '//STR )                  *
# *   END IF                                                             *
# *                                                                      *
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
# *  <FILE_INFO> ( INTEGER*4 ) -- Status. 0 -- success, non-zero means   *
# *                               the error. The error mesage can be     *
# *                               recovered if CALL GERROR ( STR ) is    *
# *                               **immediately** after FILE_INFO.       *
# *                               In the case of error UNIX_DATE=0,      *
# *                               SIZE_I8 = -1                           *
# *   UNIX_DATE  ( INTEGER*4 ) -- Date of last modification in UNIX      *
# *                               format.                                *
# *   SIZE_I8    ( INTEGER*8 ) -- File size in bytes.                    *
# *                                                                      *
# *   Caveat: it was tested under HP-UX in 32-bits mode, but it was not  *
# *           tested in 64-bits mode. It was tested in both 32-bits and  *
# *           64-bits mode under Linux.                                  *
# *                                                                      *
# *  ### 23-APR-2004   file_info   v1.1 (c)  L. Petrov  2008.12.30  ###  *
# *                                                                      *
# ************************************************************************
*/


#ifdef _NEEDED
long int file_info_ ( fname, date_mod, size, flen )
#else
long int file_info  ( fname, date_mod, size, flen )
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
       *date_mod = stat_block.st_mtime;
       *size = stat_block.st_size;
     }
    else
     {
       *date_mod = 0;
       *size = -1;

     }

  return (is);
}
