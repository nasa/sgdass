#include <dirent.h>

// ************************************************************************
// *                                                                      *
// *   These routines are a hack to a workaround of MAC OS readdir and    *
// *   opendir problem. MACOS has a pair of routines                      *
// *   readdir/readdir$INODE64 and  opendir/opendir$INODE64. When         *
// *   a C progam is linked, opendir is replaced with opendir$INODE64 and *
// *   readdir is replaced with readdir$INODE64. But this substitute does *
// *   not happen when reddir or opendir are called from Fortran! If to   *
// *   call opendir$INODE64 and readdir$INODE64 directly, Fortran         *
// *   compiler converts names to opendir$inode64 and readdir$inode64.    *
// *   These two functions just convert a call of opendir$inode64 to      *
// *   opendir$INODE64 and a call of readdir$inode64 to readdir$INODE64.  *
// *                                                                      *
// *  ### 06-APR-2014               v1.0 (c)  L. Petrov  06-APR-2014 ###  *
// *                                                                      *
// ************************************************************************

opendir$inode64(const char *dirname)
{
#ifdef DARWIN
   return opendir$INODE64 ( (const char *) dirname);
#else
   return opendir ( (const char *) dirname);
#endif
}

readdir$inode64(DIR *dirp)
{
#ifdef DARWIN
   return readdir$INODE64 ( (DIR *) dirp);
#else
   return readdir ( (DIR *) dirp);
#endif
}
