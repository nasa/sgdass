#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "png.h"
int main(int argc, char *argv[])
{
   int multiple = 0;
   int ierror = 0;

   png_structp dummy_ptr;

   dummy_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
}
