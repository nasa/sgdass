#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "zlib.h"
#  define SET_BINARY_MODE(file)
#define CHUNK 16384
// int def(FILE *source, FILE *dest, int level)

int main(){
    int ret, flush, level;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];
    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit(&strm, level);
/*    if (ret != Z_OK) */
/*            return ret; */
}
