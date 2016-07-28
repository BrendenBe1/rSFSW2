/* filefuncs.h -- contains definitions related to */
/* some generic file management functions

 * REQUIRES: generic.h
 */

/* Chris Bennett @ LTER-CSU 6/15/2000            */

#ifndef FILEFUNCS2_H

#include "generic2.h"

/***************************************************
 * Function definitions
 ***************************************************/
FILE * OpenFile(const char *, const char *) ;
void CloseFile( FILE **) ;
Bool GetALine( FILE *f, char buf[]);
char *DirName( const char *p);
const char *BaseName( const char *p);
Bool file_exists(const char * filename);
Bool FileExists( const char *f);
Bool DirExists( const char *d);
Bool ChDir( const char *d);
Bool MkDir( const char *d);
void MkDir2( const char *d);
Bool RemoveFiles(const char *fspec);

extern char inbuf[];  /* declare in main, use anywhere */

#define FILEFUNCS2_H
#endif
