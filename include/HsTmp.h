#ifndef HSTMP_H
#define HSTMP_H

#include "HsTmpConfig.h"

#include <stdlib.h>
#include <stdio.h>


#if HAVE_MKSTEMP
int __hscore_mkstemp(char *filetemplate);
#endif

#if HAVE_MKSTEMPS
int __hscore_mkstemps(char *filetemplate, int suffixlen);
#endif

#if HAVE_MKDTEMP
char *__hscore_mkdtemp(char *filetemplate);
#endif

#endif
