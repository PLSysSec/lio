#ifndef HSTMP_H
#define HSTMP_H

#include "HsTmpConfig.h"

#include <stdlib.h>
#include <stdio.h>


#if HAVE_MKSTEMP
int __hstmp_mkstemp(char *filetemplate);
#endif

#if HAVE_MKSTEMPS
int __hstmp_mkstemps(char *filetemplate, int suffixlen);
#endif

#if HAVE_MKDTEMP
char *__hstmp_mkdtemp(char *filetemplate);
#endif

#endif
