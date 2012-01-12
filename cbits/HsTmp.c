#include "HsTmp.h"

#if HAVE_MKSTEMP
int __hscore_mkstemp(char *filetemplate) {
    return (mkstemp(filetemplate));
}
#endif

#if HAVE_MKSTEMPS
int __hscore_mkstemps(char *filetemplate, int suffixlen) {
    return (mkstemps(filetemplate, suffixlen));
}
#endif

#if HAVE_MKDTEMP
char *__hscore_mkdtemp(char *filetemplate) {
    return (mkdtemp(filetemplate));
}
#endif
