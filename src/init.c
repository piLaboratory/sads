#include <stdlib.h> 
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

void volkov ( double * res, double * theta0, double * m0, int * J0, int * N0, double * total);

/* Registering native routine for R 3.4.0 */
static R_NativePrimitiveArgType C_types[] = {
   REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, REALSXP
};

static const R_CMethodDef cMethods[] = {
   {"volkov", (DL_FUNC) &volkov, 6, C_types},
   {NULL, NULL, 0, NULL}
};

void R_init_sads(DllInfo *dll) {
   R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
   R_forceSymbols(dll, TRUE);
}
