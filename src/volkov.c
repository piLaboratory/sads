#include <math.h>
#include <Rmath.h>
#include <R.h>
#include "gauss_legendre.h"

/*** Calculates the Volkov distribution density
     Based on code from untb::volkov, by Robin K. S. Hankin
     http://cran.r-project.org/web/packages/untb/ */

double f(double y, void * _data) { 
	double * data = (double *) _data;
	return exp( data[1] + lgamma(data[0]+y) + lgamma(data[2]-data[0]+data[3]-y) - lgamma(1+y) 
		- lgamma(data[3]-y) - y * data[4]/ data[3] );
}

/* Calculates the volkov density function */
extern
void volkov ( double * res, double * theta0, double * m0, int * J0, int * N0, double * total) {
	double data[5]; /* Stores: n, ln, J, upper(=gam), theta */
	data[2] = J0[0]; data[3] = m0[0]*(J0[0]-1)/(1-m0[0]); data[4] = theta0[0];
	/* some preliminary calculations to speed up
	   lJm is the term that only depends on J and m */
	double lJm = lgamma(data[2]+1) + lgamma(data[3]) - lgamma(data[2]+data[3]);
	for (int i = 0; i < J0[0]; i++) {
		data[0] = i + 1.0;
		data[1] = lJm - lgamma(i+2) - lgamma(data[2]-i);
		res[i] = data[4] * gauss_legendre(N0[0],f,data,0,data[3]);
		total[0] += res[i];
	}
	for (int i = 0; i < J0[0]; i++) {
		res[i] /= total[0];
	}
}
