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
		- lgamma(data[3]-y)- y * data[4]/ data[3] );
}

double integrate(int n, double lJm, int J, double upper, double theta, int N) {
	double ln = lJm - lgamma(n+1) - lgamma(J-n+1);
	double data[5]; data[0] = n; data[1] = ln; data[2] = J; data[3] = upper; data[4] = theta;
	return gauss_legendre(N,f,data,0,upper);
}

extern
void volkov ( double * res, double * theta0, double * m0, int * J0, int * N0) {
	int J = J0[0], N = N0[0];
	double theta = theta0[0], m = m0[0], upper = m*(J-1)/(1-m), total = 0;
	/* some preliminary calculations to speed up
	   lJm is the term that only depends on J and m */
	double lJm = lgamma(J+1) + lgamma(upper) - lgamma(J+upper);
	for (int i = 0; i < J; i++) {
		res[i] = theta * integrate(i+1, lJm, J, upper, theta, N);
		total += res[i];
	}
	for (int i = 0; i < J; i++) {
		res[i] /= total;
	}
}
