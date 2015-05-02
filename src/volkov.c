#include <math.h>
#include <Rmath.h>
#include <R.h>

/*** Calculates the Volkov distribution density
     Based on code from untb::volkov, by Robin K. S. Hankin
     http://cran.r-project.org/web/packages/untb/ */

double f(double y, int n, double ln, int J, double upper, double theta) {
	return exp(ln + lgamma(n+y) + lgamma(J-n+upper-y) - lgamma(1+y) - lgamma(upper-y)- y*theta/upper);
}

double integrate(int n, double lJm, int J, double upper, double theta, int N) {
	double h = upper/N;
	double ln = lJm - lgamma(n+1) - lgamma(J-n+1);
	/* upper = J causes NaN, slight approximation here */
	double part = f(0, n, ln, J, upper, theta) + f(upper-1.0/N/N, n, ln, J, upper, theta);
	for (int i = 1; i < N; i+=2)
		part += 4.0 * f(i*h, n, ln, J, upper, theta);
	for (int i = 2; i < N; i+=2)
		part += 2.0 * f(i*h, n, ln, J, upper, theta);
	return part * h / 3.0;
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
