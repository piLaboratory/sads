#include <math.h>
#include <Rmath.h>
#include <R.h>
#include <stdio.h>
#define MAX 20000

/* Calculates the Volkov distribution density */

double f(double y, int n, double ln, int J, double gam, double theta) {
	return exp(ln + lgamma(n+y) + lgamma(J-n+gam-y) - lgamma(1+y) - lgamma(gam-y)- y*theta/gam);
}

double integrate(int n, double lJm, int J, double gam, double theta, double from, double to) {
	int N = 200; /* hardcoded subdividions, may transform in f parameter later */
	double h = (to-from)/ (double) N;
	double ln = lJm - lgamma(n+1) - lgamma(J-n+1);
	double part = f(from, n, ln, J, gam, theta) + f(to, n, ln, J, gam, theta);
	for (int i = 1; i < N; i+=2)
		part += 4.0 * f(from + i*h, n, ln, J, gam, theta);
	for (int i = 2; i < N; i+=2)
		part += 2.0 * f(from + i*h, n, ln, J, gam, theta);
	return part * h / 3.0;
}

extern
void volkov ( double * x0, double * theta0, double * m0, int * J0) {
	int J = J0[0];
	double theta = theta0[0], m = m0[0], gam = m*(J-1)/(1-m);
	/* some preliminary calculations to speed up
	   lJm is the term that only depends on J and m */
	double lJm = lgamma(J+1) + lgamma(gam) - lgamma(J+gam);
	x0[0] = theta * integrate(1, lJm, J, gam, theta, 0, 1);
}

