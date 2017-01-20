/*
 * 
 * Calcula la o que o programa do masinha cacula
 * 
 * Author:	J. Luis
 * Date:	24 April, 2002
 * Revision:	00/00/00 
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#define WINAPI __stdcall

#define minhout 0.01

#define	FALSE		0
#define	TRUE		1
#define M_PI		3.14159265358979323846
#ifndef M_PI_2
#define M_PI_2          1.57079632679489661923
#endif
#define D2R		M_PI / 180.
#define GMT_CONV_LIMIT  1.0e-8  /* Fairly tight convergence limit or "close to zero" limit */ 

#ifndef MIN
#define MIN(x, y) (((x) < (y)) ? (x) : (y))	/* min and max value macros */
#endif
#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#ifndef rint
#define rint(x) (floor((x)+0.5))
#endif
#ifndef irint
#define irint(x) ((int)rint(x))
#endif

/* Safe math macros that check arguments */

#define d_sqrt(x) ((x) < 0.0 ? 0.0 : sqrt (x))
#define d_acos(x) (fabs (x) >= 1.0 ? ((x) < 0.0 ? M_PI : 0.0) : acos (x))
#define d_asin(x) (fabs (x) >= 1.0 ? copysign (M_PI_2, (x)) : asin (x))
#define d_atan2(y,x) ((x) == 0.0 && (y) == 0.0 ? 0.0 : atan2 (y, x))
#define d_atn(y,x) ((x) == 0.0 && (y) == 0.0 ? 0.2 : atan2 (y, x))
#define copysign(x,y) ((y) < 0.0 ? -fabs(x) : fabs(x))
#define ijs(i,j) ((i) + (j)*nx)

struct srf_header {		/* Surfer file header structure */
	char id[4];		/* ASCII Binary identifier (DSAA/DSBB) */
	short int nx;		/* Number of columns */
	short int ny;		/* Number of rows */
	double x_min;		/* Minimum x coordinate */
	double x_max;		/* Maximum x coordinate */
	double y_min;		/* Minimum y coordinate */
	double y_max;		/* Maximum y coordinate */
	double z_min;		/* Minimum z value */
	double z_max;		/* Maximum z value */
};

typedef int BOOLEAN;              /* BOOLEAN used for logical variables */
double copysign_F(double x , double y);
double uscal(double x1, double x2, double x3, double c, double cc, double dp);
double udcal(double x1, double x2, double x3, double c, double cc, double dp);
double uscal_F(double x1, double x2, double x3, double c, double cc, double dp);
double udcal_F(double x1, double x2, double x3, double c, double cc, double dp);
void write_grd_ascii(char *grdfile, double x_min, double y_min, double dx, double dy, int i_end, int j_end, float *z);
void write_grd_bin(char *grdfile, double x_min, double y_min, double dx, double dy, int i_end, int j_end, float *z);
void deform (long addFlag, double x_min, double y_min, int i_end, int j_end, float *z, double dx, double dy, double fault_length, double fault_width, double th, double dip, double rake, double slip, double top_depth, double xl, double yl,long ipTot);

void meda (double x_min, double y_min, int i_end, int j_end, float *z, double dx, double dy, double side_x, double side_y, double height, double xl, double yl);
void tm (double lon, double lat, double *x, double *y);
void tm_F (double lon, double lat, double *x, double *y);
void vtm (double lon0, double lat0);
//long  getOkada(long addFlag, long dummy,float fault_lenght, float  fault_width,float top_depth,float dip,float th,float rake,float slip,float x_epic,float y_epic,float x_min, float x_max, float y_min, float y_max,float x_inc,float y_inc, float z[(NROWS+1)][(NCOLS+1)]);
long  getOkada(long AddFlag, long dummy, double fault_lenght, double  fault_width,double top_depth,double dip,double th,double rake,double slip
,double x_epic,double y_epic,float x_min, float x_max, float y_min, float y_max,float x_inc,float y_inc, float *z);

void set_q1(float * d, int i, int j,  int jmax, float value);
float get_q1(float * d, int i, int j, int imax);

int	is_geog = FALSE;
//double	fault_length = 0.0;		/*  */
//double	fault_width = 0.0;		/*  */
//double	top_depth = 0.0;	/* Top Fault depth */
//double	dip = 0.0;		/* Dip angle */
//double	th = 0.0;		/* Strike direction */
//double	rake = 0.0;		/* Rake angle */
//double	d = 0.0;		/* Dislocation */
//double	x_epic = 0.0;		/* x_epicenter coord */
//double	y_epic = 0.0;		/* y_epicenter coord */
double	EQ_RAD = 6378137.0;	/* WGS-84 */
double	flattening = 1.0/298.2572235630;
double	ECC2, ECC4, ECC6;
double	one_m_ECC2, i_one_m_ECC2;
double	t_c1, t_c2, t_c3, t_c4, t_e2, t_M0;
double	central_meridian;

/* --------------------------------------------------------------------------- */

long  getOkada(long AddFlag, long dummy, double fault_lenght, double  fault_width,double top_depth,double dip,double th,double rake,double slip
,double x_epic,double y_epic,float x_min, float x_max, float y_min, float y_max,float x_inc,float y_inc, float *z)
{
	// AddFlag=0:  h is the calculated one
	// Addflag=1:  h is added to previous one

	int	 i_end, j_end;
	long ipTota=dummy;
	/*fault_length *= 1000;
	fault_width *= 1000;
	top_depth *= 1000;*/

/*  we trust in the window limits
	x_max = x_min + (irint ((x_max - x_min) / x_inc)) * x_inc;
	y_max = y_min + (irint ((y_max - y_min) / y_inc)) * y_inc; */

	/* Compute i_end and j_end (that is, last row and last column) 
	values will be stored  in the indexes
	(1 , i_end) =>  (y_min , y_max) => (1,nrows+2) => (latmin-dx0/2, latmax+dx0/2)
	(1 , j_end) =>  (x_min , x_max) => (1,ncols+2) => (lonmin-dx0/2, lonmax+dx0/2) */

	i_end = irint ((y_max - y_min) / y_inc) + 1;
	j_end = irint ((x_max - x_min) / x_inc) + 1;
	is_geog = TRUE;

	//th1=270-th; // to report to 0 degree the geographical zero
	//th1=th;
	/* Initialize TM variables. Fault origin will be used as projection's origin. However,
	   this would set it as a singularity point. That's whay it is arbitrarely shifted
	   by a 1/4 of grid step. */ 
	if (is_geog) vtm(x_epic+x_inc/2, y_epic+y_inc/2);

	deform (AddFlag, x_min, y_min, i_end, j_end, z, x_inc, y_inc, fault_lenght, fault_width, th, dip, rake, slip, top_depth
, x_epic, y_epic,ipTota);
	return -1;
}


void deform (long addFlag, double x_min, double y_min, int i_end, int j_end, float *z, double dx, double dy, double fault_length, double fault_width, double th, double dip, double rake, double slip, double top_depth, double xl0, double yl0,long ipTot) {

/*	fault_length - comprimento da falha (m)
	fault_width  - largura da falha (m)
	th - azimute					strike angle
	dip - inclina� do plano falha em rela� horiz	dip angle
	rake - desloca��o obl�qua da falha		rake or slip angle 
	slip - escorregamento (m)				dislocation
	top_depth - profundidade do topo (m)		Top fault depth
*/

	double	h1, h2, ds, dd, xx, yy, x1, x2, x3, us, ud, sn_tmp, cs_tmp, tg_tmp;
	double	f1, f2, f3, f4, g1, g2, g3, g4, rx, ry, flld,fwwd,xl,yl;
	double EarthRadius,radius;
	int	i, j, k = 0;

	h1 = top_depth / sin(D2R * dip);
	h2 = top_depth / sin(D2R * dip) + fault_width;
	ds = -slip * cos(D2R * rake);
	dd = slip * sin(D2R * rake);
	sn_tmp = sin(D2R*th);	cs_tmp = cos(D2R*th);	tg_tmp = tan(D2R*dip);

    flld = fault_length / EQ_RAD * 180 / 3.1515;
    fwwd = fault_width / EQ_RAD * 180 / 3.1515;
	xl = xl0- fwwd / 2 * cs_tmp - flld / 2 * sn_tmp;
    yl = yl0+ fwwd / 2 * sn_tmp - flld / 2 * cs_tmp;

	for(i = 0; i < i_end; i++) {
		yy = y_min + dy * i;
		for(j = 0; j < j_end; j++) {
			xx = x_min + dx * j;
			if (is_geog)
			{
				// tm(xx-(xl-xl0), yy-(yl-yl0), &rx, &ry);	// Remember that (xl,yl) is already the proj origin
			   EarthRadius = 6225.75e3; //6340e3;
			   EarthRadius = 6371.00410883944e3 ;
               radius = EarthRadius * cos(yl / 180 * 3.1415927);
               rx = (xx - xl) / 180 * 3.1415927 * radius;
               ry = (yy - yl) / 180 * 3.1415927 * EarthRadius;
			}
			else {
				rx = xx - xl;
				ry = yy - yl;
			}
			x1 = rx*sn_tmp + ry*cs_tmp - fault_length/2.0;
			x2 = rx*cs_tmp - ry*sn_tmp + top_depth/tg_tmp;
			x3 = 0.0;
			f1 = uscal(x1, x2, x3, fault_length/2., h2, D2R*dip);
			f2 = uscal(x1, x2, x3, fault_length/2., h1, D2R*dip);
			f3 = uscal(x1, x2, x3, -fault_length/2., h2, D2R*dip);
			f4 = uscal(x1, x2, x3, -fault_length/2., h1, D2R*dip);
			g1 = udcal(x1, x2, x3, fault_length/2., h2, D2R*dip);
			g2 = udcal(x1, x2, x3, fault_length/2., h1, D2R*dip);
			g3 = udcal(x1, x2, x3, -fault_length/2., h2, D2R*dip);
			g4 = udcal(x1, x2, x3, -fault_length/2., h1, D2R*dip);
			us = (f1-f2-f3+f4)*ds / (12.* M_PI);
			ud = (g1-g2-g3+g4)*dd / (12.* M_PI);
			/*z[k++] = (fabs(us + ud) > 3e-3) ? (float)(us + ud) : 0;*/
			//z[k++] = (float)(us + ud);
			//z[i+1][j+1]=(float)(us + ud);

// added in order to avoid unecessary calculations
			float uspud;
			if (fabs(us + ud) > minhout ) {uspud=us + ud; }
			else	{uspud=0.;}
// end added
			if (addFlag==0)
			{
				set_q1(z, j+1,i+1, ipTot,(float) (uspud) );
			}
			else
			{
				float h0=get_q1(z,j+1,i+1,ipTot);
				set_q1(z, j+1,i+1, ipTot,(float)(uspud + h0));
			}
		}
	}
}


double uscal(double x1, double x2, double x3, double c, double cc, double dp) {
/* Computation of the vertical displacement due to the STRIKE and SLIP component */
	double sn, cs, c1, c2, c3, r, q, r2, r3, q2, q3, h, k, a1, a2, a3, f;
	double b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14;

	sn = sin(dp);	cs = cos(dp);
	c1 = c;		c2 = cc * cs;	c3 = cc * sn;
	r = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3-c3)*(x3-c3));
	q = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3+c3)*(x3+c3));
	r2 = x2*sn - x3*cs;	r3 = x2*cs + x3*sn;
	q2 = x2*sn + x3*cs;	q3 = -x2*cs + x3*sn;
	h = d_sqrt(q2*q2 + (q3+cc)*(q3+cc));
	k = d_sqrt(q2*q2 + (x1-c1)*(x1-c1));
	a1 = log(r+r3-cc);	a2 = log(q+q3+cc);	a3 = log(q+x3+c3);
	b1 = 1. + 3. * (tan(dp)*tan(dp));
	b2 = 3. * tan(dp) / cs;
	b3 = 2. * r2 * sn;
	b4 = q2 + x2 * sn;
	b5 = 2. * r2*r2 * cs;
	b6 = r * (r+r3-cc);
	b7 = 4. * q2 * x3 * sn*sn;
	b8 = 2. * (q2+x2*sn) * (x3+q3*sn);
	b9 = q * (q+q3+cc);
	b10 = 4. * q2 * x3 * sn;
	b11 = (x3+c3) - q3 * sn;
	b12 = 4. * q2*q2 * q3 * x3 * cs * sn;
	b13 = 2. * q + q3 + cc;
	b14 = pow(q,3) * pow((q+q3+cc),2);
	f = cs * (a1 + b1*a2 - b2*a3) + b3/r + 2.*sn*b4/q - b5/b6 + (b7-b8)/b9 + b10*b11/(pow(q,3)) - b12*b13/b14;

	return (f);
}


double udcal(double x1, double x2, double x3, double c, double cc, double dp) {
/* Computation of the vertical displacement due to the DIP SLIP component */
	double sn, cs, c1, c2, c3, r, q, r2, r3, q2, q3, h, k, a1, a2;
	double b1, b2, b3, d1, d2, d3, d4, d5, d6, t1, t2, t3, f;

	sn = sin(dp);	cs = cos(dp);
	c1 = c;		c2 = cc * cs;	c3 = cc * sn;
	r = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3-c3)*(x3-c3));
	q = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3+c3)*(x3+c3));
	r2 = x2*sn - x3*cs;	r3 = x2*cs + x3*sn;
	q2 = x2*sn + x3*cs;	q3 = -x2*cs + x3*sn;
	h = d_sqrt(q2*q2 + (q3+cc)*(q3+cc));
	k = d_sqrt(q2*q2 + (x1-c1)*(x1-c1));
	a1 = log(r+x1-c1);	a2 = log(q+x1-c1);
	b1 = q * (q+x1-c1);	b2 = r * (r+x1-c1);	b3 = q * (q+q3+cc);
	d1 = x1 - c1;		d2 = x2 - c2;		d3 = x3 - c3;
	d4 = x3 + c3;		d5 = r3 - cc;		d6 = q3 + cc;
	t1 = d_atn(d1*d2, (h+d4)*(q+h));
	t2 = d_atn(d1*d5, r2*r);
	t3 = d_atn(d1*d6, q2*q);
	f = sn * (d2*(2.*d3/b2 + 4.*d3/b1 - 4.*c3*x3*d4*(2.*q+d1)/(b1*b1*q)) - 6.*t1 + 3.*t2 - 6.*t3) + cs * (a1-a2 - 2.*(d3*d3)/b2 - 4.*(d4*d4 - c3*x3)/b1 - 4.*c3*x3*d4*d4*(2*q+x1-c1)/(b1*b1*q)) + 6.*x3*(cs*sn*(2.*d6/b1 + d1/b3) - q2*(sn*sn - cs*cs)/b1);

	return (f);
}



int no_sys_mem (char *where, int n) {	
		fprintf (stderr, "Fatal Error: %s could not allocate memory, n = %d\n", where, n);
		exit (-1);
}

void vtm (double lon0, double lat0) {
	/* Set up an TM projection (extract of GMT_vtm)*/
	double  lat2, s2, c2;
	
	lat0 *= D2R;
	lat2 = 2.0 * lat0;
	s2 = sin(lat2);
	c2 = cos(lat2);
	ECC2 = 2 * flattening - flattening * flattening;
	ECC4 = ECC2 * ECC2;
	ECC6 = ECC2 * ECC4;
	one_m_ECC2 = 1.0 - ECC2;
	i_one_m_ECC2 = 1.0 / one_m_ECC2;
	t_c1 = 1.0 - (1.0/4.0) * ECC2 - (3.0/64.0) * ECC4 - (5.0/256.0) * ECC6;
	t_c2 = -((3.0/8.0) * ECC2 + (3.0/32.0) * ECC4 + (25.0/768.0) * ECC6);
	t_c3 = (15.0/128.0) * ECC4 + (45.0/512.0) * ECC6;
	t_c4 = -(35.0/768.0) * ECC6;
	t_e2 = ECC2 * i_one_m_ECC2;
	t_M0 = EQ_RAD * (t_c1 * lat0 + s2 * (t_c2 + c2 * (t_c3 + c2 * t_c4)));
	central_meridian = lon0;
}

void tm (double lon, double lat, double *x, double *y) {
	/* Convert lon/lat to TM x/y (adapted from GMT_tm) */
	double N, T, T2, C, A, M, dlon, tan_lat, A2, A3, A5, lat2, s, c, s2, c2;

	if (fabs (fabs (lat) - 90.0) < GMT_CONV_LIMIT) {
		M = EQ_RAD * t_c1 * M_PI_2;
		*x = 0.0;
		*y = M;
	}
	else {
		lat *= D2R;
		lat2 = 2.0 * lat;
		s = sin(lat);	s2 = sin(lat2);
		c = cos(lat);	c2 = cos(lat2);
		tan_lat = s / c;
		M = EQ_RAD * (t_c1 * lat + s2 * (t_c2 + c2 * (t_c3 + c2 * t_c4)));
		dlon = lon - central_meridian;
		if (fabs (dlon) > 360.0) dlon += copysign (360.0, -dlon);
		if (fabs (dlon) > 180.0) dlon = copysign (360.0 - fabs (dlon), -dlon);
		N = EQ_RAD / d_sqrt (1.0 - ECC2 * s * s);
		T = tan_lat * tan_lat;
		T2 = T * T;
		C = t_e2 * c * c;
		A = dlon * D2R * c;
		A2 = A * A;	A3 = A2 * A;	A5 = A3 * A2;
		*x = N * (A + (1.0 - T + C) * (A3 * 0.16666666666666666667) + (5.0 - 18.0 * T + T2 + 72.0 * C - 58.0 * t_e2) * (A5 * 0.00833333333333333333));
		A3 *= A;	A5 *= A;
		*y = (M - t_M0 + N * tan_lat * (0.5 * A2 + (5.0 - T + 9.0 * C + 4.0 * C * C) * (A3 * 0.04166666666666666667) + (61.0 - 58.0 * T + T2 + 600.0 * C - 330.0 * t_e2) * (A5 * 0.00138888888888888889)));
	}
}



double uscal_F(double x1, double x2, double x3, double c, double cc, double dp) {
/* Computation of the vertical displacement due to the STRIKE and SLIP component */
	double sn, cs, c1, c2, c3, r, q, r2, r3, q2, q3, h, k, a1, a2, a3, f;
	double b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14;

	sn = sin(dp);	cs = cos(dp);
	c1 = c;		c2 = cc * cs;	c3 = cc * sn;
	r = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3-c3)*(x3-c3));
	q = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3+c3)*(x3+c3));
	r2 = x2*sn - x3*cs;	r3 = x2*cs + x3*sn;
	q2 = x2*sn + x3*cs;	q3 = -x2*cs + x3*sn;
	h = d_sqrt(q2*q2 + (q3+cc)*(q3+cc));
	k = d_sqrt(q2*q2 + (x1-c1)*(x1-c1));
	a1 = log(r+r3-cc);	a2 = log(q+q3+cc);	a3 = log(q+x3+c3);
	b1 = 1. + 3. * (tan(dp)*tan(dp));
	b2 = 3. * tan(dp) / cs;
	b3 = 2. * r2 * sn;
	b4 = q2 + x2 * sn;
	b5 = 2. * r2*r2 * cs;
	b6 = r * (r+r3-cc);
	b7 = 4. * q2 * x3 * sn*sn;
	b8 = 2. * (q2+x2*sn) * (x3+q3*sn);
	b9 = q * (q+q3+cc);
	b10 = 4. * q2 * x3 * sn;
	b11 = (x3+c3) - q3 * sn;
	b12 = 4. * q2*q2 * q3 * x3 * cs * sn;
	b13 = 2. * q + q3 + cc;
	b14 = pow(q,3) * pow((q+q3+cc),2);
	f = cs * (a1 + b1*a2 - b2*a3) + b3/r + 2.*sn*b4/q - b5/b6 + (b7-b8)/b9 + b10*b11/(pow(q,3)) - b12*b13/b14;

	return (f);
}


double udcal_F(double x1, double x2, double x3, double c, double cc, double dp) {
/* Computation of the vertical displacement due to the DIP SLIP component */
	double sn, cs, c1, c2, c3, r, q, r2, r3, q2, q3, h, k, a1, a2;
	double b1, b2, b3, d1, d2, d3, d4, d5, d6, t1, t2, t3, f;

	sn = sin(dp);	cs = cos(dp);
	c1 = c;		c2 = cc * cs;	c3 = cc * sn;
	r = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3-c3)*(x3-c3));
	q = d_sqrt((x1-c1)*(x1-c1) + (x2-c2)*(x2-c2) + (x3+c3)*(x3+c3));
	r2 = x2*sn - x3*cs;	r3 = x2*cs + x3*sn;
	q2 = x2*sn + x3*cs;	q3 = -x2*cs + x3*sn;
	h = d_sqrt(q2*q2 + (q3+cc)*(q3+cc));
	k = d_sqrt(q2*q2 + (x1-c1)*(x1-c1));
	a1 = log(r+x1-c1);	a2 = log(q+x1-c1);
	b1 = q * (q+x1-c1);	b2 = r * (r+x1-c1);	b3 = q * (q+q3+cc);
	d1 = x1 - c1;		d2 = x2 - c2;		d3 = x3 - c3;
	d4 = x3 + c3;		d5 = r3 - cc;		d6 = q3 + cc;
	t1 = d_atn(d1*d2, (h+d4)*(q+h));
	t2 = d_atn(d1*d5, r2*r);
	t3 = d_atn(d1*d6, q2*q);
	f = sn * (d2*(2.*d3/b2 + 4.*d3/b1 - 4.*c3*x3*d4*(2.*q+d1)/(b1*b1*q)) - 6.*t1 + 3.*t2 - 6.*t3) + cs * (a1-a2 - 2.*(d3*d3)/b2 - 4.*(d4*d4 - c3*x3)/b1 - 4.*c3*x3*d4*d4*(2*q+x1-c1)/(b1*b1*q)) + 6.*x3*(cs*sn*(2.*d6/b1 + d1/b3) - q2*(sn*sn - cs*cs)/b1);

	return (f);
}
void tm_F(double lon, double lat, double *x, double *y) {
	/* Convert lon/lat to TM x/y (adapted from GMT_tm) */
	double N, T, T2, C, A, M, dlon, tan_lat, A2, A3, A5, lat2, s, c, s2, c2;

	if (fabs (fabs (lat) - 90.0) < GMT_CONV_LIMIT) {
		M = EQ_RAD * t_c1 * M_PI_2;
		*x = 0.0;
		*y = M;
	}
	else {
		lat *= D2R;
		lat2 = 2.0 * lat;
		s = sin(lat);	s2 = sin(lat2);
		c = cos(lat);	c2 = cos(lat2);
		tan_lat = s / c;
		M = EQ_RAD * (t_c1 * lat + s2 * (t_c2 + c2 * (t_c3 + c2 * t_c4)));
		dlon = lon - central_meridian;
		if (fabs (dlon) > 360.0) dlon += copysign_F (360.0, -dlon);
		if (fabs (dlon) > 180.0) dlon = copysign_F (360.0 - fabs (dlon), -dlon);
		N = EQ_RAD / d_sqrt (1.0 - ECC2 * s * s);
		T = tan_lat * tan_lat;
		T2 = T * T;
		C = t_e2 * c * c;
		A = dlon * D2R * c;
		A2 = A * A;	A3 = A2 * A;	A5 = A3 * A2;
		*x = N * (A + (1.0 - T + C) * (A3 * 0.16666666666666666667) + (5.0 - 18.0 * T + T2 + 72.0 * C - 58.0 * t_e2) * (A5 * 0.00833333333333333333));
		A3 *= A;	A5 *= A;
		*y = (M - t_M0 + N * tan_lat * (0.5 * A2 + (5.0 - T + 9.0 * C + 4.0 * C * C) * (A3 * 0.04166666666666666667) + (61.0 - 58.0 * T + T2 + 600.0 * C - 330.0 * t_e2) * (A5 * 0.00138888888888888889)));
	}
}


void set_q1(float *d, int i, int j, int imax, float value)
{
    d[(j ) * (imax+1) + i] = value;
}
float get_q1(float * d, int i, int j, int imax)
{
    return d[(j ) * (imax+1) + i];
}

double copysign_F(double x , double y)
{
	double s=1;
	if (y<0) s=-1;
	return s * abs(x);
}
