#if defined addunderscore
#define GETOKADA getOkada_
#define DEFORM deform_
#endif

/* --------------------------------------------------------------------------- */
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

#define	FALSE		0
#define	TRUE		1
#define M_PI		3.14159265


/* Safe math macros that check arguments */

#define d_sqrt(x) ((x) < 0.0 ? 0.0 : sqrt (x))
#define d_atn(y,x) ((x) == 0.0 && (y) == 0.0 ? 0.2 : atan2 (y, x))


/* --------------------------------------------------------------------------- */
double uscal(double x1, double x2, double x3, double c, double cc, double dp);
double udcal(double x1, double x2, double x3, double c, double cc, double dp);

void set_q1(float * d, long i, long j,  long jmax, float value);
float get_q1(float * d, long i, long j, long imax);
/* --------------------------------------------------------------------------- */


static double EQ_RAD = 6378137.0;	/* WGS-84 */
static double D2R, R2D, D2ARC, ARC2D;


/* --------------------------------------------------------------------------- */
#if defined WIN32
extern void __stdcall DEFORM
#else
void DEFORM 
#endif
 (long addFlag, float x_min, float y_min, long i_end, long j_end, float z[]
	, float dx, float dy, float fault_length, float fault_width
	, float th, float dip, float rake, float d, float top_depth
	, float xl0, float yl0,long ipTot, long is_geog) 
/* --------------------------------------------------------------------------- */
/*	fault_length - comprimento da falha (m)
	fault_width  - largura da falha (m)
	th           - azimute					strike angle
	dip          - inclinaç do plano falha em relaç horiz	dip angle
	rake         - deslocação oblíqua da falha		rake or slip angle 
	d            - escorregamento (m)				dislocation
	top_depth    - profundidade do topo (m)		Top fault depth
	i_end        - no. of output rows:  0 <= i < i_end
	j_end        - no. of output columns: 0 <= j < j_end
	ipTot        - no. of columns in the pointed array:  ipTot >= j_end
	is_geog      - 0 projected (x,y) coordinate
			     - 1 geographic (lat/long) coordinate
	AddFlag=0:  h is the calculated one
	Addflag=1:  h is added to previous one
*/
/* --------------------------------------------------------------------------- */
{
	double	h1, h2, ds, dd, xx, yy, x1, x2, x3, us, ud, sn_tmp, cs_tmp, tg_tmp;
	double	f1, f2, f3, f4, g1, g2, g3, g4, rx, ry, flld,fwwd,xl,yl;
	int	i, j, k = 0;

	h1 = top_depth / sin(D2R * dip);
	h2 = top_depth / sin(D2R * dip) + fault_width;
	
	ds = -d * cos(D2R * rake);
	dd = d * sin(D2R * rake);
	sn_tmp = sin(D2R*th);	
	cs_tmp = cos(D2R*th);	
	tg_tmp = tan(D2R*dip);

	if (is_geog) {
    flld = fault_length * ARC2D;
    fwwd = fault_width * ARC2D;
	} else {
    flld = fault_length ;
    fwwd = fault_width ;
	}

	xl = xl0- fwwd / 2 * cs_tmp - flld / 2 * sn_tmp;
    yl = yl0+ fwwd / 2 * sn_tmp - flld / 2 * cs_tmp;

	for(i = 0; i < i_end; i++) {
		yy = y_min + dy * i;
		for(j = 0; j < j_end; j++) {
			k = ipTot * i + j;
			xx = x_min + dx * j;
			if (is_geog)
			{
               rx = (xx - xl) * D2ARC * cos(yl * D2R);
               ry = (yy - yl) * D2ARC ;
			} else {
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

/* void set_q1(float * d, long i, long j, long imax, float value)
{   d[ j * imax + i] = value;} */
			if (addFlag==0)
//				set_q1(z, j+1,i+1, ipTot,(float)(us + ud));
            z[k] = (float)(us + ud) ;
			else
			{
//				float h0=get_q1(z,j+1,i+1,ipTot);
//				set_q1(z, j+1,i+1, ipTot,(float)(us + ud + h0));
			z[k] += (float)(us + ud);
			}
		}
	}
}

/* --------------------------------------------------------------------------- */
#if defined WIN32
extern void __stdcall GETOKADA 
#else
void GETOKADA 
#endif
( long AddFlag, long ipTot, long is_geog, float radius
	,float fault_lenght , float  fault_width , float top_depth
	,float dip , float th , float rake , float d
	,float x_epic , float y_epic
	,float x_min , long j_end  
	,float y_min , long i_end  
	,float x_inc , float y_inc
	,void *z)
{
D2R = M_PI / 180.0 ;
R2D = 180.0 / M_PI ;
EQ_RAD = radius;	
D2ARC = D2R * EQ_RAD  ;
ARC2D = R2D / EQ_RAD ;

DEFORM (AddFlag, x_min, y_min, i_end, j_end, z
		, x_inc, y_inc, fault_lenght, fault_width, th, dip, rake, d, top_depth, x_epic, y_epic, ipTot, is_geog);
}

/* --------------------------------------------------------------------------- */
double uscal(double x1, double x2, double x3, double c, double cc, double dp) {
/* --------------------------------------------------------------------------- */
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


/* --------------------------------------------------------------------------- */
double udcal(double x1, double x2, double x3, double c, double cc, double dp) {
/* --------------------------------------------------------------------------- */
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
	f = sn * (d2*(2.*d3/b2 + 4.*d3/b1 - 4.*c3*x3*d4*(2.*q+d1)/(b1*b1*q)) - 6.*t1 + 3.*t2 - 6.*t3) 
	  + cs * (a1-a2 - 2.*(d3*d3)/b2 - 4.*(d4*d4 - c3*x3)/b1 - 4.*c3*x3*d4*d4*(2*q+x1-c1)/(b1*b1*q)) 
	  + 6.*x3*(cs*sn*(2.*d6/b1 + d1/b3) - q2*(sn*sn - cs*cs)/b1);

	return (f);
}

/* --------------------------------------------------------------------------- */
void set_q1(float * d, long i, long j, long imax, float value)
{   d[ j * imax + i] = value;}
/* --------------------------------------------------------------------------- */
float get_q1(float * d, long i, long j, long imax)
{
    return d[ j * imax + i];
}

