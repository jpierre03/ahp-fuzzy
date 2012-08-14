/** envalue.cpp : Defines the entry point for the console application.
 *  Creation : (date unknown) by (unknown)
 *             Source : http://www.codeforge.com/read/64380/normal.cpp__html
 *  License : Undefined, so considered as Public Domain
 */

#include <math.h>
#include <vector>
#include <complex>
#include <iostream>
#include <fstream>

//#define BG_LIGHT
#ifndef BG_LIGHT
	#define BLACK    "\033[1;30m"
	#define RED      "\033[1;31m"
	#define GREEN    "\033[1;32m"
	#define YELLOW   "\033[1;33m"
	#define BLUE     "\033[1;34m"
	#define PURPLE   "\033[1;35m"
	#define CYAN     "\033[1;36m"
	#define GREY     "\033[1;37m"
#else
	#define BLACK    "\033[0;30m"
	#define RED      "\033[0;31m"
	#define GREEN    "\033[0;32m"
	#define YELLOW   "\033[0;33m"
	#define BLUE     "\033[0;34m"
	#define PURPLE   "\033[0;35m"
	#define CYAN     "\033[0;36m"
	#define GREY     "\033[0;37m"
#endif

#define DEFAULT_COLOR "\033[0;m"

#define FORMAT(color_delimiter,color_text, color_comment,delimiter, comment, x,y)	\
	do { printf("%s%s%s%s%s%s%s  %s%s%s\n",\
			color_delimiter,delimiter,\
			color_text,	comment,\
			color_delimiter,delimiter,\
			color_comment,\
			x,y,\
			DEFAULT_COLOR);\
       	} while (0)

#define OK(x,y)		do { FORMAT(BLUE,GREEN,DEFAULT_COLOR,"|","ok",x,y);} while (0)
#define NOK(x,y)	do { FORMAT(BLUE,RED,DEFAULT_COLOR,"|","nok",x,y);} while (0)

//int global_ok_nb=0;
//#define OK(x,y)		do { printf("%s[%s%d%s]%s   %s%s\n",BLUE,GREEN,++global_ok_nb,BLUE,DEFAULT_COLOR,x,y); } while (0)
//#define NOK(y)		do { printf("%s[%s%s%s]%s  %s\n",BLUE,RED,"nok",BLUE,DEFAULT_COLOR,y); } while (0)

#define FUNCTION(y)	do { FORMAT(BLUE,GREEN,PURPLE,	" ","  ","\t\t\tfunction "	,y);} while (0)
#define OPERATOR(y)	do { FORMAT(BLUE,GREEN,PURPLE,	" ","  ","\t\t\toperator "	,y);} while (0)
#define L1(y)		do { FORMAT(BLUE,GREEN,YELLOW,	"|","ok",""			,y);} while (0)
#define L2(y)		do { FORMAT(BLUE,GREEN,GREEN,	"|","ok","\t "		,y);} while (0)
#define L3(y)		do { FORMAT(BLUE,GREEN,CYAN,	"|","ok","\t\t "		,y);} while (0)

using namespace std;

typedef vector<double> double_vector;

vector<vector<double> > Inverse(vector<vector<double> > a, bool &judge) {
	FUNCTION("Inverse");
	vector<vector<double> > b(a);
	if (b.size() != b[0].size()) {
		judge = false;
		return b;
	} else {
		double temp = 1;
		for (int k = 0; k < (int) b.size(); ++k) {
			if (k < ((int) b.size() - 1)) {
				int tk = k;
				double tem = fabs(b[k][k]);
				for (int i = k; i < (int) b.size(); ++i) {
					if (fabs(b[i][k]) > tem) {
						tk = i;
						tem = fabs(b[i][k]);
					}
				}
				if (tk != k) {
					swap(b[tk], b[k]);
				}
				if (b[k][k] == 0) {
					temp = 0;
				} else {
					for (int i = k + 1; i < (int) b.size(); ++i) {
						tem = b[i][k];
						for (int j = k; j < (int) b[i].size(); ++j) {
							b[i][j] -= (b[k][j] * tem / b[k][k]);
						}
					}
				}
			}
		}
		if (temp != 0) {
			for (int i = 0; i < (int) b.size(); ++i) {
				temp = temp * b[i][i];
			}
		}
		if (temp == 0) {
			judge = false;
			return a;
		} else {
			vector<vector<double> > I;
			for (int i = 0; i < (int) a.size(); ++i) {
				vector<double> z(a.size(), 0);
				z[i] = 1;
				I.push_back(z);
			}
			for (int k = 0; k < (int) a.size(); ++k) {
				int tk = k;
				double tem = fabs(a[k][k]);
				for (int i = k; i < (int) a.size(); ++i) {
					if (fabs(a[i][k]) > tem) {
						tk = i;
						tem = fabs(a[i][k]);
					}
				}
				if (tk != k) {
					swap(a[tk], a[k]);
					swap(I[tk], I[k]);
				}
				tem = a[k][k];
				for (int i = 0; i < (int) a[k].size(); ++i) {
					a[k][i] /= tem;
					I[k][i] /= tem;
				}
				for (int i = 0; i < (int) a.size(); ++i) {
					if (i != k) {
						double temp = a[i][k];
						for (int j = 0; j < (int) a[i].size(); ++j) {
							a[i][j] -= (a[k][j] * temp);
							I[i][j] -= (I[k][j] * temp);
						}
					}
				}
			}
			judge = true;
			return I;
		}
	}
}

vector<vector<double> > operator -(vector<vector<double> > a,vector<vector<double> > b) {
	OPERATOR("-");
	bool dd = true;
	if (a.size() != b.size()) {
		dd = false;
	} else {
		for (int i = 0; i < (int) a.size(); ++i) {
			if (a[i].size() != b[i].size()) {
				dd = false;
			} else {
				for (int j = 0; j < (int) a.size(); ++j) {
					a[i][j] -= b[i][j];
				}
			}
		}
	}
	vector<vector<double> > ss;
	if (dd == false) {
		return ss;
	} else {
		ss = a;
		return ss;
	}
}

vector<vector<double> > operator *(double a, vector<vector<double> > b) {
	OPERATOR("*");
	for (int i = 0; i < (int) b.size(); ++i)
		for (int j = 0; j < (int) b[i].size(); ++j) {
			b[i][j] *= a;
		}
	return b;
}

vector<double> operator *(vector<vector<double> > a, vector<double> b) {
	OPERATOR("*");
	vector<double> c;
	for (int i = 0; i < (int) a.size(); ++i) {
		double s = 0;
		for (int j = 0; j < (int) b.size(); ++j) {
			s += a[i][j] * b[j];
		}
		c.push_back(s);
	}
	return c;
}

vector<vector<double> > operator *(vector<vector<double> > a,
		vector<vector<double> > b) {
	OPERATOR("*");
	if (a[0].size() != b.size()) {
		vector<vector<double> > ss;
		return ss;
	} else {
		vector<vector<double> > ss(a.size());
		for (int i = 0; i < (int) ss.size(); ++i) {
			for (int j = 0; j < (int) b[0].size(); ++j) {
				double temp = 0;
				for (int k = 0; k < (int) b.size(); ++k) {
					temp += (a[i][k] * b[k][j]);
				}
				ss[i].push_back(temp);
			}
		}
		return ss;
	}
}

vector<vector<double> > operator *(vector<double> a, vector<double> b) {
	OPERATOR("*");
	vector<vector<double> > result(a.size());
	for (int i = 0; i < (int) result.size(); ++i) {
		for (int j = 0; j < (int) b.size(); ++j) {
			result[i].push_back(a[i] * b[j]);
		}
	}
	return result;
}

vector<double> operator /(vector<double> a, double b) {
	OPERATOR("/");
	for (int i = 0; i < (int) a.size(); ++i) {
		a[i] /= b;
	}
	return a;
}

int sgn(double x) {
	FUNCTION("Sgn");
	if (x > 0) {
		return 1;
	} else if (x == 0) {
		return 0;
	} else {
		return -1;
	}
}

vector<vector<double> > Hessenberg(vector<vector<double> > A) {
	FUNCTION("Hessenberg");
	for (int r = 0; r < (int) A.size() - 2; ++r) {
		vector<double> ar(A.size(), 0);
		for (int i = 0; i < (int) A.size(); ++i) {
			ar[i] = A[i][r];
		}
		double c = 0;
		for (int i = r + 1; i < (int) A.size(); ++i) {
			c += pow(ar[i], 2);
		}
		c = sqrt(c);
		c = (-c * sgn(ar[r + 1]));
		double p = sqrt(2 * c * (c - ar[r + 1]));
		vector<double> u(A.size(), 0);
		for (int i = r + 1; i < (int) A.size(); ++i) {
			if (i == r + 1) {
				u[i] = (ar[i] - c) / p;
			} else {
				u[i] = ar[i] / p;
			}
		}
		vector<vector<double> > I;
		for (int i = 0; i < (int) A.size(); ++i) {
			vector<double> z(A.size(), 0);
			z[i] = 1;
			I.push_back(z);
		}
		vector<vector<double> > H = I - 2 * (u * u);
		bool s;
		A = H * A * Inverse(H, s);
	}
	return A;
}

vector<vector<double> > QR(vector<vector<double> > A,
		vector<vector<double> > &Q) {
	FUNCTION("QR");
	vector<vector<double> > I;
	for (int i = 0; i < (int) A.size(); ++i) {
		vector<double> z(A.size(), 0);
		z[i] = 1;
		I.push_back(z);
	}
	for (int i = 0; i < (int) A.size() - 1; ++i) {
		double theta = atan(A[i + 1][i] / A[i][i]);
		vector<vector<double> > P;
		for (int r = 0; r < (int) A.size(); ++r) {
			vector<double> z(A.size(), 0);
			z[r] = 1;
			P.push_back(z);
		}
		P[i][i + 1] = sin(theta);
		P[i][i] = cos(theta);
		P[i + 1][i + 1] = cos(theta);
		P[i + 1][i] = (-sin(theta));
		I = P * I;
		vector<double> aa(A[i]);
		vector<double> aa1(A[i + 1]);
		for (int j = i; j < (int) A.size(); ++j) {
			A[i][j] = aa[j] * cos(theta) + aa1[j] * sin(theta);
			A[i + 1][j] = (-aa[j] * sin(theta) + aa1[j] * cos(theta));
		}
	}
	bool s;
	Q = Inverse(I, s);
	return A;
}

double Delta(vector<vector<double> > a) {
	FUNCTION("Delta");
	double ss = 0;
	for (int i = 0; i < (int) a.size(); ++i) {
		for (int j = 0; j < (int) a[i].size(); ++j) {
			if (i != j) {
				if (fabs(a[i][j]) > ss) {
					ss = fabs(a[i][j]);
				}
			}
		}
	}
	return ss;
}

double Delta1(vector<vector<double> > A) {
	FUNCTION("Delta1");
	double d = 0;
	for (int i = 0; i < (int) A.size() - 1; ++i) {
		if (fabs(A[i][i + 1]) > d) {
			d = fabs(A[i][i + 1]);
		}
		if (fabs(A[i + 1][i]) > d) {
			d = fabs(A[i + 1][i]);
		}
	}
	return d;
}

vector<complex<double> > Namta(vector<vector<double> > A, double delta) {
	FUNCTION("Namta");
	double delta1 = 0;
	while ((Delta(A) >= delta) && (fabs(Delta1(A) - delta1) >= delta)) {
		delta1 = Delta1(A);
		A = Hessenberg(A);
		vector<vector<double> > Q;
		vector<vector<double> > R;
		R = QR(A, Q);
		A = R * Q;
	}
	vector<complex<double> > namta;
	if (Delta(A) < delta) {
		for (int i = 0; i < (int) A.size(); ++i) {
			complex<double> dd(A[i][i], 0);
			namta.push_back(dd);
		}
	} else {
		int r = 0;
		while (r < A.size() - 1) {
			if (fabs(A[r + 1][r]) >= delta) {
				double b = -(A[r][r] + A[r + 1][r + 1]);
				double c = (A[r][r] * A[r + 1][r + 1]
						- A[r][r + 1] * A[r + 1][r]);
				if (pow(b, 2) - 4 * c < 0) {
					complex<double> d1(-b / 2, sqrt(4 * c - pow(b, 2)) / 2);
					complex<double> d2(-b / 2, -sqrt(4 * c - pow(b, 2)) / 2);
					namta.push_back(d1);
					namta.push_back(d2);
				} else {
					complex<double> d1(-b / 2 + sqrt(pow(b, 2) - 4 * c) / 2, 0);
					complex<double> d2(-b / 2 - sqrt(pow(b, 2) - 4 * c) / 2, 0);
					namta.push_back(d1);
					namta.push_back(d2);
				}
				r += 2;
			} else {
				complex<double> d(A[r][r], 0);
				namta.push_back(d);
				++r;
			}
		}
		if (r == A.size() - 1) {
			complex<double> d(A[r][r], 0);
			namta.push_back(d);
		}
	}
	return namta;
}

void Print(vector<complex<double> > a) {
	FUNCTION("Print");
	for (int i = 0; i < (int) a.size(); ++i) {
		cout << "\t\t" << a[i] << endl;
	}
}

void Print(vector<double> a) {
	FUNCTION("Print");
	cout << "\t\t";
	for (int i = 0; i < (int) a.size(); ++i) {
		printf("%3.5f | ", a[i]);
	}
	cout << endl;
}

double Delta(vector<double> a, vector<double> b) {
	FUNCTION("Delta");
	double x = 0;
	for (int i = 0; i < (int) a.size(); ++i) {
		if (fabs(a[i] - b[i]) > x) {
			x = fabs(a[i] - b[i]);
		}
	}
	return x;
}

double Max(vector<double> a) {
	FUNCTION("Max");
	double x = 0;
	int n = 0;
	for (int i = 0; i < (int) a.size(); ++i) {
		if (fabs(a[i]) > x) {
			x = fabs(a[i]);
			n = i;
		}
	}
	return a[n];
}

vector<double> ComputeVector(vector<vector<double> > A, complex<double> namta,
		double delta) {
	FUNCTION("ComputeVector");
	vector<vector<double> > I;
	for (int i = 0; i < (int) A.size(); ++i) {
		vector<double> z(A.size(), 0);
		z[i] = 1;
		I.push_back(z);
	}
	A = A - namta.real() * I;
	bool d;
	A = Inverse(A, d);
	vector<double> z(A.size(), 1);
	vector<double> z1;
	do {
		z1 = z;
		vector<double> y = A * z;
		double m = Max(y);
		z = y / m;
	} while (Delta(z, z1) >= delta);
	return z;
}

void tranverse(double **a, double alpha, double beita, int n) {
	FUNCTION("tranverse");
	for (int i = 0; i < n; i++)
		for (int j = i + 1; j < n; j++) {
			double temp = *(a[i] + j);
			if (temp > 0) {
				if (temp == 1) {
					*(a[i] + j) = beita + (1 - beita) * (3 - 2 * alpha);
					*(a[j] + i) = beita + (1 - beita) / (3 - 2 * alpha);
				} else {
					*(a[i] + j) = beita * (temp - 2 + 2 * alpha) + (1 - beita) * (temp + 2 - 2 * alpha);
					*(a[j] + i) = beita / (temp - 2 + 2 * alpha) + (1 - beita) / (temp + 2 - 2 * alpha);
				}
			} else {
				temp = -temp;
				if (temp == 1) {
					*(a[i] + j) = beita + (1 - beita) / (3 - 2 * alpha);
					*(a[j] + i) = beita + (1 - beita) * (3 - 2 * alpha);
				} else {
					*(a[j] + i) = beita * (temp - 2 + 2 * alpha) + (1 - beita) * (temp + 2 - 2 * alpha);
					*(a[i] + j) = beita / (temp - 2 + 2 * alpha) + (1 - beita) / (temp + 2 - 2 * alpha);
				}
			}
		}
	for (int i = 0; i < n; i++)
		*(a[i] + i) = 1;

}

int checkConsistency(double maxnamta, int n) {
	FUNCTION("Check");
	double a[9] = { 0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45 };
	if (((maxnamta - n) / (n - 1)) / a[n - 1] < 0.1)
		return 1;
	else
		return 0;
}

int main() {
	L1("Preference Matrix definition");
	L2("Enter matrix size：");
	int n;
	cin >> n;

	L2("create matrix");
	double **a;
	a = new double*[n];
	for (int i = 0; i < n; i++) {
		a[i] = new double[n];
	}

	L2("intitialize matrix with user's values");
	for (int i = 0; i < n; i++) {
		for (int j = i + 1; j < n; j++) {
			cout << "\t\tline [" << i << "] column [" << j << "] Enter value ："<< endl;
			cin >> *(a[i] + j);
		}
	}

	L1("Define stop conditions");
	L2("enter alpha value：");
	double alpha;
	cin >> alpha;

	L2("enter beita value：");
	double beita;
	cin >> beita;

	L1("Tranverse matrix");
	tranverse(a, alpha, beita, n);

	L1("Create a one dimension vector from preference matrix");
	vector<vector<double> > A(n);
	for (int i = 0; i < n; i++) {
		cout << "\t\t";
		for (int j = 0; j < n; j++) {
			A[i].push_back(*(a[i] + j));
		}
		cout << endl;
	}

	L1("Show preference matrix");
	for (int i = 0; i < n; i++) {
		cout << "\t\t";
		for (int j = 0; j < n; j++) {
			if (i > j) {
				cout << BLUE;
			}
			if (i == j) {
				cout << CYAN;
			}
			printf("%7.5f | ", (*(a[i] + j)));
			cout << DEFAULT_COLOR;
		}
		cout << endl;
	}

	L2("Enter delta value:");
	double delta;
	cin >> delta;

	L1("Compute Namta");
	vector<complex<double> > namta = Namta(A, delta);
	L2("namta complex vector：");
	Print(namta);

	L2("search max real Namta value");
	double maxnamta;
	for (int i = 0; i < n; i++) {
		// found value is real AND bigger than current
		if ((namta[i].real() > maxnamta) && (namta[i].imag() == 0)) {
			maxnamta = namta[i].real();
		}
	}

	L2("max real Namta value：");
	cout << maxnamta << endl;

	L1("Check consistency");
	if (checkConsistency(maxnamta, n) == 1) {
		OK("", "一consistency check : Passed！");
	} else {
		NOK("", "一consistency check : Failed！");
	}

	L1("Preference Eigenvector");
	L2("compute");
	vector<double> ve = ComputeVector(A, maxnamta, delta);
	L2("show values");
	Print(ve);

	L1("Normalize vector");
	L2("define vector sum");
	double sum = 0;
	for (int i = 0; i < n; i++) {
		sum = sum + ve[i];
	}

	L2("normalized result：");
	vector<double> venorl = ve / sum;
	Print(venorl);

	L1("End");
	return 0;
}

