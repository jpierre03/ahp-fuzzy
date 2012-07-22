/** envalue.cpp : Defines the entry point for the console application.
 *  Creation : (date unknown) by (unknown)
 *             Source : http://www.codeforge.com/read/64380/normal.cpp__html
 *  Licence : Undefined, so considered as Public Domain
 */

#include <math.h>
#include <vector>
#include <complex>
#include <iostream>
#include <fstream>



using namespace std;

typedef vector<double> double_vector;

vector<vector<double> > Inverse(vector<vector<double> > a,bool &judge)
{
	vector<vector<double> > b(a);
	if(b.size()!=b[0].size())
	{
		judge=false;
		return b;
	}
	else
	{
		double temp=1;
		for(int k=0;k<(int)b.size();++k)
		{
			if(k<((int)b.size()-1))
			{
				int tk=k;
				double tem=fabs(b[k][k]);
				for(int i=k;i<(int)b.size();++i)
				{
					if(fabs(b[i][k])>tem)
					{
						tk=i;
						tem=fabs(b[i][k]);
					}
				}
				if(tk!=k)
				{
					swap(b[tk],b[k]);
				}
				if(b[k][k]==0)
				{
					temp=0;
				}
				else
				{
					for(int i=k+1;i<(int)b.size();++i)
					{
						tem=b[i][k];
						for(int j=k;j<(int)b[i].size();++j)
						{
							b[i][j]-=(b[k][j]*tem/b[k][k]);
						}
					}
				}
			}
		}
		if(temp!=0)
		{
			for(int i=0;i<(int)b.size();++i)
			{
				temp=temp*b[i][i];
			}
		}
		if(temp==0)
		{
			judge=false;
			return a;
		}
		else
		{
			vector<vector<double> > I;
			for(int i=0;i<(int)a.size();++i)
			{
				vector<double> z(a.size(),0);
				z[i]=1;
				I.push_back(z);
			}
			for(int k=0;k<(int)a.size();++k)
			{
				int tk=k;
				double tem=fabs(a[k][k]);
				for(int i=k;i<(int)a.size();++i)
				{
					if(fabs(a[i][k])>tem)
					{
						tk=i;
						tem=fabs(a[i][k]);
					}
				}
				if(tk!=k)
				{
					swap(a[tk],a[k]);
					swap(I[tk],I[k]);
				}
				tem=a[k][k];
				for(int i=0;i<(int)a[k].size();++i)
				{
					a[k][i]/=tem;
					I[k][i]/=tem;
				}
				for(int i=0;i<(int)a.size();++i)
				{
					if(i!=k)
					{
						double temp=a[i][k];
						for(int j=0;j<(int)a[i].size();++j)
						{
							a[i][j]-=(a[k][j]*temp);
							I[i][j]-=(I[k][j]*temp);
						}
					}
				}
			}
			judge=true;
			return I;
		}
	}
}

vector<vector<double> > operator - (vector<vector<double> > a,vector<vector<double> > b)
{
	bool dd=true;
	if(a.size()!=b.size())
	{
		dd=false;
	}
	else
	{
		for(int i=0;i<(int)a.size();++i)
		{
			if(a[i].size()!=b[i].size())
			{
				dd=false;
			}
			else
			{
				for(int j=0;j<(int)a.size();++j)
				{
					a[i][j]-=b[i][j];
				}
			}
		}
	}
	vector<vector<double> > ss;
	if(dd==false)
	{
		return ss;
	}
	else
	{
		ss=a;
		return ss;
	}
}

vector<vector<double> > operator * (double a,vector<vector<double> > b)
{
	for(int i=0;i<(int)b.size();++i)
		for(int j=0;j<(int)b[i].size();++j)
		{
			b[i][j]*=a;
		}
	return b;
}

vector<double> operator * (vector<vector<double> > a,vector<double> b)
{
	vector<double> c;
	for(int i=0;i<(int)a.size();++i)
	{
		double s=0;
		for(int j=0;j<(int)b.size();++j)
		{
			s+=a[i][j]*b[j];
		}
		c.push_back(s);
	}
	return c;
}

vector<vector<double> > operator * (vector<vector<double> > a,vector<vector<double> > b)
{
	if(a[0].size()!=b.size())
	{
		vector<vector<double> > ss;
		return ss;
	}
	else
	{
		vector<vector<double> > ss(a.size());
		for(int i=0;i<(int)ss.size();++i)
		{
			for(int j=0;j<(int)b[0].size();++j)
			{
				double temp=0;
				for(int k=0;k<(int)b.size();++k)
				{
					temp+=(a[i][k]*b[k][j]);
				}
				ss[i].push_back(temp);
			}
		}
		return ss;
	}
}

vector<vector<double> > operator * (vector<double> a,vector<double> b)
{
	vector<vector<double> > result(a.size());
	for(int i=0;i<(int)result.size();++i)
	{
		for(int j=0;j<(int)b.size();++j)
		{
			result[i].push_back(a[i]*b[j]);
		}
	}
	return result;
}

int sgn(double x)
{
	if(x>0)
	{
		return 1;
	}else if(x==0)
	{
		return 0;
	}else
	{
		return -1;
	}
}

vector<vector<double> > Hessenberg(vector<vector<double> > A)
{
	for(int r=0;r<(int)A.size()-2;++r)
	{
		vector<double> ar(A.size(),0);
		for(int i=0;i<(int)A.size();++i)
		{
			ar[i]=A[i][r];
		}
		double c=0;
		for(int i=r+1;i<(int)A.size();++i)
		{
			c+=pow(ar[i],2);
		}
		c=sqrt(c);
		c=(-c*sgn(ar[r+1]));
		double p=sqrt(2*c*(c-ar[r+1]));
		vector<double> u(A.size(),0);
		for(int i=r+1;i<(int)A.size();++i)
		{
			if(i==r+1)
			{
				u[i]=(ar[i]-c)/p;
			}
			else
			{
				u[i]=ar[i]/p;
			}
		}
		vector<vector<double> > I;
		for(int i=0;i<(int)A.size();++i)
		{
			vector<double> z(A.size(),0);
			z[i]=1;
			I.push_back(z);
		}
		vector<vector<double> > H=I-2*(u*u);
		bool s;
		A=H*A*Inverse(H,s);
	}
	return A;
}

vector<vector<double> > QR(vector<vector<double> > A,vector<vector<double> > &Q)
{
	vector<vector<double> > I;
	for(int i=0;i<(int)A.size();++i)
	{
		vector<double> z(A.size(),0);
		z[i]=1;
		I.push_back(z);
	} 
	for(int i=0;i<(int)A.size()-1;++i)
	{
		double theta=atan(A[i+1][i]/A[i][i]);
		vector<vector<double> > P;
		for(int r=0;r<(int)A.size();++r)
		{
			vector<double> z(A.size(),0);
			z[r]=1;
			P.push_back(z);
		}
		P[i][i+1]=sin(theta);
		P[i][i]=cos(theta);
		P[i+1][i+1]=cos(theta);
		P[i+1][i]=(-sin(theta));
		I=P*I;
		vector<double> aa(A[i]);
		vector<double> aa1(A[i+1]);
		for(int j=i;j<(int)A.size();++j)
		{
			A[i][j]=aa[j]*cos(theta)+aa1[j]*sin(theta);
			A[i+1][j]=(-aa[j]*sin(theta)+aa1[j]*cos(theta));
		}
	}
	bool s;
	Q=Inverse(I,s);
	return A;
}


double Delta(vector<vector<double> > a)
{
	double ss=0;
	for(int i=0;i<(int)a.size();++i)
	{
		for(int j=0;j<(int)a[i].size();++j)
		{
			if(i!=j)
			{
				if(fabs(a[i][j])>ss)
				{
					ss=fabs(a[i][j]);
				}
			}
		}
	}
	return ss;
}

double Delta1(vector<vector<double> > A)
{
	double d=0;
	for(int i=0;i<(int)A.size()-1;++i)
	{
		if(fabs(A[i][i+1])>d)
		{
			d=fabs(A[i][i+1]);
		}
		if(fabs(A[i+1][i])>d)
		{
			d=fabs(A[i+1][i]);
		}
	}
	return d;
}

vector<complex<double> > Namta(vector<vector<double> > A,double delta)
{
	double delta1=0;
	while((Delta(A)>=delta)&&(fabs(Delta1(A)-delta1)>=delta))
	{
		delta1=Delta1(A);
		A=Hessenberg(A);
		vector<vector<double> > Q;
		vector<vector<double> > R;
		R=QR(A,Q);
		A=R*Q;
	}
	vector<complex<double> > namta;
	if(Delta(A)<delta)
	{
		for(int i=0;i<(int)A.size();++i)
		{
			complex<double> dd(A[i][i],0);
			namta.push_back(dd);
		}
	}
	else
	{
		int r=0;
		while(r<A.size()-1)
		{
			if(fabs(A[r+1][r])>=delta)
			{
				double b=-(A[r][r]+A[r+1][r+1]);
				double c=(A[r][r]*A[r+1][r+1]-A[r][r+1]*A[r+1][r]);
				if(pow(b,2)-4*c<0)
				{
					complex<double> d1(-b/2,sqrt(4*c-pow(b,2))/2);
					complex<double> d2(-b/2,-sqrt(4*c-pow(b,2))/2);
					namta.push_back(d1);
					namta.push_back(d2);
				}
				else
				{
					complex<double> d1(-b/2+sqrt(pow(b,2)-4*c)/2,0);
					complex<double> d2(-b/2-sqrt(pow(b,2)-4*c)/2,0);
					namta.push_back(d1);
					namta.push_back(d2);
				}
				r+=2;
			}
			else
			{
				complex<double> d(A[r][r],0);
				namta.push_back(d);
				++r;
			}
		}
		if(r==A.size()-1)
		{
			complex<double> d(A[r][r],0);
			namta.push_back(d);
		}
	}
	return namta;
} 

void Print(vector<complex<double> > a)
{
	for(int i=0;i<(int)a.size();++i)
	{
		cout<<a[i]<<endl;
	}
}

void Print(vector<double> a)
{
	for(int i=0;i<(int)a.size();++i)
	{
		cout<<a[i]<<"\t|\t";
	}
	cout<<endl;
}

double Delta(vector<double> a,vector<double> b)
{
	double x=0;
	for(int i=0;i<(int)a.size();++i)
	{
		if(fabs(a[i]-b[i])>x)
		{
			x=fabs(a[i]-b[i]);
		}
	}
	return x;
}

double Max(vector<double> a)
{
	double x=0;
	int n=0;
	for(int i=0;i<(int)a.size();++i)
	{
		if(fabs(a[i])>x)
		{
			x=fabs(a[i]);
			n=i;
		}
	}
	return a[n];
}

vector<double> operator / (vector<double> a,double b)
{
	for(int i=0;i<(int)a.size();++i)
	{
		a[i]/=b;
	}
	return a;
}

vector<double> ComputeVector(vector<vector<double> > A,complex<double> namta,double delta)
{
	vector<vector<double> > I;
	for(int i=0;i<(int)A.size();++i)
	{
		vector<double> z(A.size(),0);
		z[i]=1;
		I.push_back(z);
	}
	A=A-namta.real()*I;
	bool d;
	A=Inverse(A,d);
	vector<double> z(A.size(),1);
	vector<double> z1;
	do
	{
		z1=z;
		vector<double> y=A*z;
		double m=Max(y);
		z=y/m;
	}while(Delta(z,z1)>=delta);
	return z;
}


void tranverse(double **a, double alpha, double beita, int n)
{
	for(int i=0;i<n;i++)   
		for(int j=i+1;j<n;j++)
		{
			double temp=*(a[i]+j);
			if (temp>0)
			{
				if(temp==1)
				{
					*(a[i]+j)=beita+(1-beita)*(3-2*alpha);
					*(a[j]+i)=beita+(1-beita)/(3-2*alpha);
				}
				else
				{
					*(a[i]+j)=beita*(temp-2+2*alpha)+(1-beita)*(temp+2-2*alpha);
					*(a[j]+i)=beita/(temp-2+2*alpha)+(1-beita)/(temp+2-2*alpha);
				}
			}
			else
			{
				temp=-temp;
				if(temp==1)
				{
					*(a[i]+j)=beita+(1-beita)/(3-2*alpha);
					*(a[j]+i)=beita+(1-beita)*(3-2*alpha);
				}
				else
				{
					*(a[j]+i)=beita*(temp-2+2*alpha)+(1-beita)*(temp+2-2*alpha);
					*(a[i]+j)=beita/(temp-2+2*alpha)+(1-beita)/(temp+2-2*alpha);
				}
			}   
		}
	for(int i=0;i<n;i++)   
		*(a[i]+i)=1;

}

int check(double maxnamta, int n)
{
	double a[9]={0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45};
	if (((maxnamta-n)/(n-1))/a[n-1]<0.1)
		return 1;
	else 
		return 0;
}


int main()
{
	cout<<"Preference Matrix definition"<<endl;
	cout<<"\tEnter matrix size："<<endl;
	int n;
	cin>>n;

	cout<<"-- Create matrix"<<endl;
	double   **a;
	a=new double*[n]; 

	for(int i=0;i<n;i++) 
	{
		a[i]=new double[n]; 
	} 

	cout<<"-- Intitialize matrix with user's values"<<endl;
	for(int i=0;i<n;i++)   
		for(int j=i+1;j<n;j++)
		{
			cout<<"\tline ["<<i<<"] column ["<<j<<"] Enter value ："<<endl;
			cin>>*(a[i]+j);   
		}

	cout<<"-- Define stop conditions"<<endl;
	cout<<"\tEnter alpha value："<<endl;
	double alpha;
	cin>>alpha;

	cout<<"\tEnter beita value："<<endl;
	double beita;
	cin>>beita;

	cout<<"-- Tranverse matrix"<<endl;
	tranverse(a,alpha,beita,n);

	cout<<"-- XXXX"<<endl;
	vector<vector<double> >A(n);
	for(int i=0;i<n;i++) 
		for(int j=0;j<n;j++)
		{
			A[i].push_back(*(a[i]+j));   
		}

	cout<<"Enter delta value:"<<endl;
	double delta;
	cin>>delta;

	cout<<"-- File name definition"<<endl;
	string s("namta.txt");

	vector < complex<double> > namta=Namta(A,delta);
	cout<<"特征值："<<endl; 
	Print(namta);

	double maxnamta;
	for(int i=0;i<n;i++)  
	{
		if ((namta[i].real()>maxnamta)&&(namta[i].imag()==0))
			maxnamta=namta[i].real();
	}

	cout<<"最大特征值："; 
	cout<<maxnamta<<endl;



	if (check(maxnamta,n)==1)
		cout<<"一致性检查通过！"<<endl;
	else
		cout<<"一致性检查未通过！"<<endl;

	cout<<maxnamta<<"所对应的特征向量："<<endl;

	vector<double> ve=ComputeVector(A,maxnamta,delta);
	Print(ve);

	cout<<"-- Normalize vector"<<endl;
	cout<<"\tdefine vector sum"<<endl;
	double sum=0;
	for(int i=0;i<n;i++)  
	{
		sum=sum+ve[i];
	}

	vector<double> venorl=ve/sum;
	cout<<maxnamta<<"\tNormalized result："<<endl;
	Print(venorl);
	return 0;
}

