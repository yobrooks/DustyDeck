#include <iostream>
#include <math.h>
#include <iomanip>
#include "walltime.cc"
#include "cputime.cc"

using namespace std;
const int MAXDIM = 25;

/*Parameters*/
double conrand(double &seed);
double trig(int i, int j);
void idcheck(int N, double &check, double AV[], double BV[], double ID[MAXDIM][MAXDIM]);

int main(){
    /* Variable declarations*/
    int IA[MAXDIM]; int N;
    double AV[MAXDIM], BV[MAXDIM], CV[MAXDIM];
    double OP[MAXDIM][MAXDIM], ID[MAXDIM][MAXDIM];
    double AM[MAXDIM][MAXDIM], BM[MAXDIM][MAXDIM];
    double CM[MAXDIM][MAXDIM], DM[MAXDIM][MAXDIM];
    double BOT, TOP, HOLDA, HOLDB;
    double TRACE3 = 0.0; int ival = 0; float sum = 0.0; double check=0.0;
    /*For the timing library*/
    double wall, cpu, seed;
    N = MAXDIM; seed = 1.0;
    wall = walltime_();
    cpu = cputime_();
    /*Loop 10*/
	
    /*#ifndef MO1
    int conrandV = conrand(seed);	
    for(int i = 0; i<N; i++){
        AV[i] = jn(0,(double)(conrandV*pow(-1,(((int)(10*conrandV)%N)))));
	BV[i] = jn(1,(double)(conrandV*pow(-1,(((int)(10*conrandV))%N))));
    }

    #else*/
    for(int i = 0; i<N; i++){
        AV[i] = jn(0,(double)(conrand(seed)*pow((-1), (((int)(10*conrand(seed)))%N))));
    }

    /*Loop 11*/
    for(int i = 0; i<N; i++){
        BV[i] = jn(1,(double)(conrand(seed)*pow((-1), (((int)(10*conrand(seed)))%N))));
    }
    //#endif
    /*Loop 12*/
    for(int i = 0; i<N; i++){
        ival = N;
        check = check+(AV[i]*BV[i]);
        idcheck(ival, check, AV, BV, ID);
    }

    /*Loop 13*/
    for(int i = 0; i<N; i++){
        /*Loop 14*/
        for(int j = 0; j<N; j++){
            idcheck(N, check, AV, BV, ID);
            if(check > 0.5)
            {
                OP[i][j] = AV[i]*BV[j]/BV[i];
            }
            else{
                OP[i][j] = AV[j]*BV[i]/BV[j];
            }
        }
        IA[i] = i;
    }

    /*Loop 15*/
    for(int i = 1;i<=N;i++){
        /*Loop 16*/
        for(int j = 0; j<=i; j+=8){
            IA[i-1] = ((i+j)%N)%N;
        }
    }

    /*Loop 20*/
    for(int i = 0; i<N; i++){
        idcheck(N, check, AV, BV, ID);
        CV[IA[i]] = (AV[IA[i]]+BV[IA[i]])/check;
    }

    /*Loop 30*/
    for(int i = 1; i<N; i++){
        idcheck(N, check, AV, BV, ID);
        AV[i] = AV[i-1]*BV[i]+CV[i];
    }

    /*Loop 40*/
    for(int i = 0; i<N; i++){
        idcheck(N, check, AV, BV, ID);
        /*Loop 45*/
        for(int j = 0; j<N; j++){
            if(check > 0.5){
                BOT = OP[i][j];
                TOP = AV[j]*BV[j];
                HOLDA = AV[j];
                AV[j] = BV[j]+CV[j]/(TOP-BOT)*ID[i][i];  
                BV[j] = HOLDA+CV[j]/(TOP-BOT)*ID[j][j];
                AM[i][j] = AV[j]*trig(IA[i]+1, IA[j]+1); 
                BM[i][j] = BV[j]*trig(IA[j]+1, IA[i]+1);
             }else {
                BOT = OP[i][j];
                TOP = AV[j]*BV[j];
                HOLDA = AV[j];
                AV[j] = BV[j]-CV[j]/(TOP-BOT)*ID[j][j];
                BV[j] = HOLDA-CV[j]/(TOP-BOT)*ID[i][i];
                AM[i][j] = AV[j]/trig(IA[i]+1, IA[j]+1); 
                BM[i][j] = BV[j]/trig(IA[j]+1, IA[i]+1);
            }
        }
    }

    /*Loop 50*/
    for(int i = 0; i<N; i++){
        /*Loop 52*/
        for(int j = 0; j<N; j++){
            CM[i][j] = 0.0;
            /*Loop 55*/
            for(int k = 0; k<N; k++){
                if(i<j){
                    CM[i][j] = CM[i][j]-AM[i][k]*BM[k][j]/check; 
                } else { 
                    CM[i][j] = CM[i][j]+AM[i][k]*BM[k][j]/check;
                }
            }  
        }
    }

    /*Loop 60*/ 
    for(int i = 0; i<N; i++){
        /*Loop 61*/
        for(int j = 0; j<N; j++){
            sum = 0.0; 
            /*Loop 62*/
            for(int k = 0; k<N; k++){
                sum =sum+(CM[i][k]*AM[j][k]);
            }
            DM[i][j] = sum;
        }
    }

    /*Loop 63*/
    for(int i = 0; i<N; i++){
        /*Loop 64*/
        for(int j = 0; j<N; j++){
            CM[i][j] = DM[i][j];
        }
    }

    /*Loop 70*/
    for(int i = 0; i<N; i++){
        /*Loop 71*/
        for(int j = 0; j<N; j++){
            sum = 0.0; 
            /*Loop 72*/
            for(int k = 0; k<N; k++){
                sum = sum-CM[i][k]*BM[j][k];
            }
            DM[i][j] = sum; 
        }
    }

    HOLDA = fabs(AM[0][0]);
    HOLDB = fabs(BM[0][0]);
    /*Loop 73*/
    for(int i = 0; i<N; i++){
        /*Loop 74*/
        for(int j = 0; j<N; j++){
            HOLDA = max(HOLDA, fabs(AM[i][j]));
            HOLDB = max(HOLDB, fabs(BM[i][j]));
        }
    }
 
    /*Loop 80*/
    for(int i = 0; i<N; i++){
        TRACE3 = TRACE3 + (AM[IA[i]][IA[i]]+BM[IA[i]][IA[i]]-DM[IA[i]][IA[i]])/(HOLDA*HOLDB);
    }

    /*DONE!*/
   cpu = cputime_() - cpu;
   wall = walltime_() - wall;
   cout << "Final Trace: " << setprecision(17) << TRACE3 << " and IDCheck: " << check << endl;;
   cout << "Runtime: " << setprecision(17) << cpu << " seconds" << endl;
}


double conrand(double &seed){
    double a, m, temp;

    a = 16807.00;
    m = 2147483647.00;

    temp = a*seed;
    seed = temp - m*(int)(temp/m);
    return (seed/m);
}

double trig(int i, int j){
    double x, y, z, trig;
    float pi = acosf(-1.0);
    x = double(i) - double(j);
    y = double(i) + double(j);
    z = exp(sin(sqrt(pow(x,2)+pow(y,2))*pi));
    trig = x+y+log10(fabs(1+z+(x*y*z)))/(fabs(x)+fabs(y));
    return trig;
}

void idcheck(int N, double &check, double AV[], double BV[], double ID[MAXDIM][MAXDIM]){
    double l2 = float(0.0);
    double check2;
    double a = 0.0; double b = 0.0; double c = 0.0; double d = 0.0;

  /*Loop 10*/
    for(int i = 0; i<N; i++){
        /*Loop 20*/
        for(int j = 0; j<N; j++){
            if(i == j){
                if((AV[i]<0) && (BV[j]<0)){
                    ID[i][j] = 1.0;
                }
                else if((AV[i]<0) && (BV[j] >0)){
                    ID[i][j] = -1.0;
                } 
                else if((AV[i]>0) && (BV[j]<0)){
                    ID[i][j] = -1.0;
                } 
                else{
                    ID[i][j] = 1.0;
                }
            } 
            else if(i!=j){
                ID[i][j] = cos(check+2*(i+1)*acosf(-1.0)/N)+2*sin(check+2*(j+1)*acosf(-1.0)/N);
            }
       }
    }

  /*Loop 30*/
    for(int i = 0; i<N; i++){
        l2 = l2+pow(AV[i],2);
    }

  /*Loop 40*/
    l2 = sqrt(l2);
    for(int i = 0; i<N; i++){
        AV[i] = AV[i]/l2;
    }

  /*Loop 50*/
    l2 = 0.0;
    for(int i = 0; i<N; i++){
        l2 = l2+(pow(BV[i],2));
    }

  /*Loop 60*/
    l2 = sqrt(l2);
    for(int i = 0; i<N; i++){
        BV[i] = BV[i]/l2;
    }

  /*Loop 70 */
    int goTo = 0;
    for(int i = 0; i<N; i++){
        /*Loop 80*/
        for(int j = 0; j<N; j++){
            /*Loop 90*/
            for(int k = 0; k<N; k++){
                goTo = (int)(((i+j+k+3)%4)+1);
                switch(goTo){
                /*goto 200*/
                    case 1:
                        a = a+AV[i]*BV[j]*ID[j][k];
                        check = check + a;
                        break;
                /*goto 300*/
                    case 2:
                        b = b+AV[j]*BV[i]*ID[k][j];
                        check = check - b;
                        break;
                /*goto 400*/
                    case 3:
                        c = c-AV[i]*BV[j]*ID[k][j];
                        check = sqrt(pow(b,2)+pow(c,2));
                        break;
                /*goto 500*/
                    case 4:
                        d = d-AV[j]*BV[i]*ID[j][k];
                        check2 = a+b+c+d;
                        break;
                }
            }
        }
    }
    check = min(fabs(check2), fabs(check))/max(fabs(check2), fabs(check));
}
