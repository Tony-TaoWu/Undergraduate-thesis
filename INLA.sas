libname MCMC "C:\Users\lenovo\Desktop";
/* import data */
proc import out = MCMC.data
/*Datafile = "C:\Users\lenovo\Desktop" dbms = excel*/
/*Datafile = "C:\Users\lenovo\Desktop" dbms = excel*/
Datafile = "C:\Users\lenovo\Desktop" dbms = excel
replace;
Getnames = YES;
run;quit;

/*options validvarname=any;
proc format;
   value sizeUR 1 = 'one' 2 = 'two' 3 = 'three' 4 = 'four' 5 = 'five';
run;
data MCMC.data2;
   set MCMC.data;
   format UR sizeUR.;
run;quit;
proc print data = MCMC.data2;run;quit; */

/* View data */
proc print data = MCMC.data;run;quit;
proc contents varnum data = MCMC.data;
    ods select position;
run;quit;
/* ods word */
ods rtf file = "C:\Users\lenovo\Desktop";
ods graphics on;
/* MCMC */
	/* NBI 是计算参数结果时去掉开头的迭代数 PLOTS是画出所有图像 */
    /* STATISTICS是计算参数所有统计量值 SEED是随机种子 NMC 是迭代次数*/
	/*ods select PostSumInt ess;*/
	/* 模型 logit(p)=beta0 + beta1x1 +...+ beta10x10 */
    /* beta0 1 beta1 1 beta2 1 beta3 1 beta4 1 beta5 1 beta6 1 beta7 1 beta8 1 beta9 1 beat10 1*/
	/* mu = beta0 + beta1*GRE + beta2*TOEFL + beta3*UR2 + beta4*UR3 + beta5*UR4 + beta6*UR5 + beta7*SOP +
                 beta8*LOR + beta9*CGPA + beta10*RE; */
    /* mu = beta0 + beta1*GRE + beta2*TOEFL + beta3*UR + beta4*SOP + beta5*LOR + beta6*CGPA + beta7*RE;*/
    /* random beta3 ~ normal(0,var = 10000) subject = UR monitor = (beta3); */ 
proc MCMC data = MCMC.data  nbi = 40000 nmc = 100000 seed = 2020 DIAG = ALL DIC
            PLOTS = ALL STATISTICS = ALL propcov = quanew outpost = MCMC.out;
	parms beta0-beta7;
	prior beta: ~ normal(0,var=10000);
	mu = beta0 + beta1*GRE + beta2*TOEFL + beta3*UR + beta4*SOP + beta5*LOR + beta6*CGPA + beta7*RE;
    p = exp(mu)/(1+exp(mu));
    model COA ~ bern(p);
run;quit;
ods graphics off;

/*                  正文到此结束 下面为测试代码                  */

ods graphics on;
/* MCMC */
proc MCMC data = MCMC.data  nbi = 40000 nmc = 100000 seed = 2020 DIAG = ALL DIC
            PLOTS = ALL STATISTICS = ALL propcov = quanew outpost = MCMC.out;
	parms beta0-beta7;
	prior beta0 ~ unif(-10,10);
	prior beta1-beta5 ~ unif(-10,10);
	prior beta6 ~ unif(0,10);
	prior beta7 ~ unif(-5,5);
	/*prior beta0 ~ unif(-10,10);
	prior beta0 ~ unif(-10,10);
	prior beta0 ~ unif(-10,10);*/
	mu = beta0 + beta1*GRE + beta2*TOEFL + beta3*UR + beta4*SOP + beta5*LOR + beta6*CGPA + beta7*RE;
    p = exp(mu)/(1+exp(mu));
    model COA ~ bern(p);
run;quit;
ods graphics off;



/* 例子 */
title 'Implement a New Sampling Algorithm';
data inputdata;
   input remiss cell smear infil li blast temp;
   ind = _n_;
   cnst = 1;
   label remiss='Complete Remission';
   datalines;
   1  0.8   0.83  0.66  1.9  1.1    0.996
   1  0.9   0.36  0.32  1.4  0.74   0.992
   0  0.8   0.88  0.7   0.8  0.176  0.982
   0  1     0.87  0.87  0.7  1.053  0.986
   1  0.9   0.75  0.68  1.3  0.519  0.98
   0  1     0.65  0.65  0.6  0.519  0.982
   1  0.95  0.97  0.92  1    1.23   0.992
   0  0.95  0.87  0.83  1.9  1.354  1.02
   0  1     0.45  0.45  0.8  0.322  0.999
   0  0.95  0.36  0.34  0.5  0      1.038
   0  0.85  0.39  0.33  0.7  0.279  0.988
   0  0.7   0.76  0.53  1.2  0.146  0.982
   0  0.8   0.46  0.37  0.4  0.38   1.006
   0  0.2   0.39  0.08  0.8  0.114  0.99
   0  1     0.9   0.9   1.1  1.037  0.99
   1  1     0.84  0.84  1.9  2.064  1.02
   0  0.65  0.42  0.27  0.5  0.114  1.014
   0  1     0.75  0.75  1    1.322  1.004
   0  0.5   0.44  0.22  0.6  0.114  0.99
   1  1     0.63  0.63  1.1  1.072  0.986
   0  1     0.33  0.33  0.4  0.176  1.01
   0  0.9   0.93  0.84  0.6  1.591  1.02
   1  1     0.58  0.58  1    0.531  1.002
   0  0.95  0.32  0.3   1.6  0.886  0.988
   1  1     0.6   0.6   1.7  0.964  0.99
   1  1     0.69  0.69  0.9  0.398  0.986
   0  1     0.73  0.73  0.7  0.398  0.986
;

proc mcmc data=inputdata nmc=100000 propcov=quanew seed=17
          outpost=mcmcout;
   ods select PostSumInt ess;
   parms beta0-beta6;
   prior beta: ~ normal(0,var=25);
   mu = beta0 + beta1*cell + beta2*smear +
         beta3*infil +  beta4*li + beta5*blast +  beta6*temp;
   p = cdf('normal', mu, 0, 1);
   model remiss ~ bern(p);
run;
/* 例子结束 */


proc fcmp outlib=sasuser.funcs.uds;
/*----------------------------------------------
      Generate left-truncated normal variate   
----------------------------------------------*/
   function rltnorm(mu,sig,lwr);
   if lwr<mu then do;
      ans = lwr-1;
      do while(ans<lwr);
         ans = rand('normal',mu,sig);
      end;
   end;
   else do;
      mul = (lwr-mu)/sig;
      alpha = (mul + sqrt(mul**2 + 4))/2;
      accept=0;
      do while(accept=0);
         z = mul + rand('exponential')/alpha;
         lrho = -(z-alpha)**2/2;
         u = rand('uniform');
         lu = log(u);
         if lu <= lrho then accept=1;
      end;
      ans = sig*z + mu;
   end;
   return(ans);
   endsub;

/*-----------------------------------------------
      Generate right-truncated normal variate   
-----------------------------------------------*/
   function rrtnorm(mu,sig,uppr);
   ans = 2*mu - rltnorm(mu,sig, 2*mu-uppr);
   return(ans);
   endsub;
run;

/* define the HH algorithm in PROC FCMP. */
%let n = 500;
%let p = 8;
options cmplib=sasuser.funcs;
proc fcmp outlib=sasuser.funcs.uds;
   subroutine HH(beta[*],Z[*],B[*],x[*,*],y[*],W[*],sQ[*],S[*,*],L[*,*]);
   outargs beta, Z, B;
   array T[&p] / nosym;
   do j=1 to &n;
      zold = Z[j];
      m = 0;
      do k = 1 to &p;
         m = m + X[j,k] * B[k];
      end;
      m = m - W[j]*(Z[j]-m);
      if (y[j]=1) then
         Z[j] = rltnorm(m,sQ[j],0);
      else
         Z[j] = rrtnorm(m,sQ[j],0);
      diff = Z[j] - zold;
      do k = 1 to &p;
         B[k] = B[k] + diff * S[k,j];
      end;
   end;
   do j=1 to &p;
      T[j] = rand('normal');
   end;
   call mult(L,T,T);
   call addmatrix(B,T,beta);
   endsub;
run;

options cmplib=sasuser.funcs;


proc mcmc data=inputdata nmc=5000 monitor=(beta) outpost=hhout;
   ods select PostSumInt ess;
   array xtx[&p,&p];      /* work space                         */
   array xt[&p,&n];       /* work space                         */
   array v[&p,&p];        /* work space                         */
   array HatMat[&n,&n];   /* work space                         */
   array S[&p,&n];        /* V * Xt                             */
   array W[&n];
   array y[1]/ nosymbols; /* y stores the response variable     */
   array x[1]/ nosymbols; /* x stores the explanatory variables */
   array sQ[&n];          /* sqrt of the diagonal elements of Q */
   array B[&p];           /* conditional mean of beta           */
   array L[&p,&p];        /* Cholesky decomp of conditional cov */
   array Z[&n];           /* latent variables Z                 */
   array beta[&p] beta0-beta7;   /* regression coefficients     */

   begincnst;
      call streaminit(2020);
      if ind=1 then do;
         rc = read_array("inputdata", x, "cnst", "cell", "smear", "infil",
                         "li", "blast", "temp");
         rc = read_array("inputdata", y, "remiss");
         call identity(v);
         call mult(v, 25, v);
         call transpose(x,xt);
         call mult(xt,x,xtx);
         call inv(v,v);
         call addmatrix(xtx,v,xtx);
         call inv(xtx,v);
         call chol(v,L);
         call mult(v,xt,S);
         call mult(x,S,HatMat);
         do j=1 to &n;
            H = HatMat[j,j];
            W[j] = H/(1-H);
            sQ[j] = sqrt(W[j] + 1);
         end;

         do j=1 to &n;
            if(y[j]=1) then
                Z[j] = rltnorm(0,1,0);
            else
                Z[j] = rrtnorm(0,1,0);
         end;
         call mult(S,Z,B);
      end;
   endcnst;
   /* 用uds添加抽样函数 */
   uds HH(beta,Z,B,x,y,W,sQ,S,L);
   parms z: beta: 0 B1-B7 / uds;
   prior z: beta: B1-B7 ~ general(0);

   model general(0);
run;

		         
