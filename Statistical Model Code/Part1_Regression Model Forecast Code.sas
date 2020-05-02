libname x "\\Client\C$\Users\jaminiyang\Desktop\Part1";

/****Aggregate Price Response Models****/
/*If we aggregate over UPCs, we have to use a volumetric measure*/
/*Import data*/
proc import out=x.division 
  datafile= "\\Client\C$\Users\jaminiyang\Desktop\Part1\model prep.xlsx" 
  DBMS=xlsx 
  replace;
run;
/*Create a temporary dataset to work with*/
data temp;
  set x.division;
run;
/*Sort the dataset */
proc sort data=temp;
  by Division FISCALWEEK;
run;


/*Create a new temporary dataset*/
data temp1;
  set temp;
  ppu=dollar_sales/units;
  lnunits=log(units);
  lnppu=log(ppu);
run;
proc print data=temp1;
run;
/*Summarize the data using proc means*/
proc means data=temp1 noprint;
	by division;
  output out=temp1_mean mean= /autoname;
run;
data temp1_mean(drop=_type_);
  set temp1_mean;
run;
proc print data=temp1_mean; 
run;


/*Are the promotional variables multicollinear?*/
proc corr data=temp1;
	by division;
  var COUPONUUSE ppu;
run;
/*Determine the best model for prediction based on AIC and SBC; use selection=rsquare
  for model selection*/
proc reg data=temp1 outest=parameters;
  by division;
  model lnunits=ppu COUPONUUSE / selection=rsquare best=1 vif aic sbc sse;
  model lnunits=lnppu COUPONUUSE / selection=rsquare best=1 vif aic sbc sse;
run;
proc print data=parameters;
run;
/*Now determine which model is best for prediction on the basis of AIC and SBC*/
data parameters;
  set parameters;
  if COUPONUUSE=. then delete;
run;
proc means data=parameters noprint;
  var _AIC_ _SBC_;
  by division;
  output out=mins min=minaic minsbc;
run;
data mins(drop=_type_);
  set mins;
run;
proc print data=mins;
run;

proc sql noprint;
create table pars as
select * from parameters p
join mins m
on p.division=m.division;
quit;
proc print data=pars;
run;

data pars_best;
  set pars;
  if _aic_^=minaic and _sbc_^=minsbc then delete;
run;
proc print data=pars_best;
run;

data pars_best;
  set pars_best;
  if (division=4 and _IN_=1) then delete;
run;
proc print data=pars_best;
run;

proc sql noprint;
create table pars_w_mean as
select * from pars_best p
join temp1_mean m
on p.division=m.division;
quit;
proc print data=pars_w_mean;
run;

/*Predict unit sales with and without promotional variables; use these predictions to compute 
  sales lifts*/
data predict1234789;
  set pars_w_mean;
  baseline_lnunits=intercept+COUPONUUSE*COUPONUUSE_Mean+lnppu*lnppu_Mean;
  baseline_units=exp(baseline_lnunits+.5*_SSE_/_EDF_);
  coupon_no=exp(baseline_lnunits-COUPONUUSE*COUPONUUSE_Mean+.5*_SSE_/_EDF_);
  coupon_yes=exp(baseline_lnunits+COUPONUUSE*(1-COUPONUUSE_Mean)+.5*_SSE_/_EDF_);
  sales_lift=(coupon_yes-coupon_no)/coupon_no;
run;
proc print data=predict1234789;
run;

data predict136;
  set pars_w_mean;
  baseline_lnunits=intercept+lnppu*lnppu_Mean;
  baseline_units=exp(baseline_lnunits+.5*_SSE_/_EDF_);
run;
proc print data=predict136;
run;

data predict4;
  set pars_w_mean;
  baseline_lnunits=intercept+deal*deal_Mean;
  baseline_units=exp(baseline_lnunits+.5*_SSE_/_EDF_);
  deal_no=exp(baseline_lnunits-deal*deal_Mean+.5*_SSE_/_EDF_);
  deal_yes=exp(baseline_lnunits+deal*(1-deal_Mean)+.5*_SSE_/_EDF_);
  deal_lift=(deal_yes-deal_no)/deal_no;
run;
proc print data=predict4;
run;

data predict5;
  set pars_w_mean;
  baseline_lnunits=intercept+ppu*ppu_Mean;
  baseline_units=exp(baseline_lnunits+.5*_SSE_/_EDF_);
run;
proc print data=predict5;
run;

data predict89;
  set pars_w_mean;
  baseline_lnunits=intercept+ppu*ppu_Mean+deal*deal_Mean;
  baseline_units=exp(baseline_lnunits+.5*_SSE_/_EDF_);
  deal_no=exp(baseline_lnunits-deal*deal_Mean+.5*_SSE_/_EDF_);
  deal_yes=exp(baseline_lnunits+deal*(1-deal_Mean)+.5*_SSE_/_EDF_);
  deal_lift=(deal_yes-deal_no)/deal_no;
run;
proc print data=predict89;
run;
