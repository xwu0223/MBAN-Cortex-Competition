/*define categorical variables and continuous variables */

%LET interval_pred =	
                Age
			    Salary	
				TotalGift
				MinGift
				MaxGift
				AmtLastYear
				Referrals;

%LET categorical_pred = 
				Woman
				GaveLastYear
				Recency
				Seniority
				SeniorList
				NbActivities
				Frequency;

/* logistic regression model */


/* Import the data and fill in the missing value */
data Cortex_data;
	set cortex.hist2;	
	/* Replace the missing value with 0 and NA */
	
	ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = . 
		THEN Column_Name = 0;
	END;
	
	
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logMaxGift=log(Maxgift + 1);
	logMinGift=log(Mingift + 1);
	logAmtLastYear=log(AmtLastYear+1);
	
	if Seniority=. and gavelastyear=1 Then Seniority=0;
	if Seniority=. and gavelastyear=0 Then Seniority=99;
	if Recency=. and gavelastyear=1 Then Recency=0;
	if Recency=. and gavelastyear=0 Then Recency=99;
run;

/* Set relevant variables */
%let intervals = Age logSalary LogAmtLastYear logTotalGift logReferrals Frequency logMaxGift logMinGift;

/* Data splitting into train and valid */
proc sort data=Cortex_data out=Cortex_data_sorted;
	by GaveThisYear;
run;


proc surveyselect noprint data=Cortex_data_sorted seed=44444 samprate=.7
	stratumseed=restore outall out=sample;
	strata GaveThisYear;
run;

proc freq data=sample;
	tables GaveThisYear*selected;
run;



data cortex_data_train(drop=selected SelectionProb SamplingWeight)
	cortex_data_valid(drop=selected SelectionProb SamplingWeight);
	set sample;
	if selected=1 then output cortex_data_train;
	else output cortex_data_valid;
run;

/*over-sampling training data */
proc surveyselect data=cortex_data_train out=cortex_data_train 
method=urs sampsize=(450000, 300000)  outhits;
strata GaveThisYear;
run;
%macro greenacre(dsn=,var=,target=);
	proc means data=&dsn nway noprint;
		class &var;
		var &target;
		output mean=prop out=level;
	run;
	title1 'Class Proportions and Frequencies';
	proc print data=level noobs;
		var &var prop _freq_;
	run;
	ods output clusterhistory=history;
	title1 'Ward''s Method';	
	proc cluster data=level method=ward outtree=dendrogram;
		freq _freq_;
		var prop;
		id &var;
	run;
	proc freq data=&dsn noprint;
		table &var*&target/chisq;
		output chisq out=chi(keep=_pchi_);
	run;
	data cutoff;
		if _n_=1 then set chi;
		set history;
		if numberofclusters > 1 then do;
			chisquare=_pchi_*rsquared;
			degfree=numberofclusters-1;
			logpvalue=logsdf('CHISQ',chisquare,degfree);
		end;
	run;
	title1 "Plot of the Log of the P-Value by Number of Clusters";
	proc sgplot data=cutoff;
		xaxis label="Number of Clusters";
		yaxis label="Log of P-Value";
		scatter y=logpvalue x=numberofclusters 
			/ markerattrs=(color=blue symbol=circlefilled);
	run;
	title1 "Number of Clusters Yielding the Minimum Log P-Value";
	proc sql;
		select NumberOfClusters into :ncl
		from cutoff
		having logpvalue=min(logpvalue);	
	quit;
	proc tree data=dendrogram nclusters=&ncl out=results noprint; 
		id &var;
	run;
	proc sort data=results;
		by clusname;
	run;
	title1 'Proposed Solution';
	proc print data=results noobs;
		by clusname;
		var &var;
	run;
%mend greenacre;

/* Cluster for SeniorList into: {0},{2},{1,3,4,5},{6,7,8},{9,10} */
/* cluster Seniority into: {3,4,5,6},{8,9,10},{0,7},{1},{2},{99}*/
%greenacre(dsn=cortex_data_train,var=SeniorList,target=GaveThisYear);
%greenacre(dsn=cortex_data_train,var=Seniority, target=GaveThisYear);
%greenacre(dsn=cortex_data_train,var=NbActivities, target=GaveThisYear);

data cortex_data_train_collapse;
	set cortex_data_train;
	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
	
run;

data cortex_data_valid_collapse;
	set cortex_data_valid;
		if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
	
run;


/* Variable Reduction*/
/* Numerical Variable Clustering */
ods select clustersummary rsquare;

proc varclus data=Cortex_data_train_collapse maxeigen=0.7 hi outtree=tree;
   var &intervals;
run;

/* Result:5 cluster, using the varibale with least 1-R**2 ratio */
%let reduced_interval = Age logAmtLastYear logSalary logReferrals logTotalGift; 

/* Screening Irrelevant Inputs */
ods select none;
ods output spearmancorr=spearman
           hoeffdingcorr=hoeffding;


/*Spearman & Hoeffding Correlation test */
/* Result: No variable shows weak association*/
proc corr data=Cortex_data_train_collapse spearman hoeffding;
   var GaveThisYear;
   with &reduced_interval;
run;


ods select all;
/* Adding the categorical variable, now we get the all qualified variables */
%let reduced = logSalary logAmtLastYear logTotalGift NbActivities_1 Salary Age LogReferrals SeniorList_1
		Woman Education City Contact Seniority_1 GaveLastYear;

/*Select the Interaction */
title1 'Determine P-Value for Entry and Retention';
proc sql;
	select 1-probchi(log(sum(GaveThisYear ge 0)),1) into :sl
	from Cortex_data_train_collapse;
quit;

/* Detect the Interactions */


/*proc logistic data=Cortex_data_train_collapse;
	class Woman(param=ref ref='1') Education(param=ref ref='University / College') 
   	City(param=ref ref='City') Seniorlist_1(param=ref ref='0')
   	Seniority_1(param=ref ref='99') Contact(param=ref ref='0') 
   	GaveLastYear(param=ref ref='0') NbActivities_1(param=ref ref='0');
	model GaveThisYear(event='1')= &reduced
			LogTotalGift|NbActivities_1|LogSalary|logAmtLastYear|Age|Woman|Education|LogReferrals|
			City|Seniorlist_1|Seniority_1|Contact|GaveLastYear @2 / 
			include=13 clodds=pl selection=forward slentry=&sl;
run;*/

/* Result: Add relevant interactions to the Model */ 
%let interactions = 		
Woman*Contact
Age*Contact
seniority_1*Contact
Contact*NbActivities_1
City*Contact
SeniorList_1*Contact
SeniorList_1*seniority_1
City*SeniorList_1
logReferrals*NbActivities_1
seniority*NbActivities_1
Education*City
logSalary*Contact
GaveLastYear
logTotalGift*Contact
Education*Contact
logTotalGift*seniority_1
logTotalGift*logReferrals
SeniorList_1*NbActivities_1
GaveLastYear*NbActivities_1
logAmtLastYear*City
logReferrals*Contact
logSalary*seniority_1
logReferrals*SeniorList_1
SeniorList_1*GaveLastYear
City*NbActivities_1
Woman*NbActivities_1
Education*NbActivities_1
logTotalGift*City
Woman*seniority_1
Age*NbActivities_1
logAmtLastYear*logReferrals
logReferrals*seniority_1;

/* Generate logistic Model for GaveThisYear and Score the model with validating Data*/
%let pi1 = 0.1495;
ods select roccurve;
proc logistic data=Cortex_data_train_collapse outmodel=ModelP;
	class Woman(param=ref ref='1') Education(param=ref ref='University / College') 
   		City(param=ref ref='City') Seniorlist_1(param=ref ref='0') 
   		Contact(param=ref ref='0') GaveLastYear(param=ref ref='0')
   		Seniority_1(param=ref ref='99') NbActivities_1(param=ref ref='0');
	model GaveThisYear(event='1')= &reduced &interactions/ 
   		clodds=pl selection=backward fast slstay=&sl hier=single outroc= roc1; 
   	score data=cortex_data_valid_collapse fitstat out=cortex_scored_valid outroc=roc;
run;




/* Import the Contact dataset and manipulate the data */
data Cortex_Contact;
	set cortex.score2_contact(rename=(Recency=intRecency Seniority=intSeniority));	

	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;	
run;

data Cortex_noContact;
	set cortex.score2_nocontact(rename=(Recency=intRecency Seniority=intSeniority));	

	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;	
run;


/* Linear Regression model */
Data Cortex_data_linear;
	set Cortex_data;
	if GaveThisYear eq 0 then delete;
run;


data Cortex_data_linear;
	set Cortex_data_linear;	

	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;	
run;


/* Set relevant variables */
%let vars = Woman Age LogSalary Education City Seniorlist_1 NbActivities_1 LogReferrals LogTotalGift  
Contact GaveLastYear LogAmtLastYear seniority_1;


/* Identify Multicollinearity */
/* Remove MaxGift */

proc reg data=Cortex_data_linear;
	model AmtThisYear=&intervals/ vif collin collinoint;
	
run;
quit;

proc surveyselect data = Cortex_data_linear out = Cortex_data_linear outall
samprate = 0.75 seed = 12345;
run;



/* training data */
data train;
set Cortex_data_linear;
where Selected =1;
run;

/* Validation data */
data validate;
set Cortex_data_linear;
where Selected =0;
run;

	
/* GLM */
proc glmselect data=train valdata=validate;
    class Woman Education City Seniorlist_1 Contact GaveLastYear NbActivities_1
     	 Seniority_1 /param=glm ref=first;
    model AmtThisYear= &vars
    Woman|Age|LogSalary|Education|City|SeniorList_1|NbActivities_1|LogReferrals|
    LogTotalGift|Contact|GaveLastYear|logAmtLastYear|Seniority_1 @2
 		/ selection=backward select=validate showpvalues choose=validate;
 	store linear_model;
run;
quit;

/* Import the Contact dataset and manipulate the data */
data Cortex_Contact;
	set cortex.score2_contact(rename=(Recency=intRecency Seniority=intSeniority));	
	/* Replace the missing value with 0 and NA*/
	ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = . 
		THEN Column_Name = 0;
	END;
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logMaxGift=log(Maxgift + 1);
	logMinGift=log(Mingift + 1);
	logAmtLastYear=log(AmtLastYear+1);
	
	if Seniority=. and gavelastyear=1 Then Seniority=0;
	if Seniority=. and gavelastyear=0 Then Seniority=99;
	if Recency=. and gavelastyear=1 Then Recency=0;
	if Recency=. and gavelastyear=0 Then Recency=99;

	
	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
run;

/* Apply the model to Contact */
proc logistic inmodel=ModelP;
	score data=Cortex_Contact priorevent= &pi1 out=pc;
run;

proc plm restore=linear_model;
	score data=Cortex_Contact out=amtcontact;
run;

/* Merge the Result and generate EC */
Data Contact;
	merge pc(rename=(p_1=probc)) 
	amtcontact (rename=(predicted=amtc));
	by ID;
	EC = probc * amtc;
	keep ID probc amtc EC;
run;
/* Import the NoContact dataset and manipulate the data */

data Cortex_NoContact;
	set cortex.score2_nocontact(rename=(Recency=intRecency Seniority=intSeniority));	
		ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = . 
		THEN Column_Name = 0;
	END;
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logMaxGift=log(Maxgift + 1);
	logMinGift=log(Mingift + 1);
	logAmtLastYear=log(AmtLastYear+1);
	
	if Seniority=. and gavelastyear=1 Then Seniority=0;
	if Seniority=. and gavelastyear=0 Then Seniority=99;
	if Recency=. and gavelastyear=1 Then Recency=0;
	if Recency=. and gavelastyear=0 Then Recency=99;

	
	if SeniorList in (6,7,8) then SeniorList_1='6-8';
		else if SeniorList in (9,10) then SeniorList_1='9-10';
		else if SeniorList in (1,3,4,5) then SeniorList_1 ='1,3-5';
		else if seniorList = 0 then SeniorList_1='0';
		else seniorList_1 = '2';
		
	if seniority in (8,9,10) then seniority_1='8-10';
		else if Seniority in (3,4,5,6) then Seniority_1='3-6';
		else if seniority in (0,7) then Seniority_1='0,7';
		else if Seniority =1 then Seniority_1='1';
		else if Seniority =2 then Seniority_1 = '2';
		else Seniority_1='99';

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
run;

/* Apply the model to the NoContact */
proc logistic inmodel=ModelP;
	score data=Cortex_NoContact priorevent= &pi1 out=pnc;
run;

proc plm restore=linear_model;
	score data=Cortex_NoContact out=amtnocontact;
run;

/* Merge the Result and generate ENC */
Data NoContact;
	merge pnc(rename=(p_1=probnc)) 
	amtnocontact (rename=(predicted=amtnc));
	by ID;
	ENC = probnc * amtnc;
	keep ID probnc amtnc ENC;
run;

/* Calculate the Uplift */
Data Result;
	merge  Contact NoContact;
	by ID;
	Uplift = EC - ENC;
run;

/* cutoff allocation */
proc sort data=Result out=Result;
	by descending Uplift ;
run;

/* Choose the cutoff point
   Contact every person have the uplift over 25 */
/*1: Persuadables
  2: Sleeping Dog 
  3: Sure Things 
  4: Lost Causes */
Data Choose_to_contact;
	set Result;
	if (Uplift >25) and (probc > 0.4) and (probc-probnc > 0.1) then Chosen = 1;
	else if (Uplift le 0) and (enc > 30) and (probnc > 0.4) then Chosen= 2;
	else if (probc > 0.4) and (probnc > 0.4)  and (enc > 30) then Chosen = 3;
	else if (probc < 0.4) and (probnc < 0.4) then chosen = 4;
run;

/* plot of  1: Persuadables
  			2: Sleeping Dog 
  			3: Sure Things 
  			4: Lost Causes */
proc sgplot data=Choose_to_contact;
  scatter y=EC x=ENC / group=chosen;
  where chosen in (1,2,3,4);
run;


/* Generate the csv file */
Data Chosen_ID;
	set Choose_to_contact;
	if Chosen NE 1 then delete;
	Keep ID;
run;

proc export data=Chosen_ID outfile="/home/u58560603/MBAN5210/Project/ID_chosen.csv" 
	dbms=csv replace;
	putnames=no;
run;
