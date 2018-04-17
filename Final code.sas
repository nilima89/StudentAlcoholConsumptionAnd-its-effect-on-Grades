TITLE "Effect of Student Alcohol Consumption on Grades";
data student.data1;
set student.studentdata;
/* to check the contents of the data*/
proc contents;
run;
/* running the OLS regression with all the independent variables*/
proc reg data=student.data1;
model G3=Dalc Fedu Medu Walc absences activities address age failures famrel famsize famsup freetime goout health internet pstatus romantic schoolsup sex studytime traveltime higher/stb vif tol collinoint;
run;
/* checking for multicollinearity among independent variables*/
proc corr;
var Dalc Fedu Medu Walc absences activities address age failures famrel famsize famsup freetime goout health internet pstatus romantic schoolsup sex studytime traveltime higher;
run;
/* running the OLS regression model with only few selected variables which result in a good model*/
proc reg data=student.data1;
model G3=Dalc Walc Medu Fedu romantic schoolsup higher/stb vif tol collinoint;
run;
proc reg data=student.data1;
model G3=Dalc Medu failures higher romantic health schoolsup studytime/stb vif tol collinoint;
run;
proc reg data=student.data1;
model G3=Walc Medu failures higher romantic health schoolsup studytime/stb vif tol collinoint;
run;
/* creating an interaction term among Dalc and Walc and Medu and Fedu*/
data student.data2;
set student.data1;
WalcXDalc= Walc*Dalc;
Parentsedu=Fedu*Medu;
run;
/* running regression model for different variables along with the interaction term*/
proc reg data=student.data2;
model G3=WalcXDalc Medu romantic schoolsup higher/stb vif tol collinoint;
run;
proc reg data=student.data2;
model G3= WalcXDalc Medu failures studytime health romantic schoolsup higher/stb vif tol collinoint;
run;
proc reg data=student.data2;
model G3= WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet goout/stb vif tol collinoint;
run;
/*getting a final model after fixing the issue of heteroscedasticity*/
proc reg data=student.data2;
model G3=WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet /stb hccmethod=3 hcc;
run;
/* calculating dw statistic to check for auto correlation*/
data student.data3;
set student.data2;
run;
proc reg;
model  G3 = WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet /dwprob;
output R=resid OUT=newdata;
run;
proc freq data=student.data2;
tables WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet;
run;
proc means data=student.data2;
var WalcXDalc Parentsedu failures studytime health;
run;
Title 'Logit Analysis';
Proc Logistic descending data =student.data2;
  Model G3 =WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet /link=logit rsq;
  store grades;
run;
Title "Effect of various factors on Student Grades";
proc plm source=grades;
effectplot fit(x=WalcXDalc)/at( Parentsedu=mean failures=mean studytime=mean health=mean romantic=0 schoolsup=0 address=1 internet=1) YRANGE=CLIP ;
run;
