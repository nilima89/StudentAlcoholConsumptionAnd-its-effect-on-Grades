TITLE "Effect of Student Alcohol Consumption on Grades";
data student.data1;
set student.studentdata;
proc contents;
run;
data student.data1;
set student.data1;
if reason="course" then d1_reason=1;
else d1_reason=0;
data student.data1;
set student.data1;
if reason="reputation" then d2_reason=1;
else d2_reason=0;
data student.data1;
set student.data1;
if reason="home" then d3_reason=1;
else d3_reason=0;
run;
proc reg data=student.data1;
model G3=Dalc Fedu Medu Walc absences activities address age failures famrel famsize famsup freetime goout health internet pstatus romantic schoolsup sex studytime traveltime higher d1_reason d2_reason d3_reason/stb vif tol collinoint;
run;
proc corr;
var Dalc Fedu Medu Walc absences activities address age failures famrel famsize famsup freetime goout health internet pstatus romantic schoolsup sex studytime traveltime higher d1_reason d2_reason d3_reason;
run;
proc reg data=student.data1;
model G3=Dalc Fedu Medu Walc address age failures famrel famsize famsup freetime goout health internet pstatus romantic schoolsup studytime traveltime higher d1_reason d2_reason d3_reason/stb vif tol collinoint;
run;
proc reg data=student.data1;
model G3=Dalc Walc Fedu Medu address age failures famrel famsize famsup goout health internet pstatus romantic schoolsup studytime higher/stb vif tol collinoint;
run;
data student.data2;
set student.data1;
WalcXDalc= Walc*Dalc;
Parentsedu=Fedu*Medu;
run;

proc reg plots=none;
model G3=WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet /stb hccmethod=3 hcc;
run;
proc reg;
model G3=WalcXDalc Parentsedu failures studytime health romantic schoolsup higher address internet /stb ;
output R=resid1 out=regout1;
run;
data white1;
set regout1;
WalcXDalcsq=WalcXDalc**2;
Parentsedusq=Parentsedu**2;
failuressq=failures**2;
studytimesq=studytime**2;
healthsq=health**2;
romanticsq=romantic**2;
schoolsupsq=schoolsup**2;
highersq=higher**2;
addresssq=address**2;
internetsq=internet**2;
AlcXParent=WalcXDalc*Parentsedu;
AlcXfailures=WalcXDalc*failures;
AlcXstudytime=WalcXDalc*studytime;
AlcXromantic=WalcXDalc*romantic;
AlcXhigher=WalcXDalc*higher;
ParentXinternet=Parentsedu*internet;
ParentXstudytime=Parentsedu*studytime;
healthXromanticXhigher=health*romantic*higher;
schoolsupXhigherXaddress=schoolsup*higher*address;
AlcXhealthXstudytime=WalcXDalc*health*studytime;
failuresXhealthXinternet=failures*health*internet;
studytimeXromanticXParent=Parentsedu*romantic*studytime;
AlcXParentseduXstudytime=WalcXDalc*Parentsedu*studytime;
residsq=resid1**2;
run;
TITLE "First Whites Test";
proc reg plots=none data=white1;
  model residsq = WalcXDalcsq Parentsedusq failuressq studytimesq healthsq romanticsq schoolsupsq highersq addresssq internetsq AlcXParent AlcXfailures AlcXstudytime AlcXromantic AlcXhigher ParentXinternet ParentXstudytime healthXromanticXhigher schoolsupXhigherXaddress AlcXhealthXstudytime failuresXhealthXinternet studytimeXromanticXParent AlcXParentseduXstudytime;
  run;
