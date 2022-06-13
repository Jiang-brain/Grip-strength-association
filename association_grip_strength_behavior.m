%excluding non-White
white_index=find(~isnan(behavior(:,7))==0);
behavior(white_index,7)=behavior(white_index,8);
white_index=find(~isnan(behavior(:,7))==0);
behavior(white_index,7)=behavior(white_index,9);
clear white_index;
included_index=find((ismember(behavior(:,7),[1,1001,1002,1003]))~=0);
behavior=behavior(included_index,:);
clear included_index;

%% excluding brain diseases
overlap_subject=intersect(non_cancer_id,behavior(:,2));
[a,overlap_index]=ismember(overlap_subject,behavior(:,2));
behavior(overlap_index,:)=[];
clear fc;clear overlap_subject;clear a;clear non_cancer_id;clear overlap_index;clear non_cancer_id;

%% cognition
zero_score=find(behavior(:,25)<=1);
behavior(zero_score,25)=NaN;
clear zero_score;
%reaction time
behavior(:,27)=log(behavior(:,27));
%prosepctive memory
index=find(behavior(:,26)==2);
behavior(index,26)=0;
clear index;
%numeric memory test
index_abondon=find(behavior(:,28)==-1);
behavior(index_abondon,28)=NaN;
clear index_abondon;
%trails making
not_completed=find(behavior(:,29)==0);
behavior(not_completed,29)=NaN;
not_completed=find(behavior(:,30)==0);
behavior(not_completed,30)=NaN;
behavior(:,29)=log(behavior(:,29));
behavior(:,30)=log(behavior(:,30));
clear not_completed;
% pairs associated memory ?
%behavior(:,36)=behavior(:,36).^2;
% pairs matching,poission

%
index_not_completed=find(behavior(:,40)==0);
behavior(index_not_completed,40)=NaN;
behavior(:,40)=log(behavior(:,40));
index_not_completed=find(behavior(:,41)==0);
behavior(index_not_completed,41)=NaN;
behavior(:,41)=log(behavior(:,41));%still positive skewness
index_not_completed=find(behavior(:,42)==0);
behavior(index_not_completed,42)=NaN;
behavior(:,42)=log(behavior(:,42));
clear index_not_completed

%
behavior(:,108)=log(behavior(:,108)+1);%neuroticism
behavior(:,111)=log(behavior(:,111));%depression
behavior(:,112)=log(behavior(:,112));%anxiety

% satisfication
index=find(behavior(:,117)<0);
behavior(index,117)=NaN;
index=find(behavior(:,118)<0);
behavior(index,118)=NaN;
index=find(behavior(:,119)<0);
behavior(index,119)=NaN;
index=find(behavior(:,120)<0);
behavior(index,120)=NaN;
index=find(behavior(:,121)<0);
behavior(index,121)=NaN;
index=find(behavior(:,122)<0);
behavior(index,122)=NaN;
index=find(behavior(:,123)<0);
behavior(index,123)=NaN;

index=find(behavior(:,131)<0|behavior(:,131)>6);%job satisfication
behavior(index,131)=NaN;

index=find(behavior(:,132)<0);
behavior(index,132)=NaN;
clear index;

%strength and handness
handness=behavior(:,[128,129,130]);
index_noNaN=find(isnan(handness(:,1)));
handness(index_noNaN,1)=max(handness(index_noNaN,:),[],2);
index_no_answer=find(handness(:,1)==-3);
handness(index_no_answer,1)=NaN;
handness(:,2:end)=[];
clear index_noNaN;

index_left=find(handness==2);
index_right=find(handness==1);
index_other=find(handness~=1&handness~=2);

strength=zeros(length(behavior),1)*NaN;
strength(index_left)=behavior(index_left,43);
strength(index_right)=behavior(index_right,44);
strength(index_other)=max(behavior(index_other,[43,44]),[],2);
clear index_left; clear index_right; clear index_other; clear handness;clear index_no_answer;clear index_noNaN;
index_outlier=find(strength<4);
strength(index_outlier)=NaN;
clear index_outlier;


%% covariate variable
%education years
edu_quantifications=behavior(:,11:16);
for i=1:6
   index_college=find(edu_quantifications(:,i)==1);
   index_no_college=find(ismember(edu_quantifications(:,i),[2,3,4,5,6,-7])==1);
   index_no_answer=find(edu_quantifications(:,i)==-3);
   
   edu_quantifications(index_college,i)=1;
   edu_quantifications(index_no_college,i)=0;
   edu_quantifications(index_no_answer,i)=NaN;
   clear index_college; clear index_no_college; clear index_no_answer;
end
behavior(:,11)=max(edu_quantifications,[],2);
clear edu_quantifications; clear i;

%bmi
behavior(:,22)=log(behavior(:,22));
%waist-hip ratio
behavior(:,23)=behavior(:,23)./behavior(:,24);
%deprivation
behavior(:,6)=log(behavior(:,6)-min(behavior(:,6))+1);

covariate_variable=behavior(:,[3,4,5,6,11,22,23,137]);
cognition_index=[25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,108,111,112,117,118,119,120,121,122,123,131,132,139]';
cognition=behavior(:,cognition_index);


%% cognition and strength
for i=1:30
    a=[strength,cognition(:,i),covariate_variable];
    noNaN_index=find(~isnan(sum(a,2)));
    a=a(noNaN_index,:);
    
    tbl = table(zscore(a(:,2)),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
        'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
    tbl.education = categorical(tbl.education);
    tbl.gender = categorical(tbl.gender);
    tbl.site = categorical(tbl.site);
    lme = fitlme(tbl,'Cognition~Strength+Age+gender+deprivation+education+bmi+whr+height+(Strength|site)');
    
    result(i,1)=lme.Coefficients.Estimate(2);
    result(i,2)=lme.Coefficients.pValue(2);
    result(i,3)=lme.Coefficients.SE(2);
    result(i,4)=lme.Coefficients.tStat(2);
    result(i,5)=lme.Coefficients.Lower(2);
    result(i,6)=lme.Coefficients.Upper(2);
    result(i,7)=length(a(:,1));
    clear a; clear tbl;clear noNaN_index;clear lme;
end
clear i;

%% 0, 1, do not zscore the cognition
a=[strength,cognition(:,2),covariate_variable];
noNaN_index=find(~isnan(sum(a,2)));
a=a(noNaN_index,:);

tbl = table(a(:,2),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
    'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
tbl.education = categorical(tbl.education);
tbl.gender = categorical(tbl.gender);
tbl.site = categorical(tbl.site);
glme = fitglme(tbl,'Cognition~Strength+Age+gender+deprivation+education+bmi+whr+height+(Strength|site)','Distribution','binomial','Link','logit');
result(2,1)=glme.Coefficients.Estimate(2);
result(2,2)=glme.Coefficients.pValue(2);
result(2,3)=glme.Coefficients.SE(2);
result(2,4)=glme.Coefficients.tStat(2);
result(2,5)=glme.Coefficients.Lower(2);
result(2,6)=glme.Coefficients.Upper(2);
result(2,7)=length(a(:,1));
clear a; clear tbl; clear noNaN_index; clear glme;

%% count data, pairs matching, 37,38,39, index 12 13 14
for i=12:14
    a=[strength,cognition(:,i),covariate_variable];
    noNaN_index=find(~isnan(sum(a,2)));
    a=a(noNaN_index,:);

    tbl = table(a(:,2),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
        'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
    tbl.education = categorical(tbl.education);
    tbl.gender = categorical(tbl.gender);
    tbl.site = categorical(tbl.site);
    glme = fitglme(tbl,'Cognition~Strength+Age+gender+deprivation+education+bmi+whr+height+(Strength|site)','Distribution','poisson');
    result(i,1)=glme.Coefficients.Estimate(2);
    result(i,2)=glme.Coefficients.pValue(2);
    result(i,3)=glme.Coefficients.SE(2);
    result(i,4)=glme.Coefficients.tStat(2);
    result(i,5)=glme.Coefficients.Lower(2);
    result(i,6)=glme.Coefficients.Upper(2);
    result(i,7)=length(a(:,1));
    clear a; clear tbl; clear noNaN_index; clear glme;
end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% association between grip strength and behavior by gender
for i=1:30
    a=[strength,cognition(:,i),covariate_variable];
    noNaN_index=find(~isnan(sum(a,2)));
    a=a(noNaN_index,:);
    
    tbl = table(zscore(a(:,2)),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
        'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
    tbl.education = categorical(tbl.education);
    tbl.site = categorical(tbl.site);
    
    index_female=find(tbl.gender==1);
    tbl=tbl(index_female,:);
    
    lme = fitlme(tbl,'Cognition~Strength+Age+deprivation+education+bmi+whr+height+(Strength|site)');
    
    result(i,1)=lme.Coefficients.Estimate(2);
    result(i,2)=lme.Coefficients.pValue(2);
    result(i,3)=lme.Coefficients.SE(2);
    result(i,4)=lme.Coefficients.tStat(2);
    result(i,5)=lme.Coefficients.Lower(2);
    result(i,6)=lme.Coefficients.Upper(2);
    result(i,7)=length(tbl.Cognition);
    clear a; clear tbl;clear noNaN_index;clear lme;
end
clear i;

%% 0, 1
a=[strength,cognition(:,2),covariate_variable];
noNaN_index=find(~isnan(sum(a,2)));
a=a(noNaN_index,:);

tbl = table(a(:,2),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
    'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
tbl.education = categorical(tbl.education);
tbl.site = categorical(tbl.site);

index_female=find(tbl.gender==1);
tbl=tbl(index_female,:);
    
glme = fitglme(tbl,'Cognition~Strength+Age+deprivation+education+bmi+whr+height+(Strength|site)','Distribution','binomial','Link','logit');
result(2,1)=glme.Coefficients.Estimate(2);
result(2,2)=glme.Coefficients.pValue(2);
result(2,3)=glme.Coefficients.SE(2);
result(2,4)=glme.Coefficients.tStat(2);
result(2,5)=glme.Coefficients.Lower(2);
result(2,6)=glme.Coefficients.Upper(2);
result(2,7)=length(tbl.Cognition);
clear a; clear tbl; clear noNaN_index; clear glme;

%% count data, pairs matching, 37,38,39, index 12 13 14
for i=12:14
    a=[strength,cognition(:,i),covariate_variable];
    noNaN_index=find(~isnan(sum(a,2)));
    a=a(noNaN_index,:);

    tbl = table(a(:,2),zscore(a(:,1)),a(:,3),a(:,4),a(:,6),a(:,7),a(:,8),a(:,9),a(:,10),a(:,5),...
        'VariableNames',{'Cognition','Strength','Age','gender','deprivation','education','bmi','whr','height','site'});
    tbl.education = categorical(tbl.education);
    tbl.site = categorical(tbl.site);
    
    index_female=find(tbl.gender==1);
    tbl=tbl(index_female,:);
    
    glme = fitglme(tbl,'Cognition~Strength+Age+deprivation+education+bmi+whr+height+(Strength|site)','Distribution','poisson');
    result(i,1)=glme.Coefficients.Estimate(2);
    result(i,2)=glme.Coefficients.pValue(2);
    result(i,3)=glme.Coefficients.SE(2);
    result(i,4)=glme.Coefficients.tStat(2);
    result(i,5)=glme.Coefficients.Lower(2);
    result(i,6)=glme.Coefficients.Upper(2);
    result(i,7)=length(tbl.Cognition);
    clear a; clear tbl; clear noNaN_index; clear glme;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%