%white
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
clear fc;clear overlap_subject;clear a;clear non_cancer_id;clear overlap_index;

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

%% strength and handness
handness=behavior(:,[128,129,130]);
index_noNaN=find(isnan(handness(:,1)));
handness(index_noNaN,1)=max(handness(index_noNaN,:),[],2);
index_no_answer=find(handness(:,1)==-3);
handness(index_no_answer,1)=NaN;
handness(:,2:end)=[];

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
behavior(:,43)=strength;
clear strength;clear index_outlier;


%% extract gmv features
gmv_features=[fast,first_subcortical,fast_cerebellum];
clear fast; clear fast_cerebellum; clear first_subcortical;


total_grey_matter=icv;
clearvars -except gmv_features subjects behavior total_grey_matter
%
overlap_subject=intersect(subjects,behavior(:,2)); % behavior(:,2) ID index
[a,b]=ismember(overlap_subject,subjects);
idp=gmv_features(b,:); % IDP save the matrics of brain features
total_grey_matter=total_grey_matter(b,:);
clear gmv_features;clear subjects;

[a,b]=ismember(overlap_subject,behavior(:,2)); 
cognition=behavior(b,:);
clear a; clear b; clear overlap_subject;clear behavior;

features1=[idp,cognition(:,43)]; % the measure to be predicted
clear idp;

noNaN_index=find(~isnan(sum(features1,2)));
features1=features1(noNaN_index,:);
covariate_variable=cognition(noNaN_index,[3,4,5,6,11,22,23,137]);
total_grey_matter=total_grey_matter(noNaN_index);
clear noNaN_index;clear cognition;


%% association between grip strength and GMV in all subjects controling for total gmv (icv)
for i=1:size(features1,2)-1
    temp_features=features1(:,i);
    removed_index=find((temp_features>mean(temp_features)+5*std(temp_features))|(temp_features<mean(temp_features)-5*std(temp_features)));
    temp_features(removed_index)=[];
    
    temp_covariate=covariate_variable;
    temp_covariate(removed_index,:)=[];
    
    strength=features1(:,end);
    strength(removed_index)=[];
    
    grey_matter=total_grey_matter;
    grey_matter(removed_index)=[];
    
    %remove NaN values
    a=[strength,temp_features,temp_covariate,grey_matter];
    %a=[strength,temp_features,temp_covariate];
    noNaN_index=find(~isnan(sum(a,2)));
    strength=strength(noNaN_index,:);
    temp_features=temp_features(noNaN_index,:);
    temp_covariate=temp_covariate(noNaN_index,:);
    grey_matter=grey_matter(noNaN_index,:);
    clear a;clear noNaN_index;
    
    
    tbl = table(zscore(temp_features),zscore(strength),grey_matter,temp_covariate(:,1),temp_covariate(:,2),temp_covariate(:,4),temp_covariate(:,5),temp_covariate(:,6),temp_covariate(:,7),temp_covariate(:,8),temp_covariate(:,3),...
        'VariableNames',{'Gm','Strength','Total_gm','Age','gender','deprivation','education','bmi','whr','height','site'});
%    tbl = table(zscore(temp_features),zscore(strength),temp_covariate(:,1),temp_covariate(:,2),temp_covariate(:,4),temp_covariate(:,5),temp_covariate(:,6),temp_covariate(:,7),temp_covariate(:,3),...
%        'VariableNames',{'Gm','Strength','Age','gender','deprivation','education','bmi','whr','site'});
    tbl.education = categorical(tbl.education);
    tbl.gender = categorical(tbl.gender);
    tbl.site = categorical(tbl.site);
    lme = fitlme(tbl,'Gm~Strength+Total_gm+Age+gender+deprivation+education+bmi+whr+height+(Strength|site)');
%    lme = fitlme(tbl,'Gm~Strength+Age+gender+deprivation+education+bmi+whr+(Strength|site)');
%    b=lme.fixedEffects;
    result(i,1)=lme.Coefficients.Estimate(2);
    result(i,2)=lme.Coefficients.pValue(2);
    result(i,3)=lme.Coefficients.SE(2);
    result(i,4)=lme.Coefficients.tStat(2);
    result(i,5)=lme.Coefficients.Lower(2);
    result(i,6)=lme.Coefficients.Upper(2);
    result(i,7)=length(temp_features);
    result(i,8)=length(removed_index);
    clear removed_index;clear temp_features; clear temp_covariate;clear strength;clear tbl;clear grey_matter;
end

%% association between grip strength and GMV in all subjects controling for total gmv (icv) in females/males
for i=1:size(features1,2)-1
    temp_features=features1(:,i);
    removed_index=find((temp_features>mean(temp_features)+5*std(temp_features))|(temp_features<mean(temp_features)-5*std(temp_features)));
    temp_features(removed_index)=[];
    
    temp_covariate=covariate_variable;
    temp_covariate(removed_index,:)=[];
    
    strength=features1(:,end);
    strength(removed_index)=[];
    
    grey_matter=total_grey_matter;
    grey_matter(removed_index)=[];
    
    %remove NaN values
    a=[strength,temp_features,temp_covariate,grey_matter];
    noNaN_index=find(~isnan(sum(a,2)));
    strength=strength(noNaN_index,:);
    temp_features=temp_features(noNaN_index,:);
    temp_covariate=temp_covariate(noNaN_index,:);
    grey_matter=grey_matter(noNaN_index,:);
    clear a;clear noNaN_index;
    
    
    tbl = table(zscore(temp_features),zscore(strength),grey_matter,temp_covariate(:,1),temp_covariate(:,2),temp_covariate(:,4),temp_covariate(:,5),temp_covariate(:,6),temp_covariate(:,7),temp_covariate(:,8),temp_covariate(:,3),...
        'VariableNames',{'Gm','Strength','Total_gm','Age','gender','deprivation','education','bmi','whr','height','site'});

    %index_female=find(tbl.gender==0);
    %tbl=tbl(index_female,:);
    
    index_male=find(tbl.gender==1);
    tbl=tbl(index_male,:);
    
    tbl.education = categorical(tbl.education);
    tbl.site = categorical(tbl.site);

    lme = fitlme(tbl,'Gm~Strength+Total_gm+Age+deprivation+education+bmi+whr+height+(Strength|site)');

    result(i,1)=lme.Coefficients.Estimate(2);
    result(i,2)=lme.Coefficients.pValue(2);
    result(i,3)=lme.Coefficients.SE(2);
    result(i,4)=lme.Coefficients.tStat(2);
    result(i,5)=lme.Coefficients.Lower(2);
    result(i,6)=lme.Coefficients.Upper(2);
    result(i,7)=length(temp_features);
    result(i,8)=length(removed_index);
    clear removed_index;clear temp_features; clear temp_covariate;clear strength;
    clear tbl;clear grey_matter; clear index_female;clear index_male;
end


%% association between grip strength and GMV in all subjects controling for total gmv (icv), and icv2
for i=1:size(features1,2)-1
    temp_features=features1(:,i);
    removed_index=find((temp_features>mean(temp_features)+5*std(temp_features))|(temp_features<mean(temp_features)-5*std(temp_features)));
    temp_features(removed_index)=[];
    
    temp_covariate=covariate_variable;
    temp_covariate(removed_index,:)=[];
    
    strength=features1(:,end);
    strength(removed_index)=[];
    
    grey_matter=total_grey_matter;
    grey_matter(removed_index)=[];
    
    %remove NaN values
    a=[strength,temp_features,temp_covariate,grey_matter];
    %a=[strength,temp_features,temp_covariate];
    noNaN_index=find(~isnan(sum(a,2)));
    strength=strength(noNaN_index,:);
    temp_features=temp_features(noNaN_index,:);
    temp_covariate=temp_covariate(noNaN_index,:);
    grey_matter=grey_matter(noNaN_index,:);
    icv2=zscore(grey_matter.^2);
    clear a;clear noNaN_index;
    
    
    tbl = table(zscore(temp_features),zscore(strength),grey_matter,icv2,temp_covariate(:,1),temp_covariate(:,2),temp_covariate(:,4),temp_covariate(:,5),temp_covariate(:,6),temp_covariate(:,7),temp_covariate(:,8),temp_covariate(:,3),...
        'VariableNames',{'Gm','Strength','Total_gm','icv2','Age','gender','deprivation','education','bmi','whr','height','site'});

    tbl.education = categorical(tbl.education);
    tbl.gender = categorical(tbl.gender);
    tbl.site = categorical(tbl.site);
    lme = fitlme(tbl,'Gm~Strength+Total_gm+icv2+Age+gender+deprivation+education+bmi+whr+height+(Strength|site)');

    result(i,1)=lme.Coefficients.Estimate(2);
    result(i,2)=lme.Coefficients.pValue(2);
    result(i,3)=lme.Coefficients.SE(2);
    result(i,4)=lme.Coefficients.tStat(2);
    result(i,5)=lme.Coefficients.Lower(2);
    result(i,6)=lme.Coefficients.Upper(2);
    result(i,7)=length(temp_features);
    result(i,8)=length(removed_index);
    result(i,9)=lme.Coefficients.pValue(4);
    clear removed_index;clear temp_features; clear temp_covariate;clear strength;clear tbl;clear grey_matter;
end





