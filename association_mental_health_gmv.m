%% lmer, cognition and 139 rois
%% cognition and strength, total gm, height
for cognition_index=1:size(cognition,2)
    for i=1:size(features1,2)
        temp_features=features1(:,i);
        removed_index=find((temp_features>mean(temp_features)+5*std(temp_features))|(temp_features<mean(temp_features)-5*std(temp_features)));
        temp_features(removed_index)=[];

        temp_covariate=covariate_variable;
        temp_covariate(removed_index,:)=[];

        temp_cognition=cognition(:,cognition_index);% cognition index
        temp_cognition(removed_index)=[];

        grey_matter=icv;
        grey_matter(removed_index)=[];

        % remove NaN values
        a=[temp_cognition,temp_features,temp_covariate,grey_matter];
        %a=[temp_cognition,temp_features,temp_covariate];
        noNaN_index=find(~isnan(sum(a,2)));
        temp_cognition=temp_cognition(noNaN_index,:);
        temp_features=temp_features(noNaN_index,:);
        temp_covariate=temp_covariate(noNaN_index,:);
        grey_matter=grey_matter(noNaN_index,:);
        clear a;clear noNaN_index;


        tbl = table(zscore(temp_features),zscore(temp_cognition),grey_matter,temp_covariate(:,1),temp_covariate(:,2),temp_covariate(:,4),temp_covariate(:,5),temp_covariate(:,6),temp_covariate(:,7),temp_covariate(:,8),temp_covariate(:,3),...
            'VariableNames',{'Gm','Cognition','icv','Age','gender','deprivation','education','bmi','whr','height','site'});

        tbl.education = categorical(tbl.education);
        tbl.gender = categorical(tbl.gender);
        tbl.site = categorical(tbl.site);
        lme = fitlme(tbl,'Gm~Cognition+icv+Age+gender+deprivation+education+bmi+whr+height+(Cognition|site)');
    %    lme = fitlme(tbl,'Gm~Cognition+Age+gender+deprivation+education+bmi+whr+(Cognition|site)');
        result.estimate(i,cognition_index)=lme.Coefficients.Estimate(2);
        result.pvalue(i,cognition_index)=lme.Coefficients.pValue(2);
        result.se(i,cognition_index)=lme.Coefficients.SE(2);
        result.tstat(i,cognition_index)=lme.Coefficients.tStat(2);
        result.lower(i,cognition_index)=lme.Coefficients.Lower(2);
        result.upper(i,cognition_index)=lme.Coefficients.Upper(2);
        result.n(i,cognition_index)=length(temp_features);
        result.removed(i,cognition_index)=length(removed_index);
        clear removed_index;clear temp_features; clear temp_covariate;clear strength;clear tbl;clear grey_matter;
    end
end
