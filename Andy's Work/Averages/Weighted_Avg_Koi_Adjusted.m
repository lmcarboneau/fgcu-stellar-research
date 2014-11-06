function [Averages] = Weighted_Avg_Koi_Adjusted(StarData)

%This is a (not so) simple script that finds the weighted averages of stars in a .txt file
%that has been processed by the 'Processing' scripts (found in the KOI and
%Non-KOI directories accordingly). This script differs from the others in
%that it removes any period entry that is above 44, which is our upper
%limit for the periods.

%NOTE: The format is very specific so please do
%not use any other formats besides tht one that is output by the
%'Processing' software, or duplicate the format.

StarData2 = [];

%The weight of each point is determined by the DET_STAT, this block sets the
%code up to assign a value (the current period value) and a weight, and also
%uses the loop to filter out the periods that are above 44:
for i = 1:length(StarData)
    if StarData(i,5) < 44
        StarData2 = [StarData2; StarData(i,:)];
    end
end

Weights = StarData2(:,6);
Vals = StarData2(:,5);


WeightedMeanFcn = @(ii) sum(Vals(ii).*Weights(ii))/sum(Weights(ii));

[NewID,~,Groups2] = unique(StarData2(:,1),'stable');
[ID, ~, Groups] = unique(StarData(:,1),'stable');
Averages = [ID, accumarray(Groups, StarData(:,2), [], @mean)];
app1 = accumarray(Groups, StarData(:,3), [], @mean);
app2 = accumarray(Groups, StarData(:,4), [], @mean);
appadd_1 = accumarray(Groups, StarData(:,4), [], @std);
FLMin = accumarray(Groups, StarData(:,4), [], @min);
app3 = accumarray(Groups2, 1:length(Groups2), [], WeightedMeanFcn);
app4 = accumarray(Groups2, StarData2(:,5), [], @std);
app5 = accumarray(Groups, StarData(:,6), [], @mean);
app6 = accumarray(Groups, StarData(:,7), [], @mean);
Averages = [Averages app1 app2 appadd_1 FLMin app3 app4 app5 app6];
end
