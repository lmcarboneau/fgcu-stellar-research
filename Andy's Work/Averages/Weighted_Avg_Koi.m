function [Averages] = Weighted_Avg_Koi(StarData)

%This is a simple script that finds the weighted averages of stars in a .txt file
%that has been processed by the 'Processing' scripts (found in the KOI and
%Non-KOI directories accordingly).

%NOTE: The format is very specific so please do
%not use any other formats besides tht one that is output by the
%'Processing' software, or duplicate the format.

%The weight of each point is determined by the DET_STAT, this line sets the
%code up to assign a value (the current period value) and a weight:
Weights = StarData(:,6); Vals = StarData(:,5);

%Custom Function to find the mean of the data with the weight applied:
WeightedMeanFcn = @(ii) sum(Vals(ii).*Weights(ii))/sum(Weights(ii));

[ID, ~, Groups] = unique(StarData(:,1),'stable');
Averages = [ID, accumarray(Groups, StarData(:,2), [], @mean)];
app1 = accumarray(Groups, StarData(:,3), [], @mean);
app2 = accumarray(Groups, StarData(:,4), [], @mean);
appadd_1 = accumarray(Groups, StarData(:,4), [], @std);
FLMin = accumarray(Groups, StarData(:,4), [], @min);
app3 = accumarray(Groups, 1:numel(Groups), [], WeightedMeanFcn);
app4 = accumarray(Groups, StarData(:,5), [], @std);
app5 = accumarray(Groups, StarData(:,6), [], @mean);
app6 = accumarray(Groups, StarData(:,7), [], @mean);
Averages = [Averages app1 app2 appadd_1 FLMin app3 app4 app5 app6];
end
