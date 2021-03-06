function [Averages] = Avg_Koi(StarData)

%This is a simple script that finds the averages of stars in a .txt file
%that has been processed by the 'Processing' scripts (found in the KOI and
%Non-KOI directories accordingly).

%NOTE: The format is very specific so please do
%not use any other formats besides tht one that is output by the
%'Processing' software, or duplicate the format.

[ID, ~, Groups] = unique(StarData(:,1),'stable');
Averages = [ID, accumarray(Groups, StarData(:,2), [], @mean)];
app1 = accumarray(Groups, StarData(:,3), [], @mean);
app2 = accumarray(Groups, StarData(:,4), [], @mean);
appadd_1 = accumarray(Groups, StarData(:,4), [], @std);
FLMin = accumarray(Groups, StarData(:,4), [], @min);
app3 = accumarray(Groups, StarData(:,5), [], @mean);
app4 = accumarray(Groups, StarData(:,5), [], @std);
app5 = accumarray(Groups, StarData(:,6), [], @mean);
app6 = accumarray(Groups, StarData(:,7), [], @mean);
Averages = [Averages app1 app2 appadd_1 FLMin app3 app4 app5 app6];
end