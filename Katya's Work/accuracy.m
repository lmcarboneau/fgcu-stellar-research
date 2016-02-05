function [Output] = accuracy(StarData)
%code to calculate false negatives or accuracy of eb finding results
format bank
dim=size(StarData);
length = dim(:,1);
width = dim(:,2);
%variable star accuracy 
VS_acc= (sum(StarData(:,4))/length)*100
%potential eclipsing binary accuracy 
EB_acc= (sum(StarData(:,5))/length)*100
%follow up accuracy 
FU_acc= (sum(StarData(:,6))/length)*100

%gets sums for each row 
row_sum=sum(StarData(:,4:6),2);
%total row accuracy percentage (how many rows add up to 3)
row_acc=(sum(sum(row_sum,2)==3)/length)*100

%file names with their row sums
StarData1=[StarData(:,1),row_sum];

%count all individual files
files=numel(unique(StarData1(:,1)));

%get list of files that have all 3 flags
StarDataEB=StarData1(:,2)==3;

%All EB files
EB_Files=unique(StarData1(StarDataEB))

%percentage of all files that got all 3 triggers at least once
EB_File_acc=numel(EB_Files)/files*100
end 