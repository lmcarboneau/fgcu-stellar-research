%Author: Ekaterina Vydra
%Function that reads in the reduced table created from Star Data output
%Returns list of files (Kepler IDs) marked as EBs
%Can return all other nonEB Kepler IDs by trigger values without duplicates

function [Output] = Accuracy(StarData)
%code to calculate false negatives or accuracy of eb finding results
StarData(isnan(StarData)) = 0; 
format bank
dim=size(StarData);
len = dim(:,1);
width = dim(:,2);

%gets sums for each row 
row_sum=sum(StarData(:,8:10),2);
%total row accuracy percentage (how many rows add up to 3)
row_acc=(sum(sum(row_sum,2)==3)/len)*100;

%file names with their row sums
StarData1=[StarData(:,1),row_sum];

%count all individual files
files=numel(unique(StarData1(:,1)))


%variable star accuracy 
VS_acc= (numel(unique(StarData(StarData(:,8)==1)))/files)*100;
%potential eclipsing binary accuracy 
EB_acc= (numel(unique(StarData(StarData(:,9)==1)))/files)*100;
%follow up accuracy 
SecondPeak_acc= (numel(unique(StarData(StarData(:,10)==1)))/files)*100;


%get list of files that have all 3 flags
StarDataEB=StarData1(:,2)==3;
%All EB files
EB_Files=unique(StarData1(StarDataEB))
E=numel(EB_Files);
%percentage of all files that got all 3 triggers at least once
All_triggers_acc=numel(EB_Files)/files*100

%%%Unsupress the last line of each section bellow to see output. 
%%%Used for error checking of outputs.
%get list of files that are missing the EB flag only 
StarData101a=unique(StarData(StarData(:,8)==1&StarData(:,9)==0&StarData(:,10)==1));
inters=intersect(StarData101a,EB_Files);
StarData101=setdiff(StarData101a,inters);

%get list of files that are missing the SP flag only 
StarData110a=unique(StarData(StarData(:,8)==1&StarData(:,9)==1&StarData(:,10)==0));
inters1=intersect(StarData110a,EB_Files);
inters2=intersect(StarData110a,StarData101);
StarData110=setdiff(setdiff(StarData110a,inters1),inters2);

%get list of files that are missing both EB and SP flags
StarData100a=unique(StarData(StarData(:,8)==1&StarData(:,9)==0&StarData(:,10)==0));
inters3=intersect(StarData100a,EB_Files);
inters4=intersect(StarData100a,StarData101);
inters5=intersect(StarData100a,StarData110);
StarData100=setdiff(setdiff(setdiff(StarData100a,inters3),inters4),inters5);
end 
