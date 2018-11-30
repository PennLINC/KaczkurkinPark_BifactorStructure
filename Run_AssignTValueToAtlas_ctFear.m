%Assigns the significant t-values to the JLF atlas for input into BrainNet Viewer.

%Specify the full path to the atlas. Should be .nii format.
Atlas_Path = '/data/jux/BBL/projects/pncT1AcrossDisorder/images/mniJLF_LabelsWithWM.nii';

%Load the .csv file with the significant t-values and index numbers.
SigRegions = csvread('/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/JLFct_signifROIs_fear.csv');

%Define a vector of index numbers for the signficant regions.
SigRegionIndex = SigRegions(:,end);

%Define a vector of t values for the significant regions.
TValue = SigRegions(:,1,:);

%Specify the path of the output file.
ResultantFile = '/data/jux/BBL/projects/pncT1AcrossDisorder/images/JLF_ct/JLFct_signifROIs_fear.nii';

%Run the AssignTValueToAtlas function.
AssignTValueToAtlas(Atlas_Path, SigRegionIndex, TValue, ResultantFile)
