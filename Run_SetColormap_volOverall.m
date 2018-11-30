%Create a custom color palette for BrainNet Viewer.

%Load the .csv file with the significant t-values.
SigRegions = csvread('/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/JLFvol_signifROIs_overall.csv');

%Define a vector of t values for the significant regions.
TValue = SigRegions(:,1,:);

%Create a custom colormap by defining a three-column matrix of values between 0.0 and 1.0, where first column = reds, second column = greens, third column = blues.
ColorMap_Matrix = [0 0 0
		   0 0 0.0159
		   0 0 0.0317
		   0 0 0.0476
		   0 0 0.0635
		   0 0 0.0794
		   0 0 0.0952
		   0 0 0.1111
		   0 0 0.1270
		   0 0 0.1429
		   0 0 0.1587
		   0 0 0.1746
		   0 0 0.1905
		   0 0 0.2063
		   0 0 0.2222
		   0 0 0.2381
		   0 0 0.2540
		   0 0 0.2698
		   0 0 0.2857
		   0 0 0.3016
		   0 0 0.3175
		   0 0 0.3333
		   0 0 0.3492
		   0 0 0.3651
		   0 0 0.3810
		   0 0 0.3968
		   0 0 0.4127
		   0 0 0.4286
		   0 0 0.4444
		   0 0 0.4604
		   0 0 0.4762
		   0 0 0.4921
		   0 0 0.5079
		   0 0 0.5238
		   0 0 0.5397
		   0 0 0.5556
		   0 0 0.5714
		   0 0 0.5873
		   0 0 0.6032
		   0 0 0.6190
		   0 0 0.6349
		   0 0 0.6508
		   0 0 0.6667
		   0 0 0.6825
		   0 0 0.6984
		   0 0 0.7143
		   0 0 0.7302
		   0 0 0.7460
		   0 0 0.7619
		   0 0 0.7778
		   0 0 0.7937
		   0 0 0.8095
		   0 0 0.8254
		   0 0 0.8413
		   0 0 0.8571
		   0 0 0.8730
		   0 0 0.8889
		   0 0 0.9048
		   0 0 0.9206
		   0 0 0.9365
		   0 0 0.9524
		   0 0 0.9683
		   0 0 0.9841
		   0 0 1.0000];

%Specify the path of the output file.
ResultantFile = '/data/jux/BBL/projects/pncT1AcrossDisorder/images/JLF_vol/blues_volOverall.txt';

%Run the SetColormap function.
SetColormap(TValue, ColorMap_Matrix, ResultantFile)
