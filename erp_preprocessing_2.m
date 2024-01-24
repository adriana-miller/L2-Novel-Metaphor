%C ERPLAB Script
% This script loads in ICd files (bad IC components were delected and bad
% channels were interpolated in script B)
%Assigns bins, extracts epochs, peak-to-peak artifact rejection, averages
%epochs
%Exports txt files for analysis in R

%Need to run this script twice: 
    %Once for verb epochs -200 to 600 with verb related bins
    %Again for final word epochs -200 to 800 with final word bins

    %% Load Info

A_datainfo;
OutputFolder = '/Users/adrianamiller/Desktop/masters/data/Metaphor/metaphor_EEG/output/';
ERPfolder = '/Users/adrianamiller/Desktop/masters/data/Metaphor/metaphor_EEG/output/erp';
BinFolder = '/Users/adrianamiller/Desktop/masters/data/Metaphor/metaphor_EEG/Actual Scripts/bin_files/';
if ~exist(ERPfolder), mkdir(ERPfolder); end

chanfile = '/Users/adrianamiller/Documents/Documents - Adriana’s MacBook Air - 1/eeglab2022.0/plugins/dipfit/standard_BESA/standard-10-5-cap385.elp';


%% Bins and Artifact Rejection

for iSubj = 3:length(sInfo)
    
    %Load ICAed file 
    [ALLEEG EEG CURRENTSET ALLCOM] = eeglab('nogui');
    EEG = pop_loadset('filename', [sInfo(iSubj).name '-IC-cont-icrej.set'], 'filepath', OutputFolder);
    EEG=pop_chanedit(EEG, 'lookup', chanfile);
    
    %create list of events
    EEG  = pop_creabasiceventlist(EEG , 'AlphanumericCleaning', 'on', 'BoundaryNumeric', { -99 }, 'BoundaryString', { 'boundary' });

    %assign bins (codes) to specific events based on the Bin Descriptor
    %File (BDF)
    %change for verb and final
    EEG  = pop_binlister( EEG , 'BDF', [BinFolder 'bin_verb.txt'], 'IndexEL',  1, 'SendEL2', 'EEG', 'Voutput', 'EEG' );
    
    %extract bin-based epochs & baseline correct
    %set for 600 for verb
    %set for 800 for final
    EEG = pop_epochbin( EEG , [-200.0  600.0],  'pre');
    
    %Artifact detection (Moving window peak-to-peak & Step-like artifacts)
    %change 600 v 800 for verb v final
    EEG  = pop_artmwppth( EEG , 'Channel', 1:30, 'Flag',  1, 'Threshold',  100, 'Twindow', [ -200 600], 'Windowsize',  200, 'Windowstep',  100 )
    
    %Average epochs to get individual ERPs
    ERP = pop_averager(EEG ,'Criterion','good','ExcludeBoundary','on','SEM','on');
    
    %Save individual ERPs
    ERP = pop_savemyerp(ERP,'erpname',[sInfo(iSubj).name],'filename',[ sInfo(iSubj).name '.erp' ], 'filepath', ERPfolder);

    %ERP artifact rejection summary: save in a table
    [ERP ALLERP] = pop_loaderp('filename', [ERPfolder sInfo(iSubj).name '.erp']);
    pop_summary_AR_erp_detection(ERP,[ERPfolder sInfo(iSubj).name '_AR.txt']);
    
end

%Exporting text files
newERPpath = '/Volumes/Untitled/masters_thesis/data/metaphor/ERPs_verb_no_extra_channel/';
newERPoutput = '/Volumes/Untitled/masters_thesis/data/metaphor/ERPs_verb_no_extra_channel/newERPoutput/'
if ~exist(newERPoutput), mkdir(newERPoutput); end

%electrodes as columns
for iSubj = 1:length(sInfo)
[ERP ALLERP] = pop_loaderp('filename', [newERPpath sInfo(iSubj).name '.erp']);
pop_export2text( ERP, [newERPoutput sInfo(iSubj).name '.txt'],  1:6, 'electrodes', 'on', 'precision',  4, 'time', 'on', 'timeunit',  0.001, 'transpose', 'off' );
end
