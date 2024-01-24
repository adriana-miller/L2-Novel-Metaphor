% B Preprocessing Script

%% General Info

A_datainfo;

inputEEGFolder = '/Volumes/Untitled/masters_thesis/data/metaphor/cnt_files';
cd(inputEEGFolder);
outputEEGFolder = '/Volumes/Untitled/masters_thesis/data/metaphor/cnt_files/output';
nameoffiguresfolder = '/Volumes/Untitled/masters_thesis/data/metaphor/cnt_files/figures';
if ~exist(outputEEGFolder), mkdir(outputEEGFolder); end
if ~exist(nameoffiguresfolder), mkdir(nameoffiguresfolder); end

chanfile = '/Users/adrianamiller/Documents/Documents - Adriana’s MacBook Air - 1/eeglab2022.0/plugins/dipfit/standard_BESA/standard-10-5-cap385.elp';

%% Preprocessing

for iSubj = 1 :length(sInfo)
    
    A_datainfo

    cd(inputEEGFolder);
    % load dataset
    [ALLEEG EEG CURRENTSET ALLCOM] = eeglab('nogui');
    EEG = pop_loadcnt([sInfo(iSubj).name '.cnt']);
    [ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG, 0 );
    EEG.setname = sInfo(iSubj).name; 
     
    % Filter 30 Hz lowpass
    EEG = pop_eegfiltnew(EEG, 'locutoff',.1);
    EEG = pop_eegfiltnew(EEG, 'hicutoff', 30);
      
    % Edit channels & remove external channels
    EEG=pop_chanedit(EEG, 'lookup', chanfile);
        
    % Reject bad portions of continuous data (listed in A_datainfo)
    EEG = eeg_eegrej( EEG, sInfo(iSubj).bad_data);
    [ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG, 0 );
      
    % Re-reference to average exclude eye electrodes and select M1 and M2
    % Will now have 32 channels because it deleted M1 and M2
    EEG = pop_reref( EEG, {'M1' 'M2'},'exclude', [33 34]);

    % Delete bad channels (listed in A_datainfo)
    % Should now have 32 minus number of bad channels
    EEG = pop_select( EEG, 'nochannel',sInfo(iSubj).bad_channels);
 
    % Run ICA
    ChannelNo = 1:length(EEG.chanlocs);
%   ICAchan = 1:length(EEG.chanlocs)-1;%-length(sInfo(iSubj).bad_channels);
    EEG = pop_runica(EEG, 'extended',1,'chanind', ChannelNo,'interupt','on'); % run ICA

       
    % Calculate IClabel (without dipoles yes)
    EEG = pop_iclabel(EEG, 'default');
    
    %Plot extended IC solutions
    for loopthroughICS = 1:(size(EEG.icawinv,2))
        cd (nameoffiguresfolder)
        numberofIC = num2str(loopthroughICS);
        pop_prop_extended(EEG, 0, loopthroughICS, NaN, {'freqrange' [2 50] });
        set(gcf,'units','normalized','outerposition',[0 0 1 1])
        print(gcf, '-dtiff', [sInfo(iSubj).name '_' numberofIC '_IC_extended.tiff' ]);  close (gcf);
    end;
    
    % Save file
    EEG = pop_saveset( EEG, 'filepath', outputEEGFolder, 'filename', [ sInfo(iSubj).name '-ICA-cont.set' ])
    
end %for iSubj=length(sInfo)

%% SAVING WITH REJECTED ICs

for iSubj = 1:length(sInfo)
    
    %% create original EEG for channel interpolation
    % load original CNT dataset
    cd(inputEEGFolder);
    [ALLEEG EEG CURRENTSET ALLCOM] = eeglab('nogui');
    EEG = pop_loadcnt([sInfo(iSubj).name '.cnt']);
    [ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG, 0 );
    EEG.setname = sInfo(iSubj).name; 
    EEG=pop_chanedit(EEG, 'lookup', chanfile);
    % create a copy of EEG as origEEG for interpolation 
    origEEG = EEG;
    origEEG = pop_reref( origEEG, {'M1' 'M2'},'exclude', [33 34]); %M! and M2 will be removed after re-referencing
   
    %% Load ICed file
    cd(outputEEGFolder)
    [ALLEEG EEG CURRENTSET ALLCOM] = eeglab('nogui');
    EEG = pop_loadset([sInfo(iSubj).name '-ICA-cont.set']);
    [ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG, 0 );
    EEG.setname = sInfo(iSubj).name;
    
    % Set IC thresholds after looking at all IC data
    % Bad ICs: eye-related activity > 40%; muscle activity > 50%; channel >
    % 90%
    % Good ICs: brain-related activity > 80%
    % 1 = brain, 2 = muscle, 3 = eye, 4 = heart, 5 = line noise, 6 =
    % channel noise, 7 = other
    % 
    
    boo = EEG.etc.ic_classification.ICLabel.classifications;
    badOnes = find(boo(:,3) > .4 & boo(:,1) < .15 | boo(:,2) > .49 & boo(:,1) < .1 | boo(:,6) > .9 & boo(:,1) < .1)
    sInfo(iSubj).bad_comps = badOnes;
    
    % Reject bad eye-related ICs
    EEG = pop_subcomp( EEG, badOnes, 0);
    
    % Interpolate deleted channels
    EEG = pop_interp(EEG,origEEG.chanlocs, 'spherical'); 
    [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET); %
    EEG = pop_saveset( EEG, 'filepath', outputEEGFolder, 'filename', [ sInfo(iSubj).name '-IC-cont-icrej.set' ])

end
