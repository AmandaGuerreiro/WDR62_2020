function [AnaphaseA1,AnaphaseA2,AnaphaseB,ProbA1,ProbA2,ZenithA1,ZenithA2] = AnaphasePerCell(Tracks)
%ANAPHASEPERCELL Summary of this function goes here
%
%   Nicolas Liaudet
%   Bioimaging Core Facility - UNIGE
%   https://www.unige.ch/medecine/bioimaging/en/bioimaging-core-facility/
%
%   28-Oct-2019

SpatialBin = 0.5;

TimeIdx = [Tracks.Kinetochore1.TimeIdx;...
    Tracks.Kinetochore2.TimeIdx;
    Tracks.Pole1.TimeIdx;
    Tracks.Pole2.TimeIdx];
TimeIdx = unique(TimeIdx);

%Pole
Pole1_xyz = nan(length(TimeIdx),3);
Pole2_xyz = nan(length(TimeIdx),3);
for idxT = 1:length(TimeIdx)
    idxT1 = Tracks.Pole1.TimeIdx == TimeIdx(idxT);
    idxT2 = Tracks.Pole2.TimeIdx == TimeIdx(idxT);
    Pole1_xyz(idxT,:) = mean(Tracks.Pole1{idxT1,{'X','Y','Z'}},1);
    Pole2_xyz(idxT,:) = mean(Tracks.Pole2{idxT2,{'X','Y','Z'}},1);    
end

%Anaphase B__________________________________________
v_P1toP2 = Pole2_xyz-Pole1_xyz;
d_P2P = vecnorm(v_P1toP2',2)';
AnaphaseB = table(d_P2P,TimeIdx);
AnaphaseB.Properties.VariableNames(1) = {'Mean'};

%Unitary vectors along Pole1-Pole2 axis__________________
u_Zenith = v_P1toP2./repmat(d_P2P,1,size(d_P2P,2));

%Anaphase A 1__________________________________________
TrackID = unique(Tracks.Kinetochore1.TrackID);
d_PK_1  = nan(length(TimeIdx),length(TrackID));
d_ZK_1  = nan(length(TimeIdx),length(TrackID));%PK1 projected on Zenital direction

for idxK = 1:length(TrackID)
    CurrentK = Tracks.Kinetochore1.TrackID == TrackID(idxK);
    
    k_t   = Tracks.Kinetochore1.TimeIdx(CurrentK);
    k_xyz = Tracks.Kinetochore1{CurrentK,{'X','Y','Z'}};
    
    K_xyz = nan(length(TimeIdx),3);
    [idxkeep, idxpos] = ismember(k_t,TimeIdx);
    idxpos = idxpos(idxkeep);
    K_xyz(idxpos,:) = k_xyz(idxkeep,:);
    
%     
%     idx = ismember(TimeIdx,k_t);
%     p_xyz = Pole1_xyz(idx,:);
%     
    PK = K_xyz-Pole1_xyz;
    d_PK = vecnorm(PK',2)';
    d_PK_1(:,idxK) = d_PK;
    
    d_ZK_1(:,idxK) = dot(u_Zenith,PK,2);
    %                 theta_1(idxK,idx) = acos(dot( u_AB(idx,:),PK,2)./d_PK);
end

AnaphaseA1 = table(nanmean(d_PK_1,2),...
    nanmedian(d_PK_1,2),...
    nanstd(d_PK_1,1,2),...    
    skewness(d_PK_1,0,2),...    
    sum(~isnan(d_PK_1),2),...
    nanmin(d_PK_1,[],2),...
    nanmax(d_PK_1,[],2),...
    TimeIdx);
AnaphaseA1.Properties.VariableNames = {'Mean','Median','StDev','Skewness','Nb','Min','Max','TimeIdx'};
tmp = array2table(d_PK_1);
tmp.Properties.VariableNames = cellfun(@num2str,num2cell(TrackID),'UniformOutput',false);
AnaphaseA1 = [AnaphaseA1 tmp];

N     = cell(height(tmp),1);
edges = cell(height(tmp),1);
for idxT = 1:height(tmp)
    [N{idxT},e] = histcounts(tmp{idxT,:},'BinWidth',SpatialBin,'Normalization','probability');
    edges{idxT} = e(1:end-1)+SpatialBin/2;
end
distbin = unique(cat(2,edges{:}));
dist_time_prob = zeros(length(distbin),height(tmp));
for idxT = 1:height(tmp)
     [a,b] = ismember(edges{idxT},distbin);
     dist_time_prob(b,idxT) = N{idxT}(a);     
end
ProbA1.distbin = distbin;
ProbA1.dist_time_prob = dist_time_prob;

ZenithA1 = table(nanmean(d_ZK_1,2),...
    nanmedian(d_ZK_1,2),...
    nanstd(d_ZK_1,1,2),...    
    skewness(d_ZK_1,0,2),...    
    sum(~isnan(d_ZK_1),2),...
    nanmin(d_ZK_1,[],2),...
    nanmax(d_ZK_1,[],2),...
    TimeIdx);
ZenithA1.Properties.VariableNames = {'Mean','Median','StDev','Skewness','Nb','Min','Max','TimeIdx'};
tmp = array2table(d_ZK_1);
tmp.Properties.VariableNames = cellfun(@num2str,num2cell(TrackID),'UniformOutput',false);
ZenithA1 = [ZenithA1 tmp];

%Unitary vectors along Pole2-Pole1 axis__________________
u_Zenith = -u_Zenith;

%Anaphase A 2__________________________________________
TrackID = unique(Tracks.Kinetochore2.TrackID);
d_PK_2  = nan(length(TimeIdx),length(TrackID));
d_ZK_2  = nan(length(TimeIdx),length(TrackID));%PK2 projected on Zenital direction

for idxK = 1:length(TrackID)
    CurrentK = Tracks.Kinetochore2.TrackID == TrackID(idxK);
    
    k_t   = Tracks.Kinetochore2.TimeIdx(CurrentK);
    k_xyz = Tracks.Kinetochore2{CurrentK,{'X','Y','Z'}};
    
    K_xyz = nan(length(TimeIdx),3);
    [idxkeep, idxpos] = ismember(k_t,TimeIdx);
    idxpos = idxpos(idxkeep);
    K_xyz(idxpos,:) = k_xyz(idxkeep,:);
    
%     
%     idx = ismember(TimeIdx,k_t);
%     p_xyz = Pole1_xyz(idx,:);
%     
    PK = K_xyz-Pole2_xyz;
    d_PK = vecnorm(PK',2)';
    d_PK_2(:,idxK) = d_PK;
    
    d_ZK_2(:,idxK) = dot(u_Zenith,PK,2);
    %                 theta_1(idxK,idx) = acos(dot( u_AB(idx,:),PK,2)./d_PK);
end
AnaphaseA2 = table(nanmean(d_PK_2,2),...
    nanmedian(d_PK_2,2),...
    nanstd(d_PK_2,1,2),...
    skewness(d_PK_2,0,2),...    
    sum(~isnan(d_PK_2),2),...
    nanmin(d_PK_2,[],2),...
    nanmax(d_PK_2,[],2),...
    TimeIdx);
AnaphaseA2.Properties.VariableNames = {'Mean','Median','StDev','Skewness','Nb','Min','Max','TimeIdx'};
tmp = array2table(d_PK_2);
tmp.Properties.VariableNames = cellfun(@num2str,num2cell(TrackID),'UniformOutput',false);
AnaphaseA2 = [AnaphaseA2 tmp];

N     = cell(height(tmp),1);
edges = cell(height(tmp),1);
for idxT = 1:height(tmp)
    [N{idxT},e] = histcounts(tmp{idxT,:},'BinWidth',SpatialBin,'Normalization','probability');
    edges{idxT} = e(1:end-1)+SpatialBin/2;
end
distbin = unique(cat(2,edges{:}));
dist_time_prob = zeros(length(distbin),height(tmp));
for idxT = 1:height(tmp)
     [a,b] = ismember(edges{idxT},distbin);
     dist_time_prob(b,idxT) = N{idxT}(a);     
end
ProbA2.distbin = distbin;
ProbA2.dist_time_prob = dist_time_prob;
           
ZenithA2 = table(nanmean(d_ZK_2,2),...
    nanmedian(d_ZK_2,2),...
    nanstd(d_ZK_2,1,2),...    
    skewness(d_ZK_2,0,2),...    
    sum(~isnan(d_ZK_2),2),...
    nanmin(d_ZK_2,[],2),...
    nanmax(d_ZK_2,[],2),...
    TimeIdx);
ZenithA2.Properties.VariableNames = {'Mean','Median','StDev','Skewness','Nb','Min','Max','TimeIdx'};
tmp = array2table(d_ZK_2);
tmp.Properties.VariableNames = cellfun(@num2str,num2cell(TrackID),'UniformOutput',false);
ZenithA2 = [ZenithA2 tmp];
end
