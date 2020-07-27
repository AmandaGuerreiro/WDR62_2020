function [metadata,spots] = imsread(filename)
%IMSREAD reads Imaris file 5.5
%
%   Nicolas Liaudet
%   Bioimaging Core Facility - UNIGE
%   https://www.unige.ch/medecine/bioimaging/en/bioimaging-core-facility/
%
%   25-Oct-2019

% [metadata,spots] = imsread('\\isis.unige.ch\medecine\nas02\Bioimagerie\_BIOIMAGING_ADMIN\Documents de LIAUDET\Projects\Meraldi P\PM_Guerreiro_2019\DATA\Batch1\siWDR62\siWDR62_C01d.ims');
fileID = H5F.open(filename, 'H5F_ACC_RDONLY', 'H5P_DEFAULT');

metadata = getmetadata(fileID);
[~,metadata.FileName] = fileparts(filename);
spots = getspots(fileID);

H5F.close(fileID);
end



function metadata = getmetadata(fileID)
metadata.DimX = 0;
metadata.DimY = 0;
metadata.DimZ = 0;
metadata.DimT = 0;
metadata.DimC = 0;

metadata.PosX = [0 0];
metadata.PosY = [0 0];
metadata.PosZ = [0 0];

metadata.ResX = 0;
metadata.ResY = 0;
metadata.ResZ = 0;
metadata.ResT = 0;

metadata.UnitX = 'um';
metadata.UnitY = 'um';
metadata.UnitZ = 'um';

metadata.time = 0;

metadata.ChannelName = {};

%------------------ Stack dimensions ------------------
%X voxel Nb
Image_gID     = H5G.open(fileID,'/DataSetInfo/Image/');
X_aID         = H5A.open(Image_gID,'X');
metadata.DimX = str2double(H5A.read(X_aID));
H5A.close(X_aID);
%Y voxel Nb
Y_aID         = H5A.open(Image_gID,'Y');
metadata.DimY = str2double(H5A.read(Y_aID));
H5A.close(Y_aID);
%Z voxel Nb
Z_aID         = H5A.open(Image_gID,'Z');
metadata.DimZ = str2double(H5A.read(Z_aID));
H5A.close(Z_aID);
%T time point
TimeInfo_gID    = H5G.open(fileID,'/DataSetInfo/TimeInfo/');
T_aID           = H5A.open(TimeInfo_gID,'DatasetTimePoints');
metadata.DimT   = str2double(H5A.read(T_aID));
H5A.close(T_aID);
%C channel Nb
idx_type = 'H5_INDEX_NAME';
order    = 'H5_ITER_INC';
lapl_id  = 'H5P_DEFAULT';
idxC = 0;
while H5L.exists(fileID,...
        ['/DataSet/ResolutionLevel 0/TimePoint 0/Channel ' num2str(idxC)],...
        'H5P_DEFAULT')
    idxC = idxC+1;
end
metadata.DimC = idxC;

%------------------ Stack position ------------------
%Xmin-Xmax
X_aID = H5A.open(Image_gID,'ExtMin0');
metadata.PosX(1) = str2double(H5A.read(X_aID));
X_aID = H5A.open(Image_gID,'ExtMax0');
metadata.PosX(2) = str2double(H5A.read(X_aID));
H5A.close(X_aID);
%Ymin-Ymax
Y_aID = H5A.open(Image_gID,'ExtMin1');
metadata.PosY(1) = str2double(H5A.read(Y_aID));
Y_aID = H5A.open(Image_gID,'ExtMax1');
metadata.PosY(2) = str2double(H5A.read(Y_aID));
H5A.close(Y_aID);
%Zmin-Zmax
Z_aID = H5A.open(Image_gID,'ExtMin2');
metadata.PosZ(1) = str2double(H5A.read(Z_aID));
Z_aID = H5A.open(Image_gID,'ExtMax2');
metadata.PosZ(2) = str2double(H5A.read(Z_aID));
H5A.close(Z_aID);
H5G.close(Image_gID);

%------------------ Stack resolution ------------------
metadata.ResX = diff(metadata.PosX)/metadata.DimX;
metadata.ResY = diff(metadata.PosY)/metadata.DimY;
metadata.ResZ = diff(metadata.PosZ)/metadata.DimZ;

%------------------ time parameters ------------------
TimeInfo_gID    = H5G.open(fileID,'/DataSetInfo/TimeInfo/','H5P_DEFAULT');
tmpstruct.timestamp = cell(metadata.DimT,1);
tmpstruct.idx = 1;
[~,~,tmpstruct] = H5A.iterate(TimeInfo_gID,...
    'H5_INDEX_NAME','H5_ITER_INC',2,@gettimepoints,tmpstruct);
H5G.close(TimeInfo_gID);
metadata.time= sort(datetime(tmpstruct.timestamp,...
    'InputFormat','yyyy-MM-dd HH:mm:ss.SSSSSSSSS'));
metadata.time.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSSSSS';
dt = diff(metadata.time);
dt.Format = 's';
metadata.ResT = mean(dt);

%------------------ units ------------------
Image_gID     = H5G.open(fileID,'/DataSetInfo/Image/');
X_aID         = H5A.open(Image_gID,'Unit');
metadata.UnitX = H5A.read(X_aID);
metadata.UnitY = metadata.UnitX;
metadata.UnitZ = metadata.UnitX;
H5A.close(X_aID);

%------------------ Channel names ------------------
ChannelName = cell(metadata.DimC,1);
for idxCh=1:metadata.DimC
    loc = ['/DataSetInfo/Channel ' num2str(idxCh-1) '/'];
    if H5L.exists(fileID,loc, 'H5P_DEFAULT')
        CH_gID  = H5G.open(fileID,loc);
        CHn_aID = H5A.open(CH_gID,'Name');
        ChannelName{idxCh} = H5A.read(CHn_aID);
    else
        ChannelName{idxCh} = ['Ch_' num2str(idxCh)];
    end
end
metadata.ChannelName = ChannelName;

end

function data = getdata(fileID,metadata)

end

function [status,cdata_out] = gettimepoints(obj_id,attr_name,info,cdata_in)
attr_id = H5A.open(obj_id,attr_name);
data    = H5A.read(attr_id);
cdata_in.timestamp{cdata_in.idx} = data ;
cdata_in.idx = cdata_in.idx+1;
cdata_out = cdata_in;
status = 0;
end

function spots = getspots(fileID)
%Get spots position
if ~H5L.exists(fileID,'Scene8','H5P_DEFAULT')
    spots = [];
    return
end
Content_gID = H5G.open(fileID,'/Scene8/Content/');

if ~H5L.exists(Content_gID, 'Points0', 'H5P_DEFAULT')
    spots = [];
    return
end

Content_aID = H5A.open(Content_gID,'NumberOfPoints');
NBspots = H5A.read(Content_aID);
spots = struct('Name',cell(NBspots,1),...
    'Tracks',cell(NBspots,1));

for idxSpot = 1:NBspots
    Spots_gID = H5G.open(Content_gID,['Points' num2str(idxSpot-1)]);
    
    Spots_aID = H5A.open(Spots_gID,'Name');
    Name = {H5A.read(Spots_aID)'};
    H5A.close(Spots_aID)
    
    DID = H5D.open(Spots_gID, 'Spot');
    Spot = transpose(H5D.read(DID));
    H5D.close(DID)
    
    DID = H5D.open(Spots_gID, 'TrackObject0');
    TrackObject0 = transpose(H5D.read(DID));
    H5D.close(DID)
    
    DID = H5D.open(Spots_gID, 'SpotTimeOffset');
    SpotTimeOffset = transpose(H5D.read(DID));
    H5D.close(DID)
    
    DID = H5D.open(Spots_gID, 'Track0');
    Track0 = transpose(H5D.read(DID));
    H5D.close(DID)
    H5G.close(Spots_gID)
    
    alltidx = repelem(SpotTimeOffset.ID+1,SpotTimeOffset.IndexEnd-SpotTimeOffset.IndexBegin);
    
    
    TrackID = cell(length(Track0.ID),1);
    SpotID  = cell(length(Track0.ID),1);
    X       = cell(length(Track0.ID),1);
    Y       = cell(length(Track0.ID),1);
    Z       = cell(length(Track0.ID),1);
    TimeIdx = cell(length(Track0.ID),1);
    for idxTrack = 1:length(Track0.ID)
        
        
        idxBegin = Track0.IndexTrackObjectBegin(idxTrack)+1;
        idxEnd   = Track0.IndexTrackObjectEnd(idxTrack);
        
        idx_SpotInTrack   = ismember(Spot.ID,TrackObject0.ID_Object(idxBegin:idxEnd));
        
        TrackID{idxTrack} = repmat(Track0.ID(idxTrack),[sum(idx_SpotInTrack) 1 ]);
        SpotID{idxTrack}  = Spot.ID(idx_SpotInTrack);
        X{idxTrack}       = Spot.PositionX(idx_SpotInTrack);
        Y{idxTrack}       = Spot.PositionY(idx_SpotInTrack);
        Z{idxTrack}       = Spot.PositionZ(idx_SpotInTrack);
        TimeIdx{idxTrack} = alltidx(idx_SpotInTrack);
        
    end
    TrackID = cat(1,TrackID{:});
    SpotID  = cat(1,SpotID{:});
    X       = cat(1,X{:});
    Y       = cat(1,Y{:});
    Z       = cat(1,Z{:});
    TimeIdx = cat(1,TimeIdx{:});
    
    Tracks = table(TrackID,...
        SpotID,...
        X,...
        Y,...
        Z,...
        TimeIdx);       
    Tracks = sortrows(Tracks);
    
    spots(idxSpot).Name = Name;
    spots(idxSpot).Tracks = Tracks;
    
end

end
    




