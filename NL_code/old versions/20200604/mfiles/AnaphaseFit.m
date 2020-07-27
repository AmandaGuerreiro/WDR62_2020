function anaphase = AnaphaseFit(t,dt,anaphase,Type)
%ANAPHASEFIT Summary of this function goes here
%   fitting of the anaphases separation
%
%   Nicolas Liaudet
%   Bioimaging Core Facility - UNIGE
%   https://www.unige.ch/medecine/bioimaging/en/bioimaging-core-facility/
%
%   20-Feb-2020

Fitmodel = fittype( @(a,b,x) a*x+b,'independent','x','coefficients',{'a','b'} );
options = fitoptions(Fitmodel);

AnaphaseOnsetIdx = anaphase.AnaphaseOnsetIdx;
t = t-t(AnaphaseOnsetIdx);
tidx = anaphase.Distances.TimeIdx;
t = t(tidx);
d = anaphase.Distances.Mean;
idxKeep = (0<=t) & (t<=dt);
t = t(idxKeep);
d = d(idxKeep);
if length(t)<=2   
    anaphase.Fit = [];
    anaphase.GoF = [];
    return
end
[t, d] = prepareCurveData(t,d);

switch Type
    case 'A1'
        options.StartPoint = [-0.5 d(1)];
    case 'A2'
        options.StartPoint = [-0.5 d(1)];
    case 'B'
        options.StartPoint = [1 d(1)];
end

[Fit, gof] = fit( t, d, Fitmodel, options);
anaphase.Fit = Fit;
anaphase.GoF = gof;
end

