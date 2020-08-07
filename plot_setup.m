set(groot,'defaultLineLineWidth',3);  %Makes default lines thicker
set(groot,'DefaultFigureColor',[1 1 1]); %Makes the background colour white, which is useful for the export_fig package as it trims out white backgrounds.
set(groot,'DefaultFigureUnits','Normalized'); %Changes figure units so they are proportional to the monitor size you are using. Namely, instead of faffing around with absolute pixel sizes you can produce relative sizes.
set(groot, 'DefaultFigurePosition',[40 40 1/4 1/3]); %Sets the figure position so it isn?t slap bang in the middle of the screen.
set(0,'defaultAxesFontSize',20)
set(groot, 'defaultAxesTickLabelInterpreter','latex'); 
set(groot, 'defaultLegendInterpreter','latex');
