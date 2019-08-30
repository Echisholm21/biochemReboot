%B_batch_01064
%Controle de qualité des données bouteilles de la mission IML0164

%Lire le fichier BTL_01064.txt (séparateur ;)
%B=B_read_btl_txtfile('test_iml.txt');

% This is a batch file that would QC all data in the folder
% For now it is all 27 AZMP cruises
%used for oxygen recovery project (E. Chisholm 2018)
%used for groundfish reboot (E. Chisholm 2019)

% pathe where the files are
%filepath='C:\Gordana\Biochem_reload\working\IML_QC';
filelist=dir('C:\Users\ChisholmE\Documents\IML QC Working\IML_QC'); % filenames actually start at position 3 of filelist(3).name 
%fn='80029_IML_format.txt';
%data_btl;

%%error list (GROUNDFISH)
%%NED2003002 (fixed) NaN dates present, can't read txt file
%%NED2012002 (fixed) error in create btl (reshape) - found random text
%%%      string
%%NED2013022 (fixed) error in create btl, reshape -rogue text strings
%%TEL2005545 (fixed) NaN dates
%%TEL2005605 (fixed) NaN dates
%%TEL2005633 (fixed) NaN dates
%%TEL2006614 (fixed) NaN dates
%%TEM2004004 (fixed) NaN lon values
%%TEM2007686 (fixed) NaN dates
%%TEM2008775 (fixed) NaN dates

%%Version 2 GroundFish
%%%NED2010027 (fixed) removed set 130 and 131 (CTD failure) --- i = 23 & 24
%%%NED2012002 (fixed) text string - "nutrient results missing?" --- i = 29
%%%NED2013022 (fixed) text strings - 'dnf', line 213 --- i = 35 & 36 (no
%%%ctd data?)
%%%TEL2005545 (fixed) date error - removed set 36 and 6, no data or date
%%%--- i = 41 & 42

for i=  42:length(filelist)        %file names start at position 3
 fn=filelist(i).name; %this is a file name with the path that has to be read
 
B=B_read_btl_txtfile(fn);

%remove last column CTD data to avoid error
%B.data = B.data(:,1:14);

%Structure du controle de qualité
Q=B_create_btl(B,'cgdg');

%Controle de qualité
Q=B_control_Q_GL(Q);

%Mise a jour des flags Q dans B
B=B_addQ2btl(B,Q);

%Écriture du fichier QC_BTL_01064.txt
B_write_btl(B);

end
