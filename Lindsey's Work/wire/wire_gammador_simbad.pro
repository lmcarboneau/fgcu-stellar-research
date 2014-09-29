; Prepare query at simbad (as file)
; adapted from wire_simbad.pro

; (c) April 7th 2005 by Hans Bruntt
;

mode = 'gammador'
mode = 'corot_atype'

; ==================================================
if mode eq 'gammador' then begin
 out = '~/VWA/MEGA/info/simbad_gamma_parameters.txt'
 out2 = '~/VWA/MEGA/info/vizier_gamma_hipparc.txt'
 infile = '~/VWA/MEGA/info/fund_gamma.dat'
endif
; ==================================================

; ==================================================
if mode eq 'corot_atype' then begin
 out  = '~/VWA/MEGA/info/simbad_corot_atype_parameters.txt'
 out2 = '~/VWA/MEGA/info/vizier_corot_atype_hipparc.txt'
 infile = '~/VWA/MEGA/info/fund_corot_atype.txt'
endif
; ==================================================

; ==================================================
openw,1,out

printf,1,'format {ID HD|SAO|1'
printf,1,'TEXT " ^ "'
printf,1,'SP 10'
printf,1,'TEXT " ^ "'
printf,1,'MAG '
printf,1,'TEXT " ^ "'
printf,1,'MES A|Hbet1 M'
printf,1,'TEXT " ^ "'
printf,1,'MES A|uvby1|uvby M'
printf,1,'TEXT " ^ "'
printf,1,'MES A|ROT M'
printf,1,'TEXT " ^ "'
printf,1,' JCOO (30) ICRS "%Ca %Cd" 2000/2000}'

if mode eq 'gammador' then $
readcol,infile,hd,a,b,c,d,e,f,teff,logg,i,j,k, $
 format='A,F,I,F,I,F,F,I,F,F,F,F'

if mode eq 'corot_atype' then $
readcol,infile,hd,a,b,c,teff,logg,feh,i,j, $
 format='A,F,I,F,I,F,F,I,F,F,F,F'

; plot,teff,logg,psym=2

nn = n_elements(hd)
for pp=0,nn-1 do printf,1,hd(pp)
close,1
; ==================================================

; ==================================================
openw,1,out2
;; printf,1,'format {ID HD|SAO|1}' ; only for WIRE

for i=0,nn-1 do printf,1,hd(i)
close,1
; ==================================================




x = strsplit(out,'.',/extract)
out_a = x(0) + '.out'

x2 = strsplit(out2,'.',/extract)
out2_a = x2(0) + '.out'

; ==================================================
print,''
print,' %%% Saved file (Simbad): ' + out
print,' %%% Saved file (Simbad --> Vizier): ' + out2
print,''

print," %%% Go to: http://simbad.u-strasbg.fr/sim-flist.pl"
print,' %%% Give '+out+' to simbad (query by list)'
print,' %%% Save output from simbad as: ' + out_a
print,' %%% 
print," %%% Go here: http://vizier.u-strasbg.fr/viz-bin/VizieR ---> Catalog I/239"
print,' %%% Select "I/239/hip/main"'
print,' %%% Instructions for Vizier: 
print,' %%%  Use main catalog = "Use LIST of Targets" (near the bottom) '
print,' %%%  Deselect the "r" in field "Output preferences for Position"'
print,' %%%  Select *ONLY* HIP, V, TrigParAx, E-Plx and B-V as output!'
print,' %%% Give vizier/hipparcos input file: ' + out2
print,' %%% Save output as file: ' + out2_a


print,''
print,' %%% Then launch .r wire_gamma_combine_info'
print,''



END
