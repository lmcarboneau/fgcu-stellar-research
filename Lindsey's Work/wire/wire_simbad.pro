m4_get_basedir, base

f = base + '/wire/wire_essential/xy_positions4.idl'
restore, f

n = where(wireinfo.object ne '',nn)

out = base + '/wire/wire_field/simbad_parameters.txt'
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

for i=0,nn-1 do begin
 w = where(xyinfo(n(i)).object ne '' and $
           xyinfo(n(i)).object ne 'Unknown',c)
 for j=0,c-1 do begin

  object = xyinfo(n(i)).object(w(j))
  g = strsplit(object,'&',/extract)
  printf,1,g(0)

 endfor
endfor
close,1





out2 = base + '/wire/wire_field/simbad_hd.txt'
openw,1,out2

printf,1,'format {ID HD|SAO|1}'

for i=0,nn-1 do begin
 w = where(xyinfo(n(i)).object ne '' and $
           xyinfo(n(i)).object ne 'Unknown',c)
 for j=0,c-1 do begin

  object = xyinfo(n(i)).object(w(j))
  g = strsplit(object,'&',/extract)
  printf,1,g(0)

 endfor
endfor
close,1






print,''
print,''
print,' %%% Saved file (Simbad): ' + out
print,' %%% Saved file (Simbad --> Vizier): ' + out2
print,''

print," %%% http://simbad.u-strasbg.fr/sim-flist.pl"
print,' %%% Give these two files to simbad (query by list)'
print,' %%% Save output as .out + data files (e.g. simbad_hd.out.nov2006)'
print,' %%%'
print,' %%% Give output file with HD numbers to Hipparcos (Vizier):'
print," %%% http://vizier.u-strasbg.fr/viz-bin/VizieR ---> Catalog I/239"
print,' %%%'
print,' %%% Use main catalog ... "Use LISTs of Targets" '
print,' %%%'
print,' %%% 1. DE-SELECT the "r"   ***AND***   "position" in "Compute Sort By" section'
print,' %%% 2. Select only HIP, V, TrigParAx, E-Plx and B-V as output!'
print,' %%%'
print,' %%% At the BOTTOM of the page give the file name simbad_hd.out + date'
print,' %%% (e.g. vizier_par.out.nov06)'
print,' %%%'
print,' %%% Copy the output to the file: vizier_par.out.DATE to dir wire_field/'
print,' %%% Remember to include the first star (HD 113226) also!'
print,' %%% Remember to remove comments/errors from Vizier + text at the top!'
print,' %%%
print,''
print,' %%% Then launch .r wire_combine_info'
print,''


END


