PRO wire_density_all, wildcard, fmax=fmax, fmin=fmin, $
 orb=orb, outdir=outdir, nmerge=nmerge, cnt_start=cnt_start, highres=highres

; orb: remove freq. near orbital periods?
default9, orb,  0B
default9,  df, 1e-6  ; do not remove orb. freq!
if orb then df = 20. ; remove orb. freq!

default9, fmin, 20.
default9, nmerge, 5  ; nmerge = 5 is default!
default9, highres, 1.

if n_elements(fmax) eq 0 then fmax = 25000.

spawnrob,'ls -1 ' + wildcard, list
nl = n_elements(list)
if nl le 0 then RETURN

procyon = 'unknown data source'
if strmatch(wildcard,'*2000*') then procyon = '2000'
if strmatch(wildcard,'*1999*') then procyon = '1999'
if strmatch(wildcard,'*mostfirst10*') then procyon = 'mostfirst10'
if strmatch(wildcard,'*procyon2005*') then procyon = 'procyon2005'

default9, cnt_start, 0L ; Counter in list of files

for i=cnt_start,nl-1 do begin

fil = list(i)

outname = fil + '.density'

if n_elements(outdir) ne 0 then begin
 if i eq 0 then spawnrob,'mkdir ' + outdir ; create the output dir!
 s5 = strsplit(fil,'/',/extract) & ns5 = n_elements(s5)
 n5 = s5(ns5-1) ; last part of name
 outname = outdir + '/' + n5 + '.density'
endif

print,' %%% File ' + strcompress(i+1) + $
        ' out of ' + strcompress(nl) + $
        ' - Filename: ' + outname

if procyon eq '1999' then begin

; 6.8641101

; microHz pr resolution element:
if nmerge eq 31 then density_use = 7.7077551D ; merge=31
if nmerge eq  5 then density_use = 6.8641101D ; merge = 5 ; 02DEC2004

density_use = 7.2942352 ; 18 FEB 2005 -- new lc, nmerge=31 + cleaned

 wire_density_analysis,fil,psim, df = df, $
   fmax=fmax, forb=173.68056,fmin=fmin, density=density_use, highres=highres
 save,filename=outname, psim
endif


if procyon eq '2000' then begin

 if nmerge eq 31 then density_use = 7.43117D
 if nmerge eq  5 then density_use = 6.7961986 ; microHz ; 03 DEC 2004

 density_use = 7.0230603 ; 18 FEB 2005 -- new lc, nmerge=31 + cleaned

 wire_density_analysis,fil,psim, df = df, $
   fmax=fmax, forb=174.23611,fmin=fmin,density=density_use
 save,filename=outname, psim
endif


if procyon eq 'mostfirst10' then begin
 density_use = 1.1361857D ; 15. Marts 2005 HB
 wire_density_analysis,fil,psim, df = 1e-6, $
   fmax=fmax, forb=174.23611,fmin=fmin,density=density_use
 save,filename=outname, psim
endif

;   wire_density_analysis, lc, cen, fmax=fmax, forb=forb, $
;    debug=debug, fmin=fmin, $
;    fskip=fskip, skip2=skip2, skip3=skip3, df=df, density=density, $
;    skip_smooth=skip_smooth, highres=highres, $
;    dont_sub_median=dont_sub_median

if procyon eq 'unknown data source' then begin
 forb = 178.2
 wire_density_analysis,fil,psim, df = 1e-6,fmax=fmax, forb=forb,fmin=fmin

 save,filename=outname, psim
endif

if procyon eq 'procyon2005' then begin
 forb_use = 178.2
 density_use = 1.9611307 ; 13th of June 2005
 wire_density_analysis,fil,psim, $
  df = 1e-6,fmax=fmax, forb=forb_use,fmin=fmin,density=density_use
 save,filename=outname, psim
endif


endfor



; FORMAT:
;wire_density_analysis, lc, cen, fmax=fmax, forb=forb, debug=debug, fmin=fmin, $
; fskip=fskip, skip2=skip2,df=df

END
