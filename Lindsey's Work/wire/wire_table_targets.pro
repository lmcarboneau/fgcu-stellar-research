; ------------------------------------------------------------
; Make a table of the targets observed with WIRE
; Nov 2006, HB
; ------------------------------------------------------------

; Try using wire_target_plot / plot2 ?

vlimit = 4.55 ; 5.1
glimit = 1 ; set to 1 to print giants
glimit = 0 ; set to 0 to only print main sequence stars (lum class V + IV)

debug = 1B
add_paper_comments = 1B ; print info on papers from ADS

omicron = '$o$'

m4_get_basedir,base
outname = base+'wire_process/wire_params_Nov2006.idl' ; wireobj3 = wire_comp_teff_calibr (216 targets)
restore,outname

wire_target_info, base + 'wire/wire_essential/targets_wire.txt', info  ; 65 targets,              Nov 2006
restore,          base + 'wire/wire_essential/wire_sec_info.idl'       ; wireobj = 216 bojects 13 Nov 2006

;w = where(strmatch(wireobj3.spec,'*B*') and wireobj3.class ge 4.,c)
;w = where(wireobj3.v lt 5.0,c) ; about 119 targets
;for i=0,c-1 do print,wireobj3(w(i)).hd, wireobj3(w(i)).v, wireobj3(w(i)).bv, wireobj3(w(i)).par,$
; wireobj3(w(i)).spec1, $
; format='(I8, F9.2, F9.3, F9.2, A8)'

s = sort(wireobj3.teff)
s = sort(wireobj3.v)
wireobj4 = wireobj3(s)
w = where(wireobj4.v le vlimit,c) ; about 119 targets

wms = where(wireobj4.v le vlimit and (wireobj4.lumclass ge 4 or wireobj4.lumclass eq 0),cms) 
wg  = where(wireobj4.v le vlimit and wireobj4.lumclass le 3 and wireobj4.lumclass ne 0, cg) 

wireobj4g  = wireobj4(wg)
wireobj4ms = wireobj4(wms)

 ind = ['Theta','Kappa','kappa','Beta','beta','Alpha','alpha','alf','Lambda','gamma','Gamma','epsilon','Epsilon','delta','Ksi','Zeta','Nu','Sigma']
 sub = ['!4t!3','!4j!3','!4j!3','!4b!3','!4b!3','!4a!3','!4a!3','!4a!3','!4k!3','!4c!3','!4c!3','!4e!3','!4e!3','4d!3','!4n!3','!4f!3','!4m!3','!4r!3']
 ni = n_elements(ind)

; NOTE THAT ksi in SIMBAD is translated to \xi in latex:
; ALSO, omicron is simply lower case "o" (searched infon on this on google)

     ind2 = ['alpha',  'alf',    'bet',   'gam',    'del',    'eps',   $
             'kap',    'ksi',  'zet',   'tau', 'phi',  'eta', 'omi',      $
             'rho',  'ups',     'sig', 'sigma', 'nu',  'lam', 'iot' ]
 sublatex = ['\alpha', '\alpha', '\beta', '\gamma', '\delta', '\epsilon', $
             '\kappa', '\xi', '\zeta', '\tau','\phi', '\eta', 'o',$
             '\rho', '\upsilon','\sigma', '\sigma', '\nu','\lambda','\iota' ]
     nind = n_elements(ind2)

paper_hd  = [112185, 187642,128620,125473, 61421, 160578, $
             111123,28319 , 95689, 158926,2151, $
             40183, 221253, 211336, 124897 ]
paper_ref = ['Retter et al.\ 2004 ApJ, 601, L95', 'Buzasi et al.\ 2005, ApJ, 619, 1072',$
             'Fletcher S. T. et al.\ 2006, MNRAS, 371, 935','Bruntt et al. 2006, A&A, 456,651',$
             'Bruntt et al.\ 2005, ApJ, 633, 440', 'Cuypers et al.\ 2004, ASP Conf. Proc. 310, 251',$
             'Cuypers J.\ et al., 2002, A&A, 392, 599','Poretti, E. et al. 2002, A&A, 382, 157',$
             'Buzasi et al.\ 2000, ApJ, 532, 133','Bruntt et al.\ 2007b, in preparation',$
             'Karoff et al.\ 2007, these proceedings', $
             'Southworth et al. 2007 in prep.','Bruntt et al. 2007b in prep.', $
             'Bruntt et al.\ 2006a, A\&A, in press',$
             'Retter et al.\ 2003, ApJ, 591L, 151'] 
paper_name = ['eps UMa', 'Altair', 'alpha Cen', 'Psi Cen', 'Procyon', 'Kappa Sco', $
             'Beta Cru','Theta 2 Tau', 'Alpha UMa','Lam Sco', 'Beta Hyi', $
             'Beta Aur','AR Cas','Eps Cep', 'Arcturus']
paper_short = ['Retter04', 'Buzasi05', 'Fletcher06','aBruntt06', 'Bruntt05',$
               'Cuypers04','Cuypers02','Poretti02', 'Buzasi00', 'aBruntt07', 'Karoff07',$
               'Southworth07a','bBruntt07','bBruntt06','Retter03' ]


notes_hd = [173524]
notes_txt = ['SB2 perculiar star. See Tsymbal Astr. Letters, 24, 90']

excep1 = [164058        ,186791,          47205,         27371,         $
          188512,        128620,           2151,        121370, $
          61421,                     161471,      187642,$
          106591,         30211,      133242, 175191,$
          29248,     158926, 143018, 205021,$
          44743,       143275, 8890,113226, $
          124897, 71129, 161892, 74195, $
          148367,128345, 40932, 130807, 126354, 126341, $
          120136,75311,162003,160922, $
          74560, 159560, 154494, 221253, 173524, $
          151769, 162579, 29388, 159433, 40239, 125823, 206952, $
          47442,5848,120477, 196171, 213306, $
          39801, 95689,111123, 158408, 149757, $
          432,129056, 131873, 160578,$
          28319, 36389, 37202 ]
          
excepn = ['$\gamma$ Dra', '$\gamma$ Aql', '$\nu^2$ CMa', '$\gamma$ Tau',$
          '$\beta$ Aql' ,'$\alpha$ Cen','$\beta$ Hyi','$\eta$ Boo', $
          '$\alpha$ CMi$^a$', '$\iota^1$ Sco','$\alpha$ Aql$^b$',$
          '$\delta$ UMa', '$\mu$ Eri','$\pi$ Lup','$\sigma$ Sgr', $  
          '$\nu$ Eri','$\lambda$ Sco','$\pi$ Sco','$\beta$ Cep',$
          '$\beta$ CMa', '$\delta$ Sco','$\alpha$ UMi$^d$','$\epsilon$ Vir',$
          '$\alpha$ Boo$^c$', '$\epsilon$ Car', 'G Sco',omicron+' Vel',$
          '$\upsilon$ Oph','$\rho$ Lup','$\mu$ Ori',omicron+' Lup','$\tau^2$ Lup','$\tau^1$ Lup',$
          '$\tau$ Boo', 'f Car', '$\psi^1$ Dra','$\omega$ Dra',$
          'HY Vel', '$\nu^2$ Dra', '60 Her', 'AR Cas', '46 Dra', $
          '20 Oph', '30 Dra', '90 Tau','Q Sco','$\pi$ Aur', 'V761 Cen','11 Cep',$
          '$\nu^3$ CMa','$-$','$\upsilon$ Boo','$\alpha$ Ind', '$\delta$ Cep', $
          '$\alpha$ Ori', '$\alpha$ UMa','$\beta$ Cru','$\upsilon$ Sco','$\zeta$ Oph',$
          '$\beta$ Cas','$\alpha$ Lup', '$\beta$ UMi', '$\kappa$ Sco',$
          '$\theta^2$ Tau', 'CE Tau', '$\zeta$ Tau' ]

; ------------------------------------------------------------
; SANITY CHECK:
; ------------------------------------------------------------
if n_elements(excep1)   ne n_elements(excepn)      then stop
if n_elements(paper_hd) ne n_elements(paper_short) then stop
; ------------------------------------------------------------

; beta Cru = HD 111123
; altair   = HD 187642

;; \multicolumn{4}{l}{$a$ Procyon}\\
;; \multicolumn{4}{l}{$b$ Altair}\\
;; \multicolumn{4}{l}{$c$ Arcturus}
;; \multicolumn{4}{l}{$d$ Polaris}\\
;; \multicolumn{4}{l}{$e$ Betelgeuse}\\ Alpha Ori
       
; Notes:
; 47205: nu2 CMa or "7 CMa"
; 61421: Alpha CMi or "10 CMi" or Procyon
; 5848: no special name, perhaps use: 'HR 285'



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Names of evolved stars:
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nam2g = strarr(cg)
for i=0,cg-1 do begin
 addnam = ''
 for k=0,nind-1 do begin
   wgreek = where(strmatch(wireobj4g(i).nam,'*'+ind2(k)+'*',/fold),cgreek)
   if cgreek eq 1 then addnam = sublatex(k)  
 endfor

 if addnam ne '' then begin
  x = strsplit(wireobj4g(i).nam,' ',/extract)
  nx = n_elements(x)
  if nx eq 3 then nam2g(i) = '$' + addnam + '$ ' + x(2)
  if nx eq 2 then nam2g(i) = '$' + addnam + '$ ' + x(1)
 endif else nam2g(i) = wireobj4g(i).nam


 wex = where(wireobj4g(i).hd eq excep1,cex) ; special treatment for some names
 if cex eq 1 then nam2g(i) = excepn(wex)

 if add_paper_comments then begin
   wpap = where(wireobj4g(i).hd eq paper_hd,cpap)
   if cpap eq 1 then nam2g(i)=nam2g(i) + ' PUB=' + paper_short(wpap)
 endif

endfor
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; MS STARS:
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nam2ms = strarr(cms)
for i=0,cms-1 do begin
 addnam = ''
 for k=0,nind-1 do begin
   wgreek = where(strmatch(wireobj4ms(i).nam,'*'+ind2(k)+'*',/fold),cgreek)
   if cgreek eq 1 then addnam = sublatex(k)  
 endfor

 if addnam ne '' then begin
  x = strsplit(wireobj4ms(i).nam,' ',/extract)
  nx = n_elements(x)
  if nx eq 3 then nam2ms(i) = '$' + addnam + '$ ' + x(2)
  if nx eq 2 then nam2ms(i) = '$' + addnam + '$ ' + x(1)
 endif else nam2ms(i) = wireobj4ms(i).nam

 wex = where(wireobj4ms(i).hd eq excep1,cex) ; special treatment for some names
 if cex eq 1 then nam2ms(i) = excepn(wex)

 if add_paper_comments then begin
   wpap = where(wireobj4ms(i).hd eq paper_hd,cpap)
   if cpap eq 1 then nam2ms(i)=nam2ms(i) + ' PUB=' + paper_short(wpap)
 endif
endfor
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if debug then begin
 print,' *********** MAIN SEQUENCE STARS ******************* '
for i=0,cms-1 do print,wireobj4ms(i).hd, wireobj4ms(i).v, $
 round(wireobj4ms(i).teff/10.)*10, wireobj4ms(i).logg, $
 wireobj4ms(i).spec1, wireobj4ms(i).lumclass, nam2ms(i), wireobj4ms(i).nam, $
 format='(I8, F5.1, I6, F6.1, X, A8, X, I4, A15,X,A15)'
print,''
 print,' *********** EVOLVED STARS ******************* '
for i=0,cg-1 do print,wireobj4g(i).hd, wireobj4g(i).v, $
 round(wireobj4g(i).teff/10.)*10, wireobj4g(i).logg, $
 wireobj4g(i).spec1, wireobj4g(i).lumclass, nam2g(i), wireobj4g(i).nam, $
 format='(I8, F5.1, I6, F6.1, X, A8, X, I4, A15,X,A15)'
endif

m4_get_basedir, base
 outname = base + 'papers/conferences/Vienna2006/proceedings/' + 'wire_targets_table.tex'

;if glimit eq 0 then $
; outname = base + 'papers/conferences/Vienna2006/proceedings/' + 'wire_targets_table_MS.tex'
;if glimit eq 1 then $
; outname = base + 'papers/conferences/Vienna2006/proceedings/' + 'wire_targets_table_giants.tex'


;  '&',round(wireobj4ms(i).teff/10.)*10, '&',wireobj4ms(i).logg, '&', $



openw,1,outname

cmax = max([cg,cms])
for i=0,cmax-1 do begin
  if i lt cms then $
  msout = string(nam2ms(i),format='(A25)')        + '&' + $
          string(wireobj4ms(i).hd,format='(I6)')  + '&' + $
          string(wireobj4ms(i).v,format='(F5.1)') + '&' + $
          string(wireobj4ms(i).spec1,format='(A8)') else $
  msout = string(' ',format='(A25)')  + '&' + $
          string(' ',format='(A6)')   + '&' + $
          string(' ',format='(A5)') + '&' + $
          string(' ',format='(A8)')

  if i lt cg then $
  gout = string(nam2g(i),format='(A25)')        + '&' + $
          string(wireobj4g(i).hd,format='(I6)')  + '&' + $
          string(wireobj4g(i).v,format='(F5.1)') + '&' + $
          string(wireobj4g(i).spec1,format='(A8)') else $
  gout = string(' ',format='(A2)')  + '&' + $
          string(' ',format='(A6)')   + '&' + $
          string(' ',format='(A5)') + '&' + $
          string(' ',format='(A8)') 

  printf,1, msout + '   &  ' + gout + '  \\'
endfor

close,1
print,' %%% Wrote file: ' + outname

; openw,1,outname
; for i=0,c-1 do printf,1,$
;  wireobj4(i).hd,'&', wireobj4(i).v, '&',round(wireobj4(i).teff/10.)*10, '&',wireobj4(i).logg, '&', $
;  wireobj4(i).spec1, '&', nam2(i), ' \\', $
;  format='(I8, A3, F5.1, A3, I6, A3, F6.1, A3, A8, A3, A25, A3)'
; close,1
; print,' %%% Wrote file: ' + outname


END
