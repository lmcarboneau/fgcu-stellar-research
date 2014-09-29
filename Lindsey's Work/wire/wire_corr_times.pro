PRO wire_corr_times, redfile, target, addt=addt

; addt is need for old reductions

; Autumn 2005: by comparing three different variable stars
; observed with SMEI or MOST satellite, I found that WIRE
; times must be offset by exactly -1.5 days, ie. addt=-1.5
; is recommended.
if n_elements(addt) eq 0 then addt = 0D

; Example:
; redfile = '~/wire/collection/org/AlphaAra_Sep2004_31.idl'
; target  = 'AlphaAra_Sep'

; Correct the JD times to HJD times !!
; This is done after wire_merge_fin.pro

; Example:
; wire_corr_times, '~/wire/collection/AlphaAra_Sep2004_31.idl','AlphaAra_Sep'

wire_red_setup, position_file, target_file
 restore,position_file
 wire_target_info, target_file, info

g = strsplit(target,'_',/extract)
short_name = g(0)

w = where(info.object eq short_name,c)
if c ne 1 then $
 w = where(strmatch(info.object,short_name+'*') eq 1,c) ; ProcyonF5..
if c ne 1 then begin
 print, ' *** Target name not found: ' + target, w
 stop
endif 

ra  = info(w).ra2
dec = info(w).dec2


hjd_converted = 'No!'
restore,redfile
if hjd_converted eq 'Yes!' then begin
 print,' %%% Apparently, JD have been converted to HJD already!'
 stop
endif

wg = where(wireult.hjd gt 50000 and wireult.hjd lt 57000.,cg)
jd = wireult(wg).hjd ; The julian date

jd = jd + addt ; needs +0.5 days for data before late-2003  !! (??)

hjd_temp = helio_jd( jd, ra, dec)     ; Heliocentric Julian Date

; =============================
wireult(wg).hjd = hjd_temp
; =============================

plot,hjd_temp-53000.,(hjd_temp-jd)*86400.,psym=3, $
 xtit='HJD - 53000.0',ytit='HJD - JD [Seconds]',$
 tit='Converting JD --> HJD for ' + target
wait,2

t0 = double( ceil(median(jd)) )

np = ceil(n_elements(jd)/100) * 100
pp = findgen(np/100) * 100

s  = poly_fit(jd(pp)-t0,hjd_temp(pp)-t0,2,myfit,sig)

si = poly_fit(hjd_temp(pp)-t0,jd(pp)-t0,2,myfit,sig) ; HJD --> JD

f = findgen(100) - 50.
tc = si(0) + si(1) * f + si(2) * f^2. ; 2nd order gives factor 2 better fit than 1st order fit!

plot,hjd_temp-t0,jd-t0,psym=3,ysty=3
oplot,f,tc,line=5,thick=2

wait,1.5

jd_new = si(0) + si(1) * (hjd_temp-t0) + si(2) * (hjd_temp-t0)^2.0
jd_new = jd_new + t0

plot,jd-53000D,(jd - jd_new) * 86400.,tit='Difference in seconds',$
 xtit='JD - 53000',ytit='JD - JD (converted)',psym=3

print,' %%% Conversion of HJD back to JD for ' + target
print,' t0 = ' + strcompress(t0,/remove_all) + 'D & hjd = wireult.hjd'
print,' si = [' + strcompress(string(si(0),format='(D18.14)')) + 'D,' + $
                  strcompress(string(si(1),format='(D18.14)')) + ',' + $
                  strcompress(string(si(2),format='(D18.14)')) + ']'

print,' jd_new = t0 + si(0) + si(1) * (hjd-t0) + si(2) * (hjd-t0)^2.0'
print,' plot,jd-53000D,(jd_new-jd)*86400.,psym=1, ytit="JD - JD(new) [secs]",xtit="JD-53000"'

; ==================================
hjd_converted = 'Yes!'
wireult.hjd = hjd_temp

; Note, January 2006: addt added to filename; reductions *after* Procyon 2005 run!
outfile = redfile + '.t' + strcompress(string(addt,format='(F9.1)'),/remove_all) + '.hjd'

save,filename = outfile, wireult, hjd_converted
print,' %%% Saved file: ' + outfile
; ==================================

END
