PRO wire_bin_ts,fname,pporb,out,outorg, debug=debug,accept_single=accept_single,lim=lim,wei=wei,truewei=truewei

; Bin wire time series in the file: fname (3 columns: time, data, weight)
; wire_bin_ts,'/mnt/WinC/linux/wire/wire_lc/wire_lc_AlphaOri_Sep2004_star_4.dat',3,d,d2,/debug

; For GB data - set lim = large number (>number of days of
;               observations), set pporb=number of bins!


default9, accept_single, 0B
default9, usewei, 0B
default9, wei, 0B ; was weights supplied?
default9, truewei, 0B ; if weights are 1/sigma, then rr6=point errors will be the weighted error!

if n_elements(debug) eq 0 then debug = 0B
col=getcolor(/load)

readcol, fname, t, d, w, format='D,F,F', /silent

if n_elements(wei) eq n_elements(t) then begin
  w = wei ; weights was supplied
  print,' %%% Point weights supplied -- they will be used when binning data points!'
  usewei = 1
endif

rr = robust_sigma(d)
mm = median(d)
np = n_elements(t)

out = replicate( {t:-100D, d:-99.9, s:99.9}, np) ; output structure
cnt = 0L
outorg = replicate( {t:-100D, d:-99.9, w:0.},np)
outorg.t = t
outorg.d = d
outorg.w = w

ts  = dblarr(np)
ts(0:np-2) = t(1:np-1) - t(0:np-2) ; steps in time
ts(np-1)  = 1e-4 ; last data point

default9, lim, 0.02

wj = where(ts gt lim,c) ; jumps in time?
if c ge 0 then begin
 print,' %%%% Groundbased data! '
; hitme,mess=' %%% Groundbased data? ',s9
; if s9 eq 'y' then begin
   c=1
   wj=n_elements(t)-1
; endif 
endif

if debug then print,' %%% ' + strcompress(c) + ' orbits detected...'

ptp_robust_fin,d,rr_default,1
rr_default = rr_default * 1.5

for i=0L,c-1 do begin

if i eq 0 then w1 = 0 else w1 = wj(i-1)+1
w2 = wj(i)

time = t(w1:w2)
dat  = d(w1:w2)
; help,w1,w2 ; good for debugging

maxt = max(time)
mint = min(time)
tbox = fltarr(2,pporb) ; start + slut times for each slot
tbox(0,*) = min(time) + (maxt-mint) * findgen(pporb)/(pporb)
tbox(1,*) = min(time) + (maxt-mint) * findgen(pporb)/(pporb) + (maxt-mint)/(pporb)

print,' %%% time box size in minutes: ',(maxt-mint)/(pporb) * 24. * 60.

if debug then begin
 plot,t,d,psym=3,yr=[-1,1]*6.*rr + mm,xr=[mint-.005,maxt+.005]
 oplot,time,dat,psym=6,symsi=.5,col=col.red

; for j=0,pporb-1 do plots,tbox(0,j),!y.crange
; for j=0,pporb-1 do plots,tbox(1,j),!y.crange,line=2,thick=2

 hitme,s999 & if s999 eq 'x' then stop
endif

for j=0,pporb-1 do begin
  tmean = avg(tbox(0:1,j))
  if j eq 0 or j eq (pporb-1) then begin
   if j eq 0 then wg = where(time ge (tbox(0,j)-0.0001) and time le tbox(1,j),cg)
   if j eq (pporb-1) then wg = where(time ge (tbox(0,j)) and time le (tbox(1,j)+0.0001),cg)
  endif else $
   wg = where(time ge tbox(0,j) and time le tbox(1,j),cg)

 if cg ge 5 then begin
    resistant_mean, dat(wg), 3, dmean, sd, nr
    rr6 = robust_sigma(dat(wg)) 

    if usewei then begin $ ; $ ; use point weights instead !!
       dmean = total(wei(wg) * dat(wg)) / total(wei(wg))
       tmean = total(wei(wg) * time(wg)) / total(wei(wg))
       if truewei then rr6 = 1./total(wei(wg))
    endif

; Debug:
;print,wei(wg)/max(wei(wg))
;plot,time(wg),dat(wg),psym=2,yr=[-1,1]*.05,xr=median(time(wg))+[-1,1]*0.03
;oplot,time,dat,psym=3
;plots,tmean,dmean,psym=7,col=col.red,thick=3
;hitme,s9 & if s9 eq 'x' then stop

 endif 

 if cg ge 2 and cg le 4 then begin
  dmean = avg(dat(wg))
  rr6 = stdev(dat(wg))

    if usewei then $ ; use point weights instead !!
       dmean = total(wei(wg) * dat(wg)) / total(wei(wg))
       if truewei then rr6 = 1./total(wei(wg))
    endif

 if wg(0) eq -1 then goto,bad_data

if cg le 1 then begin ; print, ' %%% Corrupt data in wire_bin_ts.pro: '
  if (cg eq 1 and accept_single) then begin
      dmean = dat(wg) 
      rr6 = rr_default ; default noise point 
      plots,time(wg),dat(wg),psym=4,col=col.sky
  endif else goto,jump_group
endif


 if debug and cg ge 2 then begin
   oplot,time(wg),dat(wg),psym=4
   plots,tmean, dmean, psym=7,col=col.green,symsi=2
   help,wg, nr
   hitme, s89 & if s89 eq 'x' then stop
 endif

 out(cnt).t = tmean
 out(cnt).d = dmean
 out(cnt).s = rr6

 cnt = cnt + 1
bad_data:
jump_group:

endfor ; next sub-orbit group of points

endfor                          ; next orbit 

out = out(0:cnt-1) ; remove unused entries in output array



END
