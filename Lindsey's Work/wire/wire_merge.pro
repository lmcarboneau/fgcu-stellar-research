; Purpose: Merge wire data ... (run after wire_pos.pro)

ap_number = 5 ; use 5 or 6 (4 is too small) ; use program wire_test.pro to determine ap. size
star = 1 ; number of the star to extract
n_max = 10000. * 15.

cnt3 = 0L ; counter

input_dir = '/ai39/bruntt/wire/altair/*.idl1'

if n_elements(col) eq 0 then col=getcolor(/load)

spawn,'ls -1 '+input_dir,aa
na = n_elements(aa)
if na eq 0 or aa(0) eq '' then stop

if strmatch(input_dir,'*altair*') eq 1 then $
 ra_dec = [ [19,50,47.00],[+08,52,06.0] ]

if n_elements(ra_dec) eq 0 then begin
 print,' *** Missing RA/DEC information to compute HELIOCENTRIC times!'
 stop
endif

ra_deg  = ten(ra_dec(*,0))*15.
dec_deg = ten(ra_dec(*,1))

print,' RA = ',ra_deg,' -- DEC = ',dec_deg

conv = (1e6 / 86400.) 
minfreq = 0. & maxfreq = 10. ; approx. cycles pr. day !!!
amp = 1.0 & freq = findgen(10) & amp = freq & noise_amp = 5.0

; The time stamp that you noticed is in the format YYMMDD_HHMMSS.SSZ


for fil=0,na-1 do begin
;for fil=0,1 do begin ; debug

restore,aa(fil) ; read the wire data structure (from program wire_pos.pro)
if fil eq 0 then wire3 = replicate({hjd:0D,mag:0.0,$                  
                  gc:fltarr(2)},n_max)

; t1 = strcompress(string(wire2.time1),/remove_all)
; year = 1900 + float(strmid(t1,0,2))
; month = float(strmid(t1,2,2))
; day = float(strmid(t1,4,2))
 
; t2  = strcompress(string(wire2.time2),/remove_all)
; t2a = strcompress(string(floor(wire2.time2)),/remove_all)

; nt = n_elements(t2) ; number of data points
; hjd = dblarr(nt)
; jd = hjd

; Debugging:
; w = where(strlen(t2a) eq 3)
; w = where(strlen(t2a) eq 6)
; i = w(50)
;  ll = strlen(t2a(i))
; print,t2a(i),' ',t2(i)
; print,hr, min, sec

; Get HJD times!
; for t=0L,nt-1 do begin ; for each data point

;  ll = strlen(t2a(t)) ; length of string ... 
;  if ll eq 1 or ll eq 2 then begin
;   sec = wire2(t).time2
;   min = 0.
;   hr  = 0.
;  endif

;  if ll eq 3 or ll eq 4 then begin
;   sec = float( strmid(t2a(t),1+ll-3,2) ) + ( wire2(t).time2-floor(wire2(t).time2) )
;   min = float( strmid(t2a(t),0,1+ll-3) )
;   hr  = 0.
;  endif

;  if ll eq 5 or ll eq 6 then begin
;   sec = float( strmid(t2a(t),3+ll-5,2) ) + ( wire2(t).time2-floor(wire2(t).time2) )
;   min = float( strmid(t2a(t),1+ll-5,2) )
;   hr  = float( strmid(t2a(t),0,ll-5+1) )
;  endif

;  if ll ge 7 or ll eq 0 then stop

;   min_x = min + sec / 60. ; Time is UT, right?
;   juldate, [year(t), month(t), day(t), hr, min_x], jd_temp ; Julian Date
;   hjd_temp = helio_jd( jd_temp, ra_deg, dec_deg) ; Heliocentric Julian Date
;;   ; print, (hjd_temp-jd_temp)*86400. ; difference in seconds ...
;   jd(t) = jd_temp
;   hjd(t) = hjd_temp

; endfor ; next time entry

   dat = wire2.p(star,ap_number) 

   w = where(dat gt 10.,c)
   times = wire2(w).hjd(star) ; times(w)
   dat = dat(w)
   gc = wire2(w).gc(*,star) ; gaussian (x,y)-center position

   dat = -2.5 * alog10(dat) + 25.0
   dat = dat - median(dat)

;   resistant_mean,dat,3,me,sd_of_mean,nr   
   ptp_robust_fin,dat,noise,1
   fivesigma = 5. * noise ; 5. * robust_sigma(dat)

   astet = 0.7 & bstet = 6.0 ; Stetson outlier weights !
;   fudge_weight = (1. + (abs(dat-me)/(astet*fivesigma))^bstet)^(-1.)
   fudge_weight = (1. + (abs(dat-0.0)/(astet*fivesigma))^bstet)^(-1.)
   fudge_weight = fudge_weight / total(fudge_weight)
   weight = fudge_weight
 
   plot,dat,weight,psym=1,symsi=.2,xr=[-1,1]*fivesigma*2., $
    title='Stetson Weights',xtit='!4D!3mag',ytit='W!IStetson!N'
   plots,fivesigma,!y.crange,line=2
   plots,-fivesigma,!y.crange,line=2
   plots,0.0,!y.crange,line=2
   avv = avg(weight)
   arrow,fivesigma,avv,0.0,avv,/data,thick=2
   arrow,0.0,avv,fivesigma,avv,/data,thick=2
   xyouts,0.4 * ( 0.0 + fivesigma), 1.05 * avv, $
          alignment = 0.5, '5!4r!3!N-limit', charsi=1.3

   resistant_mean,dat,3,me,sd,nr
   w = where(abs(dat-me) lt fivesigma*1.5,c)
   ptp_robust_fin,dat(w),noise,1
   fivesigma = 5. * noise ; 5. * robust_sigma(dat)
   dat = dat(w)
   times = times(w)
   gc = gc(*,w)
   weight = weight(w) / total(weight(w)) ; renormalize!

   wire_merge_dat,times,dat,gc,weight,time3,dat3,gc3,sig3,15.

   plot,times,dat,psym=3,yr=[-1,1]*0.01,ysty=3,xsty=3
   oplot,time3,dat3,psym=3,col=col.red

; Calc. new Stetson weights
   resistant_mean,dat3,3,me,sd_of_mean,nr   
   fivesigma = 5. * robust_sigma(dat3)
   astet = 0.7 & bstet = 6.0 ; Stetson outlier weights !
   fudge_weight = (1. + (abs(dat3-me)/(astet*fivesigma))^bstet)^(-1.)
   fudge_weight = fudge_weight / total(fudge_weight)
   wei3 = fudge_weight

; Merge the data
 wire_merge_ww,time3,dat3,wei3,gc3,cnt3,wire3

 plot,wire3(0:cnt3-1).hjd-51469.,wire3(0:cnt3-1).mag,psym=3,xr=[0,15]

 print,' %%% Merged points so far: ',cnt3
 print,''

endfor ; next restore file is read

wire3 = wire3(0:cnt3-1) ; remove unused data points

g = strsplit(input_dir,'/',/extract) & ng = n_elements(g)
outfile = '/'
for k=0,ng-2 do outfile = outfile + g(k) + '/'
outfile = outfile + g(k-1) + '_merged_slot'+strcompress(string(star),/remove_all)+'.idl'
save, filename = outfile, wire3
print,' %%% Saved "restore" file: '+outfile

; compl with up to 300.000 datapoints
 ampl_spec_calc_wire,time3,dat3,wei3,minfreq,maxfreq,freq,amp,phase

; < 30000 data points?
; ampl_spec_calc2,time3,dat3,wei3,minfreq,maxfreq,freq,amp,phase

;t3 = time3(100:189)
;d3 = dat3(100:189)
;w3 = wei3(100:189) & w3 = w3 / total(w3)
;ampl_spec_calc2,t3,d3,w3,minfreq,maxfreq,freq,amp,phase

 freq = freq * conv ; convert to microHz

 plot,freq,amp,ysty=3,xsty=3,xtit='!3m!4!NHz',ytit='Amplitude'


END
