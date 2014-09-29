PRO wire_smooth_phase, t,d, ran1, ran2, phase, data, error, $
 per=per,binsize=binsize, debug=debug, slack=slack, $
 norm=norm, outfile=outfile, $ 
 noerrb=noerrb, sm2=sm2

; Example:
; wire_smooth_phase, t, d, [-4.,6], [100.,202], phase, data, error

default9, debug, 0B
default9, noerrb, 0B ; plot error bars?
default9, sm2, 0B

t2 = t
d2 = d

w = where( (t2 gt ran1(0) and t2 lt ran1(1) ) or $
           (t2 gt ran2(0) and t2 lt ran2(1) ), c)

t2 = t2(w)
d2 = d2(w)

help,t,t2

; Sort times:
so = sort(t2)
t2 = t2(so) & d2 = d2(so)

default9, per, 5.95175D
t0ph = min(t) 
print,' %%% Zero point in time: ', t0ph 

phase2 = ( (t2-t0ph) mod per)/per

n = n_elements(t2)
dt = t2(1:n-1) - t2(0:n-1)
wt = where(dt gt 50,c)

if debug then begin
 !P.multi=[0,1,2]
 plot,t2(0:wt-1),d2(0:wt-1),psym=1,symsi=.3,yr=[0.05,-0.01],xr=[-14,14]
 plot,t2(wt+1:*),d2(wt+1:*),psym=1,symsi=.3,yr=[0.05,-0.01],xr=198. + [-14,14]
 !P.multi=0

 hitme,s1 & if s1 eq 'x' then stop
endif

s = sort(phase2)
phase3 = phase2(s)
dat3 = d2(s)
tim3 = t2(s)

w1 = where(tim3 lt 100,c1)
w2 = where(tim3 gt 100,c2)

col = getcolor(/load)

plot,phase3, dat3,psym=1,symsi=.3,yr=[0.05,-0.01],/nodata
if c1 ge 2 then oplot,phase3(w1),dat3(w1),col=col.red,psym=1,symsi=.3
if c2 ge 2 then oplot,phase3(w2),dat3(w2),col=col.sky,psym=1,symsi=.3

default9, binsize, 0.005
default9, slack, 0.5 ; overlap btw bins (in percent of the binsize)

bin = 0D - binsize * 0.5

np = 10000.
phase = dblarr(np)
data  = phase
error = phase
cnt = 0L

; Calculate typical noise level
  ptp_robust_fin, dat3, noise_typ, 1


while bin le 1.0 do begin
 w = where( phase3 ge (bin-slack*binsize) and phase3 lt (bin+binsize+slack*binsize),cw)

 if cw ge 3 then begin
  resistant_mean, dat3(w), 3, me, sd, nr
  phase(cnt) = bin + binsize * 0.5
  data(cnt) = me
  ptp_robust_fin, dat3(w), noise, 1
  error(cnt) = sqrt(noise^2. + (noise_typ*0.5)^2.) ; robust_sigma(dat3(w))
  cnt = cnt + 1
 endif
 bin = bin + binsize
endwhile

phase = phase(0:cnt-1)
data  = data(0:cnt-1)
error = error(0:cnt-1)

error = smooth(error, 5) ; smooth the errors!

; Normalize spectrum ?
if n_elements(norm) eq 2 then begin
 w = where(phase ge norm(0) and phase le norm(1),c)
 off = median(data(w))
 print,' %%% Offset data based on phase range: ',norm
 data = data - off
endif

plotsym,0,/fill

; Additional smoothing?
if sm2 then data = smooth(data,5, /edge)

for j=0,cnt-1 do begin
 plots,phase(j),data(j),psym=8,col=col.yellow,symsi=.5
 if noerrb eq 0 then $
  oplot,phase(j)*[1.,1.],data(j)+[-1.,1]*error(j),thick=1,col=col.yellow
endfor

if n_elements(outfile) eq 1 then begin
 openw,1,outfile
 n = n_elements(error)
 for i=0L,n-1 do $
  printf,1,phase(i), data(i), error(i), $
   format='(D15.7, D15.7, D15.5)'
 close,1
 print,' %%% Wrote phased LC as file: ' + outfile
endif


END
