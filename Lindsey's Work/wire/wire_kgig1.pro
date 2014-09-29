; Compare results for HD 101666 analysed by Bruntt / Preston

; ==============================================================
dops = 1
colx = 150
; ==============================================================

; ==============================================================
mode = 101666
mode =  28749
; ==============================================================

; ==============================================================
if mode eq 28749 then begin
 readcol,'/home/bruntt/wire/wire_periods/NuEri_Feb2004_s3_HD28749_K3Iab.per',a,f1,a1,p1,format='a,f,f,f'
 readcol,'/home/bruntt/wire/giants/hd028749.dat',a,hf1,ha1,hs1,format='a,f,f,i'

c1 = n_elements(f1)
hc1 = n_elements(hf1)

easyps, keywords, kk, dops=dops, /prepare, dim = [15,8,-1,1], $
 fil = 'HD028749a.ps', dir = '/home/bruntt/wire/giants/'

plotsym,0,/fill

plot,[0,1],/nodata,xr=[0,1],yr=[-2,2],$
 xtit='Frequency [c/day]' ,ytit='Amplitude [mmag]',$
 tit='HD 28749 (K3 I)'

for i=0,c1-1 do oplot,[f1(i),f1(i)],[0,a1(i)]*1e3
for i=0,hc1-1 do oplot,[hf1(i),hf1(i)],[0,-ha1(i)]*1e3

plots,!x.crange,0.

xyouts,.4,-1.5,'HLP'
xyouts,.4,1.5,'HB'

easyps, keywords, kk, dops=dops, /close


endif
; ==============================================================

; ==============================================================
if mode eq 101666 then begin

readcol,'/home/bruntt/wire/wire_periods/KsiHya_Dec2003_s2_HD101666_K5III.per',a,f1,a1,p1,format='a,f,f,f'
readcol,'/home/bruntt/wire/wire_periods/KsiHya_May2004_s3_HD101666_K5III.per',a,f2,a2,p2,format='a,f,f,f'

w = where(f1 lt 14 and f1 gt .05,c1) & f1 = f1(w) & a1 = a1(w) & p1 = p1(w)
w = where(f2 lt 14 and f2 gt .05,c2) & f2 = f2(w) & a2 = a2(w) & p2 = p2(w)

readcol,'/home/bruntt/wire/giants/hd101666_heather1.dat',a,hf1,ha1,hs1,format='a,f,f,i'
readcol,'/home/bruntt/wire/giants/hd101666_heather2.dat',a,hf2,ha2,hs2,format='a,f,f,i'

hc1 = n_elements(hf1)
hc2 = n_elements(hf2)

df = 0.00

easyps, keywords, kk, dops=dops, /prepare, dim = [15,8,-1,1], $
 fil = 'HD101666a.ps', dir = '/home/bruntt/wire/giants/'

plotsym,0,/fill

plot,[0,1],/nodata,xr=[0,1],yr=[-5,5]* 2,$
 xtit='Frequency [c/day]' ,ytit='Amplitude [mmag]',$
 tit='HD 101666 (K5 III)'

for i=0,c1-1 do oplot,[f1(i),f1(i)],[0,a1(i)]*1e3
for i=0,c2-1 do oplot,[f2(i),f2(i)]+df,[0,a2(i)]*1e3,col=colx,line=2
for i=0,c2-1 do plots,[f2(i)]+df,[a2(i)]*1e3,col=colx,psym=8,symsi=.3

for i=0,hc1-1 do oplot,[hf1(i),hf1(i)],[0,-ha1(i)]*1e3
for i=0,hc2-1 do oplot,[hf2(i),hf2(i)]+df,[0,-ha2(i)]*1e3,col=colx,line=2
for i=0,hc2-1 do plots,[hf2(i)]+df,[-ha2(i)]*1e3,col=colx,psym=8,symsi=.3

plots,!x.crange,0.

xyouts,.4,-8,'HLP'
xyouts,.4,8,'HB'

easyps, keywords, kk, dops=dops, /close

endif
; ==============================================================

END



