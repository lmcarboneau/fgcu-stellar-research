pro hej
;set_plot,'ps'
;device,filename='bcsum.1000.Z2.17.s.ps',yoffset=8,bits_per_pixel=24,/color

close,1
m=0.
read,'100 x massen: ',m
tung=strcompress(fix(m),/remove)
openr,1,'bcsum.0'+tung+'.Z2.17.s'
skpcom,1
a=fltarr(8,1300)
n=rdarr2(1,a,8)
a=a(*,0:n)

;plot,a(4,*),a(5,*);,xr=[7000,4000],yr=[0,300],title='Udviklingsspor
;for M = 1.00 M!D!9'+string("156B)+'!3!N',xtitle='T!Deff!N',ytitle='L
;/ L!D!9'+string("156B)+'!3!N'

age=a[2,*]
radius=a[3,*]
teff=a[4,*]
lum=a[5,*]
xc=a[6,*]
qc=a[7,*]
mmsun=a[1,1]
z = 2
feh = alog10((1D*z/100.)/0.02) ;metallicity - antaget Z=0.02 for Solen
alpha=1.8
x=0.70

save,filename='bcsum0'+tung+'Z217s.sav',age,radius,teff,lum,xc,qc,mmsun,z,x,alpha,feh
close,1

write_evol,'bcsum0'+tung+'Z217s.sav', 'mass0'+tung+'_metal2.txt'

;device,/close
;set_plot,'x'
end

pro plotte
close,1

openr,1,'bcsum.0100.Z06.17.s'
skpcom,1
a=fltarr(8,1300)
n=rdarr2(1,a,8)
a=a(*,0:n)
lsun=3.846d33
plot,a(4,*),a(5,*),xr=[40000,0],yr=[4500,100000],xtitle='T!Deff!N',ytitle='L / L!D!9'+string("156B)+'!3!N'

;age=a[2,*]
;radius=a[3,*]
;teff=a[4,*]
;lum=a[5,*]
;xc=a[6,*]
;qc=a[7,*]
;mmsun=a[1,1]
;z = 2
;feh = alog10((1D*z/100.)/0.02) ;metallicity - antaget Z=0.02 for Solen
;alpha=1.8
;x=0.70

;save,filename='bcsum0100Z0207s.sav',age,radius,teff,lum,xc,qc,mmsun,z,x,alpha,feh
close,1
 
no_track=21
models=['bcsum.0150.Z06.17.s',$
        'bcsum.0200.Z06.17.s',$
        'bcsum.0250.Z06.17.s',$
        'bcsum.0300.Z06.17.s',$
        'bcsum.0350.Z06.17.s',$
            $; 'bcsum.0400.Z06.17.s',$
      $;       'bcsum.0450.Z06.17.s',$
     $;        'bcsum.0500.Z06.17.s',$
           $;  'bcsum.0550.Z06.17.s',$
     $;        'bcsum.0600.Z06.17.s',$
     $;        'bcsum.0650.Z06.17.s',$
        'bcsum.0700.Z06.17.s',$
        'bcsum.0750.Z06.17.s',$
        'bcsum.0800.Z06.17.s',$
        'bcsum.0850.Z06.17.s',$
        'bcsum.0900.Z06.17.s',$
        'bcsum.0950.Z06.17.s',$
           $;  'bcsum.1000.Z06.17.s',$
        'bcsum.1100.Z06.17.s',$
        'bcsum.1200.Z06.17.s',$
        'bcsum.1300.Z06.17.s',$
        'bcsum.1400.Z06.17.s',$
        'bcsum.1500.Z06.17.s',$
        'bcsum.1600.Z06.17.s',$
        'bcsum.1700.Z06.17.s',$
        'bcsum.1800.Z06.17.s',$
        'bcsum.1900.Z06.17.s',$
        'bcsum.2000.Z06.17.s']

for x = 0, (no_track-1) do begin
  openr, 1, models(x)
  skpcom,1
  a=fltarr(8,1501)
  n=rdarr2(1,a,8)
  a=a(*,0:n)
  oplot,a(4,*),a(5,*)
  close,1

endfor

end

pro farver
device,true_color=24
device,decompose=0
loadct,39
tvlct, [255,0,0], [0,255,0], [0,0,255], 1 ;nu kan der plottes i farver
end


pro udregnmasser
;spawn,'run-evol.mass 0320 0300 06 17 300'
;spawn,'run-evol.mass 0350 0320 06 17 300'
;spawn,'run-evol.mass 0370 0350 06 17 300'
;spawn,'run-evol.mass 0400 0370 06 17 300'
;spawn,'run-evol.mass 0420 0400 06 17 300'
;spawn,'run-evol.mass 0450 0420 06 17 300'
;spawn,'run-evol.mass 0470 0450 06 17 300'
;spawn,'run-evol.mass 0500 0470 06 17 300'
;spawn,'run-evol.mass 0520 0500 06 17 300'
;spawn,'run-evol.mass 0550 0520 06 17 300'
;spawn,'run-evol.mass 0570 0550 06 17 300'
;spawn,'run-evol.mass 0600 0570 06 17 300'
;spawn,'run-evol.mass 0620 0600 06 17 300'
;spawn,'run-evol.mass 0650 0620 06 17 300'
;spawn,'run-evol.mass 0670 0650 06 17 300'
;spawn,'run-evol.mass 0700 0670 06 17 300'
;spawn,'run-evol.mass 0720 0700 06 17 300'
;spawn,'run-evol.mass 0750 0720 06 17 300'
;spawn,'run-evol.mass 0770 0750 06 17 300'
;spawn,'run-evol.mass 0800 0770 06 17 300'
;spawn,'run-evol.mass 0820 0800 06 17 300'
;spawn,'run-evol.mass 0850 0820 06 17 300'
;spawn,'run-evol.mass 0870 0850 06 17 300'
;spawn,'run-evol.mass 0900 0870 06 17 300'
;spawn,'run-evol.mass 0920 0900 06 17 300'
;spawn,'run-evol.mass 0950 0920 06 17 300'
;spawn,'run-evol.mass 0970 0950 06 17 300'
;spawn,'run-evol.mass 0000 0970 06 17 600' ; Er udregnet ;-)
;spawn,'run-evol.mass 0020 1000 06 17 600'
;spawn,'run-evol.mass 0050 1020 06 17 600'
;spawn,'run-evol.mass 0070 1050 06 17 600'
;spawn,'run-evol.mass 0100 1070 06 17 600'
;spawn,'run-evol.mass 0120 1100 06 17 600'
;spawn,'run-evol.mass 0150 1120 06 17 600'
;spawn,'run-evol.mass 0170 1150 06 17 600'
;spawn,'run-evol.mass 0200 1170 06 17 600'
;spawn,'run-evol.mass 0220 1200 06 17 600'
;spawn,'run-evol.mass 0250 1220 06 17 600'
;spawn,'run-evol.mass 0270 1250 06 17 600'
;spawn,'run-evol.mass 0300 1270 06 17 600'
;spawn,'run-evol.mass 0320 1300 06 17 600'
;spawn,'run-evol.mass 0350 1320 06 17 600'
;spawn,'run-evol.mass 0370 1350 06 17 600'
;spawn,'run-evol.mass 0400 1370 06 17 600'
;spawn,'run-evol.mass 0420 1400 06 17 600'
;spawn,'run-evol.mass 0450 1420 06 17 600'
;spawn,'run-evol.mass 0470 1450 06 17 600'
;spawn,'run-evol.mass 0500 1470 06 17 600'
;spawn,'run-evol.mass 0520 1500 06 17 600'
;spawn,'run-evol.mass 0550 1520 06 17 600'
;spawn,'run-evol.mass 0570 1550 06 17 600'
;spawn,'run-evol.mass 0600 1570 06 17 600'
;spawn,'run-evol.mass 0620 1600 06 17 600'
;spawn,'run-evol.mass 0650 1620 06 17 600'
;spawn,'run-evol.mass 0670 1650 06 17 600'
;spawn,'run-evol.mass 0700 1670 06 17 600'
;spawn,'run-evol.mass 0720 1700 06 17 600'
;spawn,'run-evol.mass 0750 1720 06 17 600'
;spawn,'run-evol.mass 0770 1750 06 17 600'
;spawn,'run-evol.mass 0800 1770 06 17 600'
;spawn,'run-evol.mass 0820 1800 06 17 600'
;spawn,'run-evol.mass 0850 1820 06 17 600'
;spawn,'run-evol.mass 0870 1850 06 17 600'
;spawn,'run-evol.mass 0900 1870 06 17 600'
;spawn,'run-evol.mass 0920 1900 06 17 600'
;spawn,'run-evol.mass 0950 1920 06 17 600'
;spawn,'run-evol.mass 0970 1950 06 17 600'
;spawn,'run-evol.mass 2000 1970 06 17 600'
;spawn,'run-evol.mass 0920 0900 06 17 600'

;spawn,'run-evol.mass 0970 1000 2 17 600'
;spawn,'run-evol.mass 0950 0970 2 17 600'
;spawn,'run-evol.mass 0920 0950 2 17 600'
;spawn,'run-evol.mass 0900 0920 2 17 600'
;spawn,'run-evol.mass 0870 0900 2 17 600'
;spawn,'run-evol.mass 0850 0870 2 17 600'
;spawn,'run-evol.mass 0820 0850 2 17 600'
;spawn,'run-evol.mass 0800 0820 2 17 600'
;spawn,'run-evol.mass 0770 0800 2 17 600'
;spawn,'run-evol.mass 0750 0770 2 17 600'
;spawn,'run-evol.mass 0720 0750 2 17 600'
;spawn,'run-evol.mass 0700 0720 2 17 600'
spawn,'run-evol.mass 0670 0700 2 17 600'
spawn,'run-evol.mass 0650 0670 2 17 600'
spawn,'run-evol.mass 0620 0650 2 17 600'
spawn,'run-evol.mass 0600 0620 2 17 600'
spawn,'run-evol.mass 0570 0600 2 17 600'
spawn,'run-evol.mass 0550 0570 2 17 600'
spawn,'run-evol.mass 0520 0550 2 17 600'
spawn,'run-evol.mass 0500 0520 2 17 600'
spawn,'run-evol.mass 0470 0500 2 17 600'
spawn,'run-evol.mass 0450 0470 2 17 600'
spawn,'run-evol.mass 0420 0450 2 17 600'
spawn,'run-evol.mass 0400 0420 2 17 600'
spawn,'run-evol.mass 0370 0400 2 17 600'
spawn,'run-evol.mass 0350 0370 2 17 600'
spawn,'run-evol.mass 0320 0350 2 17 600'
spawn,'run-evol.mass 0300 0320 2 17 600'
spawn,'run-evol.mass 0270 0300 2 17 600'
spawn,'run-evol.mass 0250 0270 2 17 600'
spawn,'run-evol.mass 0220 0250 2 17 600'
spawn,'run-evol.mass 0200 0220 2 17 600'
spawn,'run-evol.mass 0170 0200 2 17 600'
spawn,'run-evol.mass 0150 0170 2 17 600'
spawn,'run-evol.mass 0120 0150 2 17 600'
spawn,'run-evol.mass 0100 0120 2 17 600'
end
