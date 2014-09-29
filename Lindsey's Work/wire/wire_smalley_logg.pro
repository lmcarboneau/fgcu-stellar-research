PRO wire_smalley_logg, teff, hbeta, logg, debug=debug

default9, debug, 0B

grid = fltarr(26+2, 11) ; teff, logg

gteff = [5000, $
         5500.,6000,6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000.,$
         11000,12000,13000,14000,15000,16000,17000,18000, 20000,$
         22500, 25000, 30000, 35000, 40000, 45000, 50000, $
         55000]

if teff lt 6000 or teff gt 45000 then $
 print,' *** Warning: logg based on smalley grid interpolated'

if teff lt 5200 or teff gt 52000 then begin
 print,' %%% Teff outside acceptable range in wire_smalley_logg: ',teff
 logg = -0.5
 goto,aborted
endif

; Points at 5000 and 55000 artificially inserted

glogg = [0.0, 0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]

grid( 0,*) = -1.
grid( 1,*) = [605, 603,  600,596,590,584,580,575,570,566,-1.]
grid( 2,*) = [-1., 640,  637,633,262,620,614,607,601,596,-1.]
grid( 3,*) = [-1.,  -1,  680,679,678,672,665,656,648,640,-1.]
grid( 4,*) = [-1.,  -1,  724,727,730,729,725,718,708,699,-1.]
grid( 5,*) = [-1.,  -1,  729,767,785,785,785,782,775,755,-1.]
grid( 6,*) = [-1.,  -1,  673,746,796,833,841,841,839,832,-1.]
grid( 7,*) = [-1.,  -1,  602,690,760,818,860,887,900,901,-1.]
grid( 8,*) = [-1.,  -1,   -1,641,715,782,839,883,911,926,-1.]
grid( 9,*) = [-1.,  -1,   -1,611,676,743,807,864,906,932,-1.]
grid(10,*) = [-1.,  -1,   -1,591,651,711,773,835,889,926,-1.]
grid(11,*) = [-1.,  -1,   -1, -1,618,669,722,778,834,886,-1.]
grid(12,*) = [-1.,  -1,   -1, -1,596,642,689,738,788,838,-1.]
grid(13,*) = [-1.,  -1,   -1, -1,580,623,666,710,754,800,-1.]
grid(14,*) = [-1.,  -1,   -1, -1,566,608,648,689,730,771,-1.]
grid(15,*) = [-1.,  -1,   -1, -1,554,596,634,672,711,750,-1.]
grid(16,*) = [-1.,  -1,   -1, -1,542,585,622,659,696,732,-1.]
grid(17,*) = [-1.,  -1,   -1, -1,530,575,611,647,682,716,-1.]
grid(18,*) = [-1.,  -1,   -1, -1, -1,565,602,636,670,703,-1.]
grid(19,*) = [-1.,  -1,   -1, -1, -1,545,584,618,650,681,-1.]
grid(20,*) = [-1.,  -1,   -1, -1, -1, -1,563,597,628,658,-1.]
grid(21,*) = [-1.,  -1,   -1, -1, -1, -1,545,578,608,638,668]
grid(22,*) = [-1.,  -1,   -1, -1, -1, -1, -1,549,579,605,631]
grid(23,*) = [-1.,  -1,   -1, -1, -1, -1, -1,523,548,575,599]
grid(24,*) = [-1.,  -1,   -1, -1, -1, -1, -1, -1,532,548,565]
grid(25,*) = [-1.,  -1,   -1, -1, -1, -1, -1, -1, -1,539,552]
grid(26,*) = [-1.,  -1,   -1, -1, -1, -1, -1, -1, -1,533,545]
grid(27,*) = [-1.,  -1,   -1, -1, -1, -1, -1, -1, -1, -1, -1] ; INTERPOLATED

; The end points were interpolated!
; print,interpol(grid(*,9),gteff,55000)



; Interpolation fails for the single point:
grid(0,0) = grid(1,0) - 37.

; Interpolate points for slightly lower / higher temp
ng = n_elements(glogg)
for i=1,ng-1 do $
 grid(0,i) = interpol(grid(1:*,i), gteff(1:*), 5000.)

for i=0,ng-1 do begin
 wu = where(grid(*,i) lt 0.,c)
 if i eq 10 then wu = max(wu) else wu = wu(0); select the first one with negative value
 grid(wu,i) = interpol(grid(0:wu-1,i), gteff(0:wu-1), gteff(wu))

 if i eq 10 then begin
  wu2 = where(grid(*,i) lt 0.,c)
  wu2 = max(wu2)
  grid(wu2,i) = interpol(grid(wu2+1:*,i), gteff(wu2+1:*), gteff(wu2))
 endif
endfor


grid = grid / 1000.
grid = grid + 2.0
w = where(grid le 2.1,c) & grid(w) = 0.

if debug then $
 for j=0,27 do print,grid(j,*),format='(11F6.3)'


difteff = abs(gteff - teff)
w = where(difteff eq min(difteff),c)
w=w(0)



if debug then begin
 contour,grid,gteff,glogg,levels=[2.3,2.4,2.5,2.6,2.7,2.8,2.9],$
 c_label=[1,1,1,1,1,1,1,1,1,1,1],charsi=2.0,c_charsi=2.0

 plots, teff, !y.crange, line=2,color=200, thick=2
 xyouts, teff, 1.0, strcompress(string(hbeta,format='(F9.2)'),/remove_all),charsi=2.0,col=200

 plot,glogg,grid(w,*),psym=-6,yr=[2.5,3.0]
 oplot,glogg,grid(w-1,*),psym=-6,line=2
 oplot,glogg,grid(w+1,*),psym=-6,line=2
 plots, !x.crange, hbeta, line=2,color=200, thick=2
endif

 g1 = interpol(glogg,grid(w-1,*),hbeta)
 g2 = interpol(glogg,grid(w,*),hbeta)
 g3 = interpol(glogg,grid(w+1,*),hbeta)

 tt = [gteff(w-1),gteff(w),gteff(w+1)]
 gg = [g1,g2,g3]

if debug then begin
 plot,tt,gg,psym=-6,yr=g2 + [-1,1]*1.,xr=teff + [-1,1]*2000.,xtit='Teff',ytit='log g'
 plots,teff,!y.crange,line=2,col=200,thick=2
 hitme,uuu
 if uuu eq 'x' then stop
endif

 logg = interpol(gg,tt,teff)

if debug then begin
 xyouts,teff,logg,$
  'log g = ' + strcompress(string(logg,format='(F9.2)'),/remove_all),charsi=2.0
endif

aborted: 

if logg lt 0 or logg gt 5. then begin
 print, ' *** smalley_logg: logg outside grid range: ',logg
 logg = -1
endif





END
