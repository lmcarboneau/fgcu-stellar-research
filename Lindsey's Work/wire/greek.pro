; Print the greek alpha bet

dops = 1
easyps, keywords, kk, dops=dops, /prepare, dim = [26,15,2.5,28], $
 fil = 'greek.ps', dir = '~/temp/',/landscape


x = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
nx = n_elements(x)

nx2 = ceil(nx/2)

!P.charthick=1
!P.charsize=2.5

plot,[0,1],/nodata,charsi=.1,xsty=6,ysty=6,xmargin=0,ymargin=0
for i=0,nx2-1 do xyouts,.05+0.07*i,.9,'!4'+x(i)+'!3'
for i=0,nx2-1 do xyouts,.05+0.07*i,.7,'!3'+x(i)+'!3'

for i=nx2,nx-1 do xyouts,.05+0.07*(i-nx2),.4,'!4'+x(i)+'!3'
for i=nx2,nx-1 do xyouts,.05+0.07*(i-nx2),.2,'!3'+x(i)+'!3'

easyps, keywords, kk, dops=dops, /close

end
