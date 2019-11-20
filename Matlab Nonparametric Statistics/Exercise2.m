%Teorema Glivenko Cantelli
clc, clear all, close all;


maxn=500000
landa=1
mu=1/landa
i=1

for n=500:500:maxn
    x=sort(exprnd(mu,1,n))';
    F=cdf('Exponential',x,mu);
    [cdfe,xe]=ecdf(x);
    cdfe=cdfe(2:size(cdfe,1));
    xe=xe(2:size(xe,1));
    Cant(i)=max(abs(cdfe-F));
    i=i+1;
end

n=500:500:maxn;
plot(n,Cant)
figure

plot(x,F)
hold on
plot(xe,cdfe)
legend('Teorica','Empirica')
figure

% ejemplo puntual con n =2000

x=sort(exprnd(mu,1,2000));
F=cdf('Exponential',x,mu);
[cdf,xe]=ecdf(x)
mean(x)
hist(x)
figure

plot(x,F)
hold on
plot(xe,cdf)
legend('Teorica','Empirica')