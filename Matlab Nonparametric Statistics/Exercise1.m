%Evaluación de las temperaturas
clc, clear all, close all;
legendCell = strcat('N=',string(num2cell(1:35)))
legendCell = fliplr(legendCell)
Data=load('temperaturas.txt');
plot(Data)
legend(legendCell)
figure

for i=1:size(Data,2)
    temp=Data(:,i);
    [cdf,x]=ecdf(temp);
    plot(x,cdf)
    hold on
end
legend(legendCell)
figure

[cdf1,x1]=ecdf(Data(:,35));
[cdf2,x2]=ecdf(Data(:,1));
plot(x1,cdf1)
hold on
plot(x2,cdf2)
legend('n=first year','n=last year')
figure

%Construcción de las bandas de confianza.
n=365
alpha=0.05
e=sqrt((1/(2*n))*log(2/alpha))
L1=max(cdf1-e,0)
U1=min(cdf1+e,1)
L2=max(cdf2-e,0)
U2=min(cdf2+e,1)
plot(x1,cdf1,x1,L1,'--.r',x1,U1,'--.m')
hold on
plot(x2,cdf2,x2,L2,'--.r',x2,U2,'--.m')
legend('first year','L first year','U first year','last year','L last year','U last year')





