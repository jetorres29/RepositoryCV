%Evaluación de las temperaturas
clc, clear all, close all;

Data=load('temperaturas.txt');
year1=Data(:,size(Data,2));
n=length(year1);
plot(year1)
legend('year1 temperaturas')
figure
[cdf,x]=ecdf(year1);
plot(x,cdf)
legend('year1 F empirica')
figure
%intervalo de confianza no parametrico

B=1000;

minB = bootstrp(B,@min,year1);
xbins = [-34.8 -34.2 -33.6]
hist(minB,xbins)
legend('Histograma Tn minimo')
IC = prctile(minB,[5 95],'all')

%intervalo de confianza con bootstrap

alpha=0.05
Tmin=min(year1)
TmeanB=mean(minB)
vboot=sum((minB-TmeanB).^2)/B
Li=Tmin+norminv(alpha/2)*sqrt(vboot)
Ls=Tmin+norminv(1 - alpha/2)*sqrt(vboot)

%sesgo por Jackknife

Tmin=min(year1)


for i=1:n
    iyear1=year1;
    iyear1(i)=[];
    T(i)=min(iyear1);
end

Tmean=mean(T)
bjack=(n-1)*(Tmean-Tmin)
Tjack=Tmin-bjack

%varianza

for i=1:n
    iyear1=year1;
    iyear1(i)=[];
    T=min(iyear1);
    Tpv(i)=n*Tmin - (n-1)*T;
end

sTpv=var(Tpv)
vjack=sTpv/n

T1=table([Tmin;vboot;sqrt(vboot)],'VariableNames',{'Bootstrap_variance_estimation'},'RowNames',{'Tn' 'vboot' 'SE^boot'})

T2=table([prctile(minB,5,'all');Li],[prctile(minB,95,'all');Ls],'VariableNames',{'Limite_inferior' 'Limite_superior'},'RowNames',{'Percentiles' 'Bootstraping'})

T3=table([bjack;Tjack;Tmean;Tmin;n],'VariableNames',{'Jackknife_sesgo'},'RowNames',{'bjack' 'Tjack' 'Tn_barra' 'Tn' 'n'})

T4=table([vjack;vboot],'VariableNames',{'Variance_estimation'},'RowNames',{'Jackknife_Method' 'Bootstrap_Method'})

