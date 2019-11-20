clc, clear all, close all;

%generamos muestra distribucion exponencial

n=10000;
landa=1;
mu=1/landa;

U=sort(exprnd(mu,1,n));
hist(U)
figure
%teorica
F=cdf('Exponential',U,mu);
%Empirica
[cdfe,Ue]=ecdf(U);
cdfe=cdfe(2:size(cdfe,1));
Ue=Ue(2:size(Ue,1));

plot(U,F)
hold on
plot(Ue,cdfe)
legend('Teorica','Empirica')

% bootstrap var y SE

B=2000
minB = bootstrp(B,@min,U);
TmeanB=mean(minB);
vboot=sum((minB-TmeanB).^2)/B;
SEboot=sqrt(vboot);

% Jackknife sesgo

Tmin=min(U);

for i=1:n
    iU=U;
    iU(i)=[];
    T(i)=min(iU);
end

Tmean=mean(T);
bjack=(n-1)*(Tmean-Tmin)
Tjack=Tmin-bjack;

% sesgo teorico

Sesgot=1/(landa*n)

%diferencia entre sesgos

difsesgos=bjack-Sesgot

T1=table([vboot;B;bjack;Sesgot;difsesgos;n],'VariableNames',{'Resultados'},'RowNames',{'Bootstrap_variance' 'B_bootstraping' 'Sesgo_Jackknife' 'Sesgo_Teorico' 'Diferencia_sesgos' 'Muestra_n'})


