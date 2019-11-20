clc, clear all, close all;

%% temperaturas year 1 y 35
Data=load('temperaturas.txt');
years=Data(:,[1,35]);
plot(years)
legend('last year','first year')
n=length(years);

%% metodo parametrico

covp=cov(years)
[COEFF,latent,explained] = pcacov(covp)

%% metodo robusto cov

yearmedian=years-median(years)

for i=1:size(yearmedian,2)
    for j=1:size(yearmedian,2)
        if i<=j
           covr(i,j)=median(yearmedian(:,i).*yearmedian(:,j))
        else
           covr(i,j)=covr(j,i) 
        end
    end
end    

[COEFF2,latent2,explained2] = pcacov(covr)

%% muestra contaminada

years2=years
years2(5:20,:)=years2(5:20,:)+100
years2(100:115,:)=years2(100:115,:)+100

covp2=cov(years2)
[COEFF3,latent3,explained3] = pcacov(covp2)

yearmedian2=years2-median(years2)

for i=1:size(yearmedian2,2)
    for j=1:size(yearmedian2,2)
        if i<=j
           covr2(i,j)=median(yearmedian2(:,i).*yearmedian2(:,j))
        else
           covr2(i,j)=covr2(j,i) 
        end
    end
end    

[COEFF4,latent4,explained4] = pcacov(covr2)

%% Tablas resumen

T1=table(covp,covr,covp2,covr2,'VariableNames',{'Mat_Cov_Parametrica' 'Mat_Cov_Robusta' 'Mat_Cov_Parametrica_contaminada' 'Mat_Cov_Robusta_contaminada'})

T2=table(COEFF,COEFF2,COEFF3,COEFF4,'VariableNames',{'Vect_propios_Parametrica' 'Vect_propios_Robusta' 'Vect_propios_Para_Contam' 'Vect_propios_Robus_Contam'})

T3=table(latent,latent2,latent3,latent4,'VariableNames',{'Val_propios_Parametrica' 'Val_propios_Robusta' 'Val_propios_Para_Contam' 'Val_propios_Robus_Contam'})


