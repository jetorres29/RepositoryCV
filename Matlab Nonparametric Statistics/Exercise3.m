clc, clear all, close all;


for i=1:50
    a(i)=exp(-1-i)
    b(i)=(1/i^2)
end

n=1:50
plot(n,a)
hold on
plot(n,b)
legend('exp(-1-k)','1/k^2')
