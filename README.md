---
title: "Atividade 19 - MAT0222"
subtitle: "Álgebra Linear II"
date: "29/06/2020"
output: pdf_document
---
```{r,echo=FALSE}
library(ggplot2)
```

# Exercício 14 - Lista 6
## a)
Quero encontrar uma base ortonormal para o subespaço $V$ gerado pelos polinômios$1$, $x$ e $x^2$.

Seja $\beta=\{w_1,w_2,w_3\}$ a base desejada. Para encontrar essa base utilizaremos o processo de ortogonalização de Gram-Schimdt.
Considere que $v_1=1$, $v_2=x$ e $v_3=x^2$, então temos que


\begin{align*}
u_1&=v_1\\
u_2&=v_2-\frac{<v_2,u_1>u_1}{\|u_1\|^2}\\
u_3&=v_3-\frac{<v_3,u_1>u_1}{\|u_1\|^2}-\frac{<v_3,u_2>u_2}{\|u_2\|^2}\\
\end{align*}

Para $u_2$

\begin{align*}
<v_2,u_1>&=\int_0^1x\ dx=\frac{x^2}{2}\Big|_0^1=\frac{1}{2}\\
\\
\|u_1\|^2&=1\\
u_2&=x-\frac{1}{2}
\end{align*}
Para $u_3$

\begin{align*}
<v_3,u_1>&=\int_0^1x^2dx=\frac{x^3}{3} \Big|_0^1=\frac{1}{3}\\
\\
<v_3,u_2>&=\int_0^1\left(x^3-\frac{x^2}{2}\right)dx=\left(\frac{x^4}{4}-\frac{x^3}{6}\right)\Big|^1_0=\frac{1}{12}\\
\\
\|u_2\|^2&=<u_2,u_2>=\int_0^1\left(x^2-x+\frac{1}{4}\right)dx=\left(\frac{x^3}{3}-\frac{x^2}{2}+\frac{x}{4}\right)\Big|_0^1)=\frac{1}{12}\\
\\
u_3&=x^2-\frac{1}{3}-x+\frac{1}{2}=x^2-x+\frac{1}{6}
\end{align*}

Sabemos então que $\beta_1=\{u_1,u_2,u_3\}$ é uma base ortogonal, para chegarmos em $\beta$, basta dividir cada vetor pela sua norma.
Desta forma, falta apenas calcular a norma de $u_3$, pois as outras ja foram calculadas acima,

\begin{align*}
\|u_3\|^2&=\int_0^1(x^2-x+\frac{1}{6})^2dx\\
&=\int_0^1\left(x^4-2x^3+\frac{4x^2}{3}-\frac{x}{3}+\frac{1}{36}\right)dx\\
&=\left(\frac{x^5}{5}-\frac{x^4}{2}+\frac{4x^3}{9}-\frac{x^2}{6}+\frac{x}{36}\right)\Big|_0^1\\
&=\frac{1}{180}
\end{align*}

Desta forma $\beta=\{w_1,w_2,w_3\}$, onde

\begin{align}
w_1&=\frac{u_1}{\|u_1\|}=1\\
w_2&=\frac{u_2}{\|u_2\|}=x\sqrt{12}-\frac{\sqrt{12}}{2}\\
w_3&=\frac{u_3}{\|u_3\|}=6\sqrt{5} x^2-6\sqrt{5}x+\frac{6\sqrt{5}}{6}
\end{align}

## b)
Para calcular o polinomio de grau 2 que melhor aproxima a função $f(x)=\cos(x)$ no intervalo $[0,1]$, usaremos o seguinte resultado:

*Seja V um e.v. com produto interno,  U um subespço vetorial de dimensão finita e* ${u_1, ...,u_k}$ *base ortonormal de U. Se v* $\in$ *V, então o  vetor em U mais próximo de v é a projeção ortogonal em U do vetor v, isto é*
$$P_U(v)= <v,u_1> u_1 + ...+<v,u_k> u_k$$

\begin{align*}
<cos(x),w_1>&=\int^1_0 cos(x)dx=sen(x)\Big|_0^1=sen(1)\\
\\
<cos(x),w_2>&=\int^1_0 cos(x)\left(x\sqrt{12}-\frac{\sqrt{12}}{2}\right)dx=\sqrt{12}\int_0^1xcos(x)dx-\frac{\sqrt{12}}{2}\int^1_0cos(x)dx\\
&=\sqrt{12}\left(xsen(x)\Big|_0^1-\int^1_0sen(x)dx\right)-\frac{\sqrt{12}}{2}sen(1)=\sqrt{12}\left(sen(1)+cos(1)-1\right)-\frac{\sqrt{12}}{2}sen(1)\\
&=\frac{\sqrt{12}}{2}sen(1)+\sqrt{12}cos(1)-\sqrt{12}=\sqrt{12}\left(\frac{1}{2}sen(1)+cos(1)-1\right)\\
\\
<cos(x),w_3>&=\int^1_0 cos(x)\left(6\sqrt{5} x^2-6\sqrt{5}x+\frac{6\sqrt{5}}{6}\right)dx=6\sqrt{5}\int^1_0 x^2cos(x)dx-6\sqrt{5}\int^1_0xcos(x)dx+\frac{6\sqrt{5}}{6}\int^1_0cos(x)dx\\
&=6\sqrt{5}(2cos(1)-sen(1))-6\sqrt{5}(sen(1)+cos(1)-1)+\frac{6\sqrt{5}}{6}sen(1)=6\sqrt{5}cos(1)-11\sqrt{5}sen(1)+6\sqrt{5}\\
&=\sqrt{5}(6cos(1)-11sen(1)+6)
\end{align*}
```{r,echo=F}
a <- sin(1)
b <- sqrt(12)*(0.5*sin(1)+cos(1)-1)
c <- sqrt(5)*(6*cos(1)-11*sin(1)+6)
cosseno <- function(x){cos(x)}
proj <- function(x){a+sqrt(12)*(x-0.5)*b+sqrt(5)*6*c*(x**2-x+1/6)}
p <- ggplot(data = data.frame(x = c(-0.5,1.5)),aes(x = x))+
  stat_function(fun = cosseno,aes(colour="cos(x)"))+
  stat_function(fun = proj,aes(colour="h(x)"))+
  geom_vline(xintercept =0)+
  geom_vline(xintercept =1)+
labs(title = "Comparação da curva cos(x) com a curva h(x)",colour="Legenda")
```
Desta forma podemos encontrar a função $h(x)\in P_2(\mathbb{R})$, que mais se aproxima da função $cos(x)$ no intervalo $[0,1]$.
$$h(x)=`r a`+`r b`\left(x\sqrt{12}-\frac{\sqrt{12}}{2}\right)+`r c`\left(6\sqrt{5} x^2-6\sqrt{5}x+\frac{6\sqrt{5}}{6}\right)$$
```{r,echo=FALSE}
p
```


