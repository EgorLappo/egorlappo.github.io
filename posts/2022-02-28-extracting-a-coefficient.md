---
title: Extracting coefficients of analytic function
subtitle: Using the Cauchy formula
description: |
    In this post we show a way to extract a coefficient of an analytic function using the Cauchy formula.
---

Sometimes, it is necessary to get an expression for the coefficient in the Taylor expansion of an analytic function $f(z)$, without necessarily knowing the expression for $f$ itself. There is a way to do this using the Cauchy theorem. We will use the Cauchy theorem in the following form.

*Proposition 1.* Let $U \in \mathbb{C}$ be any domain such that $0\in U$. Then

$$
\int_{\partial U} \frac{dz}{z^n} = \begin{cases} 2\pi i, & n=1 \\ 0, & \text{otherwise}. \end{cases}
$$

So, suppose we have an analytic function $f(z)$ with a Taylor expansion 
$$
f(z) = \sum_{k=0}^\infty a_k z^k.
$$

We claim that 
$$
a_k = \frac{1}{2\pi i} \int_{U} \frac{dz}{z^{n+1}} f(z),
$$
where $U$ is a domain containing $0$.

To show this, we substitute the Taylor expansion for $f$ and work case-by-case.
$$
    \frac{1}{2\pi i} \int_{U} \frac{dz}{z^{n+1}} f(z) =  \sum_{k=0}^\infty \frac{1}{2\pi i} \int_{U} \frac{a_k dz}{z^{n-k+1}}.
$$

When $n = k$, the summand becomes 
$$
\frac{1}{2\pi i} \int_{U} \frac{a_n dz}{z} = a_n,
$$
due to the first case in Proposition 1 above.
When $n \neq k$, the summand is zero, due to the second case in Proposition 1. So, we have shown that 
$$
a_k = \frac{1}{2\pi i} \int_{U} \frac{dz}{z^{n+1}} f(z).
$$