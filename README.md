---
editor_options: 
  markdown: 
    wrap: 256
---

# Event Vector Space

*event.vector.space* is an R6 class object that represents relations among the duration of events. Moving *from* one event *to* another creates a vector space consisting of relative ordering among *start*, *end*, *overlap*, and *span*:

$$
\vec{e}:=\begin{bmatrix}
\epsilon_{0,s}&\epsilon_{0,e}\\
\epsilon_{1,s}&\epsilon_{1,e}
\end{bmatrix}
$$

|                        |                                                                                             |
|------------------------|---------------------------------------------------------------------------------------------|
| $\Delta S$, $\Delta E$ | Difference of *start* and *end* boundaries: $\epsilon_{1,i}-\epsilon_{0,i}$                 |
| $\Gamma$               | The degree of temporal intersection: $\epsilon_{1,s}-\epsilon_{0,e}$                        |
| $\beta$                | The maximal time spanned: $\epsilon_{1,e}-\epsilon_{0,s}$                                   |
| $||V||$                | Duration of the *from* and *to* events: $\epsilon_{i,e}-\epsilon_{i,s}$                     |
| $\tau$                 | Measure of relative duration across the event vector: $$ ln(\frac{ ||V_1|| }{ ||V_0|| }) $$ |

$\vec\epsilon$ is complex: $$
\vec\epsilon := x + \hat{i}y \Rightarrow 
{\sqrt{(tan^{-1}\frac{\Delta{E}}{\Delta{S}})(tan^{-1}{\frac{\Gamma \beta}{\Gamma}})} + \Gamma^{\frac{\tau}{2}}}
$$ and is interpreted as follows:

|   $x$   | $\hat{i}y$ |               **Definition**               |
|:-------:|:----------:|:------------------------------------------:|
| $\ne 0$ |   $= 0$    |                 *Disjoint*                 |
|  $= 0$  |  $\ne 0$   |               *Concurrency*                |
| $\ne 0$ |  $\ne 0$   |             *Full Concurrency*             |
|  $= 0$  |   $= 0$    |                *Continuity*                |
|         |   $< 1$    |    `to` event shorter than `from` event    |
|         |   $= 1$    | `to` event of equal length to `from` event |
|         |   $> 1$    |    `to` event longer than `from` event     |

The time between these events is the focus of derivation that encodes the relationships between the boundaries of the events, thus allowing one to describe this relationship in a concise manner.
