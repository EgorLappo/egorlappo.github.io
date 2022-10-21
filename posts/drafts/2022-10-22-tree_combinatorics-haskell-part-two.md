---
title: Combinatorics of phylogenetic trees in Haskell, Part II
subtitle: Ancestral configurations
description: |
    In this post I implement more simple results in phylogenetic combinatorics using Haskell
---

*Check out **[Part I](/posts/2022-10-21-tree-combinatorics-haskell-part-one.html)** of this series first!*

For the necessary background to this post, please read a **[small introduction into phylogenetic trees](/posts/2022-10-20-gene-trees-species-trees.html)**. 
In that post, I have defined species and gene trees as focal objects of modern phylogenetics. The essence of it is that a species tree describes the natural history of speciation events for a group of species, and a gene tree represents the relationship between genetic lineages sampled from these species. So, the two trees are closely related, but at the same time they can have different shapes (topologies).

In this post, I will offer another mathematical perspective on the relationship between gene and species trees: _ancestral configuration_. We will once again consult the papers for definitions and theorems, and then implement this structure with Haskell.

Coalescent histories were essentially maps from nodes of the gene tree into the branches of the species tree that satisfy some natural conditions. Ancestral configurations are not maps but _sets of lineages_, i.e. sets of edges of the gene tree.

To define ancestral configurations, we need to talk about realizations. Given a gene tree $G$ and a species tree $S$, a _realization_ of $G$ in $S$ is essentially a juxtaposition of the "thin" tree $G$ with the "thick" tree $S$, just like in the illustration of the multispecies coalescent from the [introductory blogpost](/posts/2022-10-20-gene-trees-species-trees.html). Here is how it looks: 

![**Figure 1** Two example realizations of $G$ in $S$. The gene tree $G$ is shown in gray, and the species tree $S$ is shown in black.](/images/blog/tree_posts/realizations.png)

In these pictures, 