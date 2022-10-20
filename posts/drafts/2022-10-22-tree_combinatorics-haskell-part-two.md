---
title: Combinatorics of phylogenetic trees in Haskell, Part II
subtitle: Ancestral configurations
description: |
    In this post I implement more simple results in phylogenetic combinatorics using Haskell
---

Check out **[Part I](/posts/2022-10-21-tree-combinatorics-haskell-part-one.html)** of this series first!

For the necessary background to this post, please read a **[small introduction into phylogenetic trees](/posts/2022-10-20-gene-trees-species-trees.html)**. 
In that post, I have defined species and gene trees as focal objects of modern phylogenetics. The essence of it is that a species tree describes the natural history of speciation events for a group of species, and a gene tree represents the relationship between genetic lineages sampled from these species. So, the two trees are closely related, but at the same time they can have different shapes (topologies).
