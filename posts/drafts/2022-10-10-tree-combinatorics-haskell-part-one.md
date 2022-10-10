---
title: Phylogenetic tree combinatorics in Haskell, Part I
subtitle: Coalescent histories
description: |
    In this post I implement several simple results in phylogenetic combinatorics using Haskell
---

In this series of posts, I want to put some of [my](https://egorlappo.github.io) or [my collaborators'](https://rosenberglab.stanford.io) results in combinatorics of trees in a concrete form. I will explain some mathematical objects that are used in phylogenetic algorithms and show how theorems about them translate to Haskell code. In the first post, we will discuss the basic setup and then count _coalescent histories_ for a pair of trees.

I will be assuming elementary knowledge of Haskell syntax, but no advanced features or external packages will be used.

## Introduction

The first order of business is to settle on terms and definitions. We will be talking about biological _species_ and evolutionary relationships between them. In fact, whenever I say "species", you can just think about a population of kittens/puppies/bunnies/... that is in reproductive and geographical isolation from other similar populations. New species are formed through _speciation_, when one existing species splits in two, because of a new separating geographical barrier, ecological specialization, or some other reason. We will always assume that all of our species originate from a single ancestral species through a sequence of speciations.

The history of such speciation events is encoded by a tree, which we will call a _species tree_ and denote by a letter $S$. A species tree has _nodes_ and _edges_. Each edge corresponds to a species that existed at some point in time. There are two types of nodes in $S$