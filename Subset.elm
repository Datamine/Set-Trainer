module Subset where

import List(..)

set = List Int
set = [1..12]

# need to write a more efficient algorithm for computing the set of size-3 subsets of [1..12]
(there are 220 such subsets, and my algorithm currently generates 1728 of them. Computationally
negligible because the scale is so small, but a gross inefficiency nonetheless.)

general idea for algorithm (dynamic programming):

suppose we're picking elements from set {0..n-1}
then compute the 3,2, and 1 element subsets of {n-4,n-1}
e.g.:
    3: [{n-3, n-2, n-1}]
    2: [{n-3, n-2}, {n-2, n-1}, {n-3, n-1}]
    1: [{n-1}, {n-2}, {n-3}]
then work backwards to {n-i,n-1}, incrementing i
then to compute all size==3 subsets of {n-i..n-1}, we take the union of all size j
subsets of {n-(i-1)..n-1} and the set of all size (j-1) subsets of {n-(i-1)..n-1} after
appending the element (n-i) to all of them

off-by-one error somewhere in that
