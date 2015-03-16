module Subset where

import List(..)

set = List Int
set = [1..12]

# need to write a more efficient algorithm for computing the set of size-3 subsets of [1..12]
(there are 220 such subsets, and my algorithm currently generates 1728 of them. Computationally
negligible because the scale is so small, but a gross inefficiency nonetheless.)
