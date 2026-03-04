; In the procedures given in SICP, each call to `encode-symbol` implies a call to `element-of-set?`,
; entailing a walk through the entire tree to the leaves. This is an `O(n)` time complexity
; operation. In the case of the most frequently appearing letter, we only encode 1 bit which means
; walking to this leaf will have a time complexity of `O(1)`. In constrast: the least frequent
; letter will be at the very end of the degenerate tree, implying a time complexity of `O(n)`. If
; we multiply these by the big O of the initial `element-of-set?` call we get a total time
; complexity of `O(n*1) = O(n)` in the best case and `O(n*n) = O(n^2)` in the worst case.
;
; To generalise this further: if we let n be the total number of characters in our degenerate
; Huffman tree and let i represent the 1-based index of our characters, ordered from most frequent
; (i=1) to least frequent (i=n).
;
; - Probability (P): The frequencies of the characters are powers of 2.
; To get the probability of hitting a specific character, we divide its frequency by the total
; sum of all frequencies. For this degenerate tree, the probability approximately halves at each
; step down. `P(i) ≈ 1 / (2^i)`
;
; - Cost (C): As we traverse down the tree, each element-of-set? call searches a decreasing number
; of elements: n at the root, n-1 at the next level, down to n-i+1. The total cost to reach depth i
; is an arithmetic progression: `C(i) = \sum_{k=1}^{i} (n - k + 1) = i * n - (i^2 - i)/2`
;
; To find the average time complexity, we calculate the expected value E, which is the sum of the
; cost of each character multiplied by its probability of appearing. Adding this to our big O and
; extrapolating out the overall complexity should be something like:
;
; ```latex
; O\left(\sum_{i=1}^{n} \frac{1}{2^i} \left( i \cdot n - \frac{i^2 - i}{2} \right)\right) \\
; \Rightarrow O\left(\sum_{i=1}^{n} \frac{i \cdot n}{2^i}\right) \\
; \Rightarrow O\left(n \sum_{i=1}^{n} \frac{i}{2^i}\right)
; ```
;
; We can further simplify using the convergent infinite series for arithmetico-geometric sequences
; and by dropping our multiplicative constants as is common in big O analysis.
;
; ```latex
; \begin{align*}
; O\left(n \sum_{i=1}^{n} \frac{i}{2^i}\right)
; &\approx O\left(n \cdot 2\right) \tag{Infinite Series Convergence} \\
; &= O(n) \tag{Drop Multiplicative Constants}
; \end{align*}
; ```
;
; By converting the product to an expected value summation and applying standard series convergence,
; the math collapses elegantly. The heavy computational cost of reaching the deepest leaves (O(n^2))
; is completely neutralized by the exponentially shrinking probability (1 / 2^i) of actually needing
; to traverse down there.
;
; This leaves us with an overall big O of O(n) for the encoding of any character on average.
