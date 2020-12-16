> Five international partners together with the journal Systematic and Applied Microbiology (SAM) started “**The All-Species Living Tree" Project (LTP)** to provide a valuable resource particularly for microbial taxonomists. The aim of the project is to reconstruct separate and curated 16S and 23S rRNA datasets and trees spanning **all sequenced type strains** of the hitherto classified species of *Archaea* and *Bacteria*.

> These datasets and trees are **regularly updated** by adding new species with validly published names appearing monthly in the **validation and/or notification lists of the International Journal of Systematic and Evolutionary Microbiology (IJSEM)**


Full Tree is availiable at https://www.arb-silva.de/fileadmin/silva_databases/living_tree/LTP_release_132/LTPs132_SSU_tree.newick. [*newick*, with header]. [full tree local copy](./LTPs132_SSU_tree.newick)

A general overview is available at https://www.arb-silva.de/fileadmin/silva_databases/living_tree/LTP_release_132/LTPs132_tree_overview.pdf. [Overview local copy](./LTPs132_tree_overview.pdf)



## header in newick tree file

```
[All-Species Living Tree. 16S rRNA. June 2018

tree_LTPs132_SSU:
New sequences in this tree were added to the previous
release (LTPs128) using ARB parsimony.

LTP 30% maximum frequency filter was used.

Groups are displayed accordingly to monophyletism and
valid taxonomic affiliations when possible. The
phylogeny of each species is capitulated in the
field "phyl_ltp".

Fields displayed: fullname_ltp, acc, hi_tax_ltp, name
]
```

Note: *the presence of newick header will affect `read.tree()`, therefore, we move the header to this [README.md](./LTP-README.md).

