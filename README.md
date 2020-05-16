# Jerboa/OCaml

This is the OCaml version of the Jerboa geometrical modeller.
- Author: Thomas Bellet
- Context: This is the software artifact for Thomas Bellet's PhD "Transformations de graphes pour la modélisation géométrique à base topologique"
  (Université de Poitiers)
- Ported to `dune` by Ilias Garnier

License: GNU GPL (say, V2)

# Build
1. Go to the root of the project.
2. `dune build`
3. `cp _build/default/rules_mod.cmxs .` (the executable uses a plugin mechanism)

# Execute
`_build/default/main_mod.exe`
