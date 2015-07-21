(do
 (clojure.core/ns
  conexp.contrib.java.Main
  (:require conexp.main)
  (:gen-class
   :prefix
   conexp-clj-
   :methods
   (^{:static true}
    [get_resource [Object] java.net.URL]
    ^{:static true}
    [hash_combine_hash ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [disjoint_union ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [conexp_version [] Object]
    ^{:static true}
    [gcd [Object Object] Object]
    ^{:static true}
    [next_closed_set_in_family [Object Object Object Object] Object]
    ^{:static true}
    [quit [] Object]
    ^{:static true}
    [lectic__lt__i [Object Object Object Object] Object]
    ^{:static true}
    [graph_of_function_p [Object Object Object] Object]
    ^{:static true}
    [minimal_generating_subsets [Object Object] Object]
    ^{:static true}
    [union [] Object]
    ^{:static true}
    [union [Object] Object]
    ^{:static true}
    [union [Object Object] Object]
    ^{:static true}
    [union [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [topological_sort [Object Object] Object]
    ^{:static true}
    [cross_product ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [superset_p [Object Object] java.lang.Boolean]
    ^{:static true}
    [floor [Object] Object]
    ^{:static true}
    [exact_integer_sqrt [Object] Object]
    ^{:static true}
    [subset_p [Object Object] java.lang.Boolean]
    ^{:static true}
    [next_closed_set [Object Object Object] Object]
    ^{:static true}
    [join [Object Object] Object]
    ^{:static true}
    [join [Object Object Object] Object]
    ^{:static true}
    [set_of_range [Object] Object]
    ^{:static true}
    [set_of_range [Object Object] Object]
    ^{:static true}
    [set_of_range [Object Object Object] Object]
    ^{:static true}
    [ask [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [first_non_nil [Object] Object]
    ^{:static true}
    [lectic__lt_ [Object Object Object] Object]
    ^{:static true}
    [select [Object Object] Object]
    ^{:static true}
    [illegal_state ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [ensure_length [Object long] Object]
    ^{:static true}
    [ensure_length [Object long Object] Object]
    ^{:static true}
    [ceil [Object] Object]
    ^{:static true}
    [sort_by_second [Object Object] Object]
    ^{:static true}
    [integer_length [Object] Object]
    ^{:static true}
    [proper_subset_p [Object Object] Object]
    ^{:static true}
    [transitive_closure [Object] Object]
    ^{:static true}
    [singleton_p [Object] Object]
    ^{:static true}
    [intersection [Object] Object]
    ^{:static true}
    [intersection [Object Object] Object]
    ^{:static true}
    [intersection [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [expt [Object Object] Object]
    ^{:static true}
    [distinct_by_key [Object Object] Object]
    ^{:static true}
    [tails [Object] Object]
    ^{:static true}
    [sort_by_first [Object Object] Object]
    ^{:static true}
    [inits [Object] Object]
    ^{:static true}
    [all_closed_sets_in_family [Object Object Object] Object]
    ^{:static true}
    [all_closed_sets_in_family [Object Object Object Object] Object]
    ^{:static true}
    [now [] Object]
    ^{:static true}
    [index [Object Object] Object]
    ^{:static true}
    [clojure_type [Object] Object]
    ^{:static true}
    [sqrt [Object] Object]
    ^{:static true}
    [yes_or_no_p [Object] Object]
    ^{:static true}
    [warn [Object] Object]
    ^{:static true}
    [rename_keys [Object Object] Object]
    ^{:static true}
    [order_by [Object] Object]
    ^{:static true}
    [unsupported_operation ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [minimum_set_covers [Object Object] Object]
    ^{:static true}
    [proper_superset_p [Object Object] Object]
    ^{:static true}
    [has_version_p [Object] Object]
    ^{:static true}
    [reflexive_transitive_closure [Object Object] Object]
    ^{:static true}
    [illegal_argument ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [immigrate ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [map_invert [Object] Object]
    ^{:static true}
    [not_yet_implemented [] Object]
    ^{:static true}
    [ensure_seq [Object] Object]
    ^{:static true}
    [rename [Object Object] Object]
    ^{:static true}
    [minimal_hypergraph_transversals [Object Object] Object]
    ^{:static true}
    [partial_min [Object Object] Object]
    ^{:static true}
    [_lt__eq__gt_ [Object Object] Object]
    ^{:static true}
    [to_set [Object] Object]
    ^{:static true}
    [round [Object] Object]
    ^{:static true}
    [partial_max [Object Object] Object]
    ^{:static true}
    [improve_basic_order [Object Object] Object]
    ^{:static true}
    [reduce_f [Object Object Object] Object]
    ^{:static true}
    [project [Object Object] Object]
    ^{:static true}
    [abs [Object] Object]
    ^{:static true}
    [all_closed_sets [Object Object] Object]
    ^{:static true}
    [all_closed_sets [Object Object Object] Object]
    ^{:static true}
    [subsets [Object] Object]
    ^{:static true}
    [seqable_p [Object] Object]
    ^{:static true}
    [difference [Object] Object]
    ^{:static true}
    [difference [Object Object] Object]
    ^{:static true}
    [difference [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [zip [Object Object] Object]
    ^{:static true}
    [split_at_last [Object Object] Object]
    ^{:static true}
    [transitive_reduction [Object] Object]
    ^{:static true}
    [transitive_reduction [Object Object] Object]
    ^{:static true}
    [parallel_closures [Object Object] Object]
    ^{:static true}
    [split_at_first [Object Object] Object]
    ^{:static true}
    [map_by_fn [Object Object] Object]
    ^{:static true}
    [lcm [Object Object] Object]
    ^{:static true}
    [with_str_out ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [null_context [Object] Object]
    ^{:static true}
    [attribute_reduced_p [Object] Object]
    ^{:static true}
    [context_product [Object Object] Object]
    ^{:static true}
    [direct_upper_concepts [Object Object] Object]
    ^{:static true}
    [context_xia_product [Object Object] Object]
    ^{:static true}
    [context_intersection [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object Object] Object]
    ^{:static true}
    [attributes [Object] Object]
    ^{:static true}
    [attribute_clarified_p [Object] Object]
    ^{:static true}
    [make_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [context_semiproduct [Object Object] Object]
    ^{:static true}
    [invert_context [Object] Object]
    ^{:static true}
    [context_sum [Object Object] Object]
    ^{:static true}
    [diag_context [Object] Object]
    ^{:static true}
    [context_apposition [Object Object] Object]
    ^{:static true}
    [context_p [Object] Object]
    ^{:static true}
    [object_reduced_p [Object] Object]
    ^{:static true}
    [intents [Object] Object]
    ^{:static true}
    [intents [Object Object] Object]
    ^{:static true}
    [adiag_context [Object] Object]
    ^{:static true}
    [adprime [Object Object] Object]
    ^{:static true}
    [concept_p [Object Object] Object]
    ^{:static true}
    [odprime [Object Object] Object]
    ^{:static true}
    [make_context [Object Object Object] Object]
    ^{:static true}
    [dual_context [Object] Object]
    ^{:static true}
    [context_attribute_closure [Object Object] Object]
    ^{:static true}
    [objects [Object] Object]
    ^{:static true}
    [rename_objects [Object Object] Object]
    ^{:static true}
    [object_derivation [Object Object] Object]
    ^{:static true}
    [context_object_closure [Object Object] Object]
    ^{:static true}
    [incidence [Object] Object]
    ^{:static true}
    [context_reduced_p [Object] Object]
    ^{:static true}
    [object_concept [Object Object] Object]
    ^{:static true}
    [down_arrows [Object] Object]
    ^{:static true}
    [__gt_Formal_Context [Object Object Object] Object]
    ^{:static true}
    [object_clarified_p [Object] Object]
    ^{:static true}
    [oprime [Object Object] Object]
    ^{:static true}
    [incident_p [Object Object Object] Object]
    ^{:static true}
    [one_context [Object] Object]
    ^{:static true}
    [reduce_context [Object] Object]
    ^{:static true}
    [subcontext_p [Object Object] Object]
    ^{:static true}
    [restrict_concept [Object Object] Object]
    ^{:static true}
    [context_disjoint_union [Object Object] Object]
    ^{:static true}
    [context_clarified_p [Object] Object]
    ^{:static true}
    [random_contexts [Object Object] Object]
    ^{:static true}
    [context_size [Object] Object]
    ^{:static true}
    [extents [Object] Object]
    ^{:static true}
    [extents [Object Object] Object]
    ^{:static true}
    [up_down_arrows [Object] Object]
    ^{:static true}
    [context_subposition [Object Object] Object]
    ^{:static true}
    [clarify_context [Object] Object]
    ^{:static true}
    [clarify_objects [Object] Object]
    ^{:static true}
    [context_transitive_closure [Object] Object]
    ^{:static true}
    [make_context_nc [Object Object Object] Object]
    ^{:static true}
    [aprime [Object Object] Object]
    ^{:static true}
    [context_to_string [Object] java.lang.String]
    ^{:static true}
    [context_to_string [Object Object Object] java.lang.String]
    ^{:static true}
    [reduce_objects [Object] Object]
    ^{:static true}
    [incidence_relation [Object] Object]
    ^{:static true}
    [concepts [Object] Object]
    ^{:static true}
    [print_context [Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [attribute_concept [Object Object] Object]
    ^{:static true}
    [context_union [Object Object] Object]
    ^{:static true}
    [rename_attributes [Object Object] Object]
    ^{:static true}
    [up_arrows [Object] Object]
    ^{:static true}
    [clarify_attributes [Object] Object]
    ^{:static true}
    [direct_lower_concepts [Object Object] Object]
    ^{:static true}
    [context_composition [Object Object] Object]
    ^{:static true}
    [reduce_attributes [Object] Object]
    ^{:static true}
    [random_context [Object Object] Object]
    ^{:static true}
    [random_context [Object Object Object] Object]
    ^{:static true}
    [attribute_derivation [Object Object] Object]
    ^{:static true}
    [__gt_Many_Valued_Context [Object Object Object] Object]
    ^{:static true}
    [dichotomic_scale [Object] Object]
    ^{:static true}
    [make_mv_context_nc [Object Object Object] Object]
    ^{:static true}
    [make_mv_context [Object Object Object] Object]
    ^{:static true}
    [interordinal_scale [Object] Object]
    ^{:static true}
    [interordinal_scale [Object Object Object] Object]
    ^{:static true}
    [interordinal_scale [Object Object Object Object] Object]
    ^{:static true}
    [values_of_object [Object Object] Object]
    ^{:static true}
    [values_of_attribute [Object Object] Object]
    ^{:static true}
    [nominal_scale [Object] Object]
    ^{:static true}
    [nominal_scale [Object Object] Object]
    ^{:static true}
    [ordinal_scale [Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object Object] Object]
    ^{:static true}
    [scale_mv_context [Object Object] Object]
    ^{:static true}
    [scale_mv_context [Object Object Object] Object]
    ^{:static true}
    [biordinal_scale [Object Object] Object]
    ^{:static true}
    [biordinal_scale [Object Object Object Object Object] Object]
    ^{:static true}
    [interval_scale [Object] Object]
    ^{:static true}
    [interval_scale [Object Object] Object]
    ^{:static true}
    [interval_scale [Object Object Object Object] Object]
    ^{:static true}
    [mv_context_to_string [Object] Object]
    ^{:static true}
    [mv_context_to_string [Object Object Object] Object]
    ^{:static true}
    [make_mv_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [confidence [Object Object] Object]
    ^{:static true}
    [canonical_base_from_base [Object] Object]
    ^{:static true}
    [frequent_closed_itemsets [Object Object] Object]
    ^{:static true}
    [holds_p [Object Object] Object]
    ^{:static true}
    [pseudo_intents [Object] Object]
    ^{:static true}
    [parallel_canonical_base_from_clop [Object Object] Object]
    ^{:static true}
    [parallel_canonical_base_from_clop [Object Object Object] Object]
    ^{:static true}
    [ryssel_base [Object] Object]
    ^{:static true}
    [premise [Object] Object]
    ^{:static true}
    [proper_premises [Object] Object]
    ^{:static true}
    [respects_p [Object Object] Object]
    ^{:static true}
    [proper_premises_for_attribute [Object Object] Object]
    ^{:static true}
    [clop_by_implications [Object] Object]
    ^{:static true}
    [stem_base [Object] Object]
    ^{:static true}
    [stem_base [Object Object] Object]
    ^{:static true}
    [stem_base [Object Object Object] Object]
    ^{:static true}
    [conclusion [Object] Object]
    ^{:static true}
    [pseudo_clop_by_implications [Object] Object]
    ^{:static true}
    [luxenburger_base [Object Object Object] Object]
    ^{:static true}
    [parallel_canonical_base [Object] Object]
    ^{:static true}
    [parallel_canonical_base [Object Object] Object]
    ^{:static true}
    [support [Object Object] Object]
    ^{:static true}
    [proper_conclusion [Object Object] Object]
    ^{:static true}
    [equivalent_implications_p [Object Object] Object]
    ^{:static true}
    [canonical_base [Object] Object]
    ^{:static true}
    [canonical_base [Object Object] Object]
    ^{:static true}
    [canonical_base [Object Object Object] Object]
    ^{:static true}
    [intersect_implicational_theories
     [Object "[Ljava.lang.Object;"]
     Object]
    ^{:static true}
    [complete_implication_set_p [Object Object] Object]
    ^{:static true}
    [close_under_implications [Object Object] Object]
    ^{:static true}
    [__gt_Implication [Object Object] Object]
    ^{:static true}
    [sound_implication_set_p [Object Object] Object]
    ^{:static true}
    [make_implication [Object Object] Object]
    ^{:static true}
    [follows_p [Object Object] Object]
    ^{:static true}
    [minimal_implication_set_p [Object] Object]
    ^{:static true}
    [proper_premise_implications [Object] Object]
    ^{:static true}
    [luxenburger_basis [Object Object Object] Object]
    ^{:static true}
    [pseudo_close_under_implications [Object Object] Object]
    ^{:static true}
    [irredundant_subset [Object] Object]
    ^{:static true}
    [canonical_base_from_clop [Object Object] Object]
    ^{:static true}
    [canonical_base_from_clop [Object Object Object] Object]
    ^{:static true}
    [canonical_base_from_clop [Object Object Object Object] Object]
    ^{:static true}
    [proper_premise_p [Object Object] Object]
    ^{:static true}
    [implication_p [Object] Object]
    ^{:static true}
    [stem_base_from_base [Object] Object]
    ^{:static true}
    [follows_semantically_p [Object Object] Object]
    ^{:static true}
    [default_handler_for_incomplete_counterexamples
     [Object Object Object Object]
     Object]
    ^{:static true}
    [explore_attributes ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [default_handler_for_complete_counterexamples
     [Object Object Object]
     Object]
    ^{:static true}
    [make_handler ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [standard_context [Object] Object]
    ^{:static true}
    [dual_lattice [Object] Object]
    ^{:static true}
    [lattice_one [Object] Object]
    ^{:static true}
    [has_lattice_order_p [Object] Object]
    ^{:static true}
    [base_set [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [directly_neighboured_p [Object Object Object] Object]
    ^{:static true}
    [sup [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [concept_lattice [Object] Object]
    ^{:static true}
    [modular_p [Object] Object]
    ^{:static true}
    [lattice_zero [Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object Object] Object]
    ^{:static true}
    [lattice_sup_irreducibles [Object] Object]
    ^{:static true}
    [distributive_p [Object] Object]
    ^{:static true}
    [lattice_doubly_irreducibles [Object] Object]
    ^{:static true}
    [lattice_coatoms [Object] Object]
    ^{:static true}
    [lattice_atoms [Object] Object]
    ^{:static true}
    [__gt_Lattice [Object Object Object Object] Object]
    ^{:static true}
    [make_lattice ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [inf [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [lattice_lower_neighbours [Object Object] Object]
    ^{:static true}
    [lattice_upper_neighbours [Object Object] Object]
    ^{:static true}
    [lattice_inf_irreducibles [Object] Object]
    ^{:static true}
    [order [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [bond_p [Object Object Object] Object]
    ^{:static true}
    [compatible_subcontexts [Object] Object]
    ^{:static true}
    [compatible_subcontext_p [Object Object] Object]
    ^{:static true}
    [all_bonds [Object Object] Object]
    ^{:static true}
    [context_from_clop [Object Object] Object]
    ^{:static true}
    [all_bonds_by_shared_intents [Object Object] Object]
    ^{:static true}
    [implication_context [Object] Object]
    ^{:static true}
    [smallest_bond [Object Object Object] Object]
    ^{:static true}
    [all_shared_intents [Object Object] Object]
    ^{:static true}
    [latex [Object] Object]
    ^{:static true}
    [latex [Object Object] Object]
    ^{:static true}
    [write_context [Object Object Object] Object]
    ^{:static true}
    [write_context [Object Object] Object]
    ^{:static true}
    [get_default_context_format [] Object]
    ^{:static true}
    [list_context_output_formats [] Object]
    ^{:static true}
    [list_context_input_formats [] Object]
    ^{:static true}
    [list_context_formats [] Object]
    ^{:static true}
    [read_context [Object] Object]
    ^{:static true}
    [read_context [Object Object] Object]
    ^{:static true}
    [set_default_context_format_f [Object] Object]
    ^{:static true}
    [list_lattice_output_formats [] Object]
    ^{:static true}
    [list_lattice_input_formats [] Object]
    ^{:static true}
    [get_default_lattice_format [] Object]
    ^{:static true}
    [set_default_lattice_format_f [Object] Object]
    ^{:static true}
    [list_lattice_formats [] Object]
    ^{:static true}
    [read_lattice [Object] Object]
    ^{:static true}
    [read_lattice [Object Object] Object]
    ^{:static true}
    [write_lattice [Object Object Object] Object]
    ^{:static true}
    [write_lattice [Object Object] Object]
    ^{:static true}
    [list_layout_output_formats [] Object]
    ^{:static true}
    [read_layout [Object] Object]
    ^{:static true}
    [read_layout [Object Object] Object]
    ^{:static true}
    [set_default_layout_format_f [Object] Object]
    ^{:static true}
    [get_default_layout_format [] Object]
    ^{:static true}
    [list_layout_formats [] Object]
    ^{:static true}
    [list_layout_input_formats [] Object]
    ^{:static true}
    [write_layout [Object Object Object] Object]
    ^{:static true}
    [write_layout [Object Object] Object]
    ^{:static true}
    [list_mv_context_output_formats [] Object]
    ^{:static true}
    [list_mv_context_formats [] Object]
    ^{:static true}
    [get_default_mv_context_format [] Object]
    ^{:static true}
    [list_mv_context_input_formats [] Object]
    ^{:static true}
    [write_mv_context [Object Object Object] Object]
    ^{:static true}
    [write_mv_context [Object Object] Object]
    ^{:static true}
    [set_default_mv_context_format_f [Object] Object]
    ^{:static true}
    [read_mv_context [Object] Object]
    ^{:static true}
    [read_mv_context [Object Object] Object]
    ^{:static true}
    [write_many_valued_context [Object Object Object] Object]
    ^{:static true}
    [write_many_valued_context [Object Object] Object]
    ^{:static true}
    [read_many_valued_context [Object] Object]
    ^{:static true}
    [read_many_valued_context [Object Object] Object]
    ^{:static true}
    [inf_additive_layout [Object] Object])))
 (clojure.core/import 'conexp.fca.contexts.Context)
 (clojure.core/import 'conexp.fca.lattices.Lattice)
 (clojure.core/defn
  conexp-clj-get_resource
  ([res] (conexp.base/get-resource res)))
 nil
 (clojure.core/defn
  conexp-clj-hash_combine_hash
  ([^"[Ljava.lang.Object;" args] (conexp.base/hash-combine-hash args)))
 (clojure.core/defn
  conexp-clj-disjoint_union
  ([^"[Ljava.lang.Object;" sets] (conexp.base/disjoint-union sets)))
 (clojure.core/defn
  conexp-clj-conexp_version
  ([] (conexp.base/conexp-version)))
 (clojure.core/defn conexp-clj-gcd ([a b] (conexp.base/gcd a b)))
 (clojure.core/defn
  conexp-clj-next_closed_set_in_family
  ([predicate base clop A]
   (conexp.base/next-closed-set-in-family predicate base clop A)))
 (clojure.core/defn conexp-clj-quit ([] (conexp.base/quit)))
 (clojure.core/defn
  conexp-clj-lectic__lt__i
  ([base i A B] (conexp.base/lectic-<_i base i A B)))
 (clojure.core/defn
  conexp-clj-graph_of_function_p
  ([relation source target]
   (conexp.base/graph-of-function? relation source target)))
 (clojure.core/defn
  conexp-clj-minimal_generating_subsets
  ([clop A] (conexp.base/minimal-generating-subsets clop A)))
 nil
 (clojure.core/defn
  conexp-clj-union
  ([] (conexp.base/union))
  ([s1] (conexp.base/union s1))
  ([s1 s2] (conexp.base/union s1 s2))
  ([s1 s2 ^"[Ljava.lang.Object;" sets] (conexp.base/union s1 s2 sets)))
 (clojure.core/defn
  conexp-clj-topological_sort
  ([comp coll] (conexp.base/topological-sort comp coll)))
 (clojure.core/defn
  conexp-clj-cross_product
  ([^"[Ljava.lang.Object;" sets] (conexp.base/cross-product sets)))
 (clojure.core/defn
  conexp-clj-superset_p
  ([set1 set2] (conexp.base/superset? set1 set2)))
 (clojure.core/defn conexp-clj-floor ([n] (conexp.base/floor n)))
 (clojure.core/defn
  conexp-clj-exact_integer_sqrt
  ([n] (conexp.base/exact-integer-sqrt n)))
 (clojure.core/defn
  conexp-clj-subset_p
  ([set1 set2] (conexp.base/subset? set1 set2)))
 (clojure.core/defn
  conexp-clj-next_closed_set
  ([base clop A] (conexp.base/next-closed-set base clop A)))
 (clojure.core/defn
  conexp-clj-join
  ([xrel yrel] (conexp.base/join xrel yrel))
  ([xrel yrel km] (conexp.base/join xrel yrel km)))
 (clojure.core/defn
  conexp-clj-set_of_range
  ([end] (conexp.base/set-of-range end))
  ([start end] (conexp.base/set-of-range start end))
  ([start end step] (conexp.base/set-of-range start end step)))
 (clojure.core/defn
  conexp-clj-ask
  ([prompt read ^"[Ljava.lang.Object;" preds-and-fail-messages]
   (conexp.base/ask prompt read preds-and-fail-messages)))
 (clojure.core/defn
  conexp-clj-first_non_nil
  ([seq] (conexp.base/first-non-nil seq)))
 (clojure.core/defn
  conexp-clj-lectic__lt_
  ([base A B] (conexp.base/lectic-< base A B)))
 (clojure.core/defn
  conexp-clj-select
  ([pred xset] (conexp.base/select pred xset)))
 (clojure.core/defn
  conexp-clj-illegal_state
  ([^"[Ljava.lang.Object;" strings]
   (conexp.base/illegal-state strings)))
 (clojure.core/defn
  conexp-clj-ensure_length
  ([string ^long length] (conexp.base/ensure-length string length))
  ([string ^long length padding]
   (conexp.base/ensure-length string length padding)))
 (clojure.core/defn conexp-clj-ceil ([n] (conexp.base/ceil n)))
 (clojure.core/defn
  conexp-clj-sort_by_second
  ([x y] (conexp.base/sort-by-second x y)))
 (clojure.core/defn
  conexp-clj-integer_length
  ([n] (conexp.base/integer-length n)))
 (clojure.core/defn
  conexp-clj-proper_subset_p
  ([set-1 set-2] (conexp.base/proper-subset? set-1 set-2)))
 (clojure.core/defn
  conexp-clj-transitive_closure
  ([pairs] (conexp.base/transitive-closure pairs)))
 (clojure.core/defn
  conexp-clj-singleton_p
  ([x] (conexp.base/singleton? x)))
 (clojure.core/defn
  conexp-clj-intersection
  ([s1] (conexp.base/intersection s1))
  ([s1 s2] (conexp.base/intersection s1 s2))
  ([s1 s2 ^"[Ljava.lang.Object;" sets]
   (conexp.base/intersection s1 s2 sets)))
 (clojure.core/defn conexp-clj-expt ([a b] (conexp.base/expt a b)))
 (clojure.core/defn
  conexp-clj-distinct_by_key
  ([sequence key] (conexp.base/distinct-by-key sequence key)))
 (clojure.core/defn conexp-clj-tails ([sqn] (conexp.base/tails sqn)))
 (clojure.core/defn
  conexp-clj-sort_by_first
  ([x y] (conexp.base/sort-by-first x y)))
 (clojure.core/defn conexp-clj-inits ([sqn] (conexp.base/inits sqn)))
 (clojure.core/defn
  conexp-clj-all_closed_sets_in_family
  ([predicate base clop]
   (conexp.base/all-closed-sets-in-family predicate base clop))
  ([predicate base clop initial]
   (conexp.base/all-closed-sets-in-family
    predicate
    base
    clop
    initial)))
 (clojure.core/defn conexp-clj-now ([] (conexp.base/now)))
 (clojure.core/defn
  conexp-clj-index
  ([xrel ks] (conexp.base/index xrel ks)))
 nil
 (clojure.core/defn
  conexp-clj-clojure_type
  ([thing] (conexp.base/clojure-type thing)))
 (clojure.core/defn conexp-clj-sqrt ([n] (conexp.base/sqrt n)))
 (clojure.core/defn
  conexp-clj-yes_or_no_p
  ([question] (conexp.base/yes-or-no? question)))
 (clojure.core/defn
  conexp-clj-warn
  ([message] (conexp.base/warn message)))
 (clojure.core/defn
  conexp-clj-rename_keys
  ([map kmap] (conexp.base/rename-keys map kmap)))
 (clojure.core/defn
  conexp-clj-order_by
  ([sequence] (conexp.base/order-by sequence)))
 (clojure.core/defn
  conexp-clj-unsupported_operation
  ([^"[Ljava.lang.Object;" strings]
   (conexp.base/unsupported-operation strings)))
 nil
 (clojure.core/defn
  conexp-clj-minimum_set_covers
  ([base-set sets] (conexp.base/minimum-set-covers base-set sets)))
 (clojure.core/defn
  conexp-clj-proper_superset_p
  ([set-1 set-2] (conexp.base/proper-superset? set-1 set-2)))
 (clojure.core/defn
  conexp-clj-has_version_p
  ([{^int my-major :major, ^int my-minor :minor, ^int my-patch :patch}]
   (conexp.base/has-version?
    {^int my-major :major,
     ^int my-minor :minor,
     ^int my-patch :patch})))
 (clojure.core/defn
  conexp-clj-reflexive_transitive_closure
  ([base-set pairs]
   (conexp.base/reflexive-transitive-closure base-set pairs)))
 (clojure.core/defn
  conexp-clj-illegal_argument
  ([^"[Ljava.lang.Object;" strings]
   (conexp.base/illegal-argument strings)))
 (clojure.core/defn
  conexp-clj-immigrate
  ([^"[Ljava.lang.Object;" ns-names] (conexp.base/immigrate ns-names)))
 (clojure.core/defn
  conexp-clj-map_invert
  ([m] (conexp.base/map-invert m)))
 (clojure.core/defn
  conexp-clj-not_yet_implemented
  ([] (conexp.base/not-yet-implemented)))
 (clojure.core/defn
  conexp-clj-ensure_seq
  ([x] (conexp.base/ensure-seq x)))
 (clojure.core/defn
  conexp-clj-rename
  ([xrel kmap] (conexp.base/rename xrel kmap)))
 (clojure.core/defn
  conexp-clj-minimal_hypergraph_transversals
  ([vertices edges]
   (conexp.base/minimal-hypergraph-transversals vertices edges)))
 (clojure.core/defn
  conexp-clj-partial_min
  ([<= xs] (conexp.base/partial-min <= xs)))
 (clojure.core/defn
  conexp-clj-_lt__eq__gt_
  ([a b] (conexp.base/<=> a b)))
 (clojure.core/defn
  conexp-clj-to_set
  ([thing] (conexp.base/to-set thing)))
 (clojure.core/defn conexp-clj-round ([n] (conexp.base/round n)))
 nil
 (clojure.core/defn
  conexp-clj-partial_max
  ([<= xs] (conexp.base/partial-max <= xs)))
 (clojure.core/defn
  conexp-clj-improve_basic_order
  ([base clop] (conexp.base/improve-basic-order base clop)))
 (clojure.core/defn
  conexp-clj-reduce_f
  ([fn initial-value coll]
   (conexp.base/reduce! fn initial-value coll)))
 (clojure.core/defn
  conexp-clj-project
  ([xrel ks] (conexp.base/project xrel ks)))
 (clojure.core/defn conexp-clj-abs ([n] (conexp.base/abs n)))
 (clojure.core/defn
  conexp-clj-all_closed_sets
  ([base clop] (conexp.base/all-closed-sets base clop))
  ([base clop initial]
   (conexp.base/all-closed-sets base clop initial)))
 nil
 (clojure.core/defn
  conexp-clj-subsets
  ([base-set] (conexp.base/subsets base-set)))
 (clojure.core/defn
  conexp-clj-seqable_p
  ([x] (conexp.base/seqable? x)))
 (clojure.core/defn
  conexp-clj-difference
  ([s1] (conexp.base/difference s1))
  ([s1 s2] (conexp.base/difference s1 s2))
  ([s1 s2 ^"[Ljava.lang.Object;" sets]
   (conexp.base/difference s1 s2 sets)))
 (clojure.core/defn
  conexp-clj-zip
  ([seq-1 seq-2] (conexp.base/zip seq-1 seq-2)))
 (clojure.core/defn
  conexp-clj-split_at_last
  ([predicate sequence]
   (conexp.base/split-at-last predicate sequence)))
 (clojure.core/defn
  conexp-clj-transitive_reduction
  ([pairs] (conexp.base/transitive-reduction pairs))
  ([base pred] (conexp.base/transitive-reduction base pred)))
 nil
 (clojure.core/defn
  conexp-clj-parallel_closures
  ([base clop] (conexp.base/parallel-closures base clop)))
 (clojure.core/defn
  conexp-clj-split_at_first
  ([predicate sequence]
   (conexp.base/split-at-first predicate sequence)))
 (clojure.core/defn
  conexp-clj-map_by_fn
  ([function keys] (conexp.base/map-by-fn function keys)))
 (clojure.core/defn conexp-clj-lcm ([a b] (conexp.base/lcm a b)))
 (clojure.core/defn
  conexp-clj-with_str_out
  ([^"[Ljava.lang.Object;" body] (conexp.base/with-str-out body)))
 (clojure.core/defn
  conexp-clj-null_context
  ([base-set] (conexp.fca.contexts/null-context base-set)))
 (clojure.core/defn
  conexp-clj-attribute_reduced_p
  ([ctx] (conexp.fca.contexts/attribute-reduced? ctx)))
 (clojure.core/defn
  conexp-clj-context_product
  ([ctx-1 ctx-2] (conexp.fca.contexts/context-product ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-direct_upper_concepts
  ([ctx [A B]] (conexp.fca.contexts/direct-upper-concepts ctx [A B])))
 (clojure.core/defn
  conexp-clj-context_xia_product
  ([ctx-1 ctx-2]
   (conexp.fca.contexts/context-xia-product ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-context_intersection
  ([ctx1 ctx2] (conexp.fca.contexts/context-intersection ctx1 ctx2)))
 (clojure.core/defn
  conexp-clj-rand_context
  ([base-set fill-rate]
   (conexp.fca.contexts/rand-context base-set fill-rate))
  ([objects attributes fill-rate]
   (conexp.fca.contexts/rand-context objects attributes fill-rate)))
 (clojure.core/defn
  conexp-clj-attributes
  ([ctx] (conexp.fca.contexts/attributes ctx)))
 (clojure.core/defn
  conexp-clj-attribute_clarified_p
  ([ctx] (conexp.fca.contexts/attribute-clarified? ctx)))
 (clojure.core/defn
  conexp-clj-make_context_from_matrix
  ([G M bits] (conexp.fca.contexts/make-context-from-matrix G M bits)))
 (clojure.core/defn
  conexp-clj-context_semiproduct
  ([ctx-1 ctx-2]
   (conexp.fca.contexts/context-semiproduct ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-invert_context
  ([ctx] (conexp.fca.contexts/invert-context ctx)))
 (clojure.core/defn
  conexp-clj-context_sum
  ([ctx-1 ctx-2] (conexp.fca.contexts/context-sum ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-diag_context
  ([base-set] (conexp.fca.contexts/diag-context base-set)))
 (clojure.core/defn
  conexp-clj-context_apposition
  ([ctx-1 ctx-2] (conexp.fca.contexts/context-apposition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-context_p
  ([thing] (conexp.fca.contexts/context? thing)))
 (clojure.core/defn
  conexp-clj-object_reduced_p
  ([ctx] (conexp.fca.contexts/object-reduced? ctx)))
 (clojure.core/defn
  conexp-clj-intents
  ([ctx] (conexp.fca.contexts/intents ctx))
  ([ctx pred] (conexp.fca.contexts/intents ctx pred)))
 (clojure.core/defn
  conexp-clj-adiag_context
  ([base-set] (conexp.fca.contexts/adiag-context base-set)))
 (clojure.core/defn
  conexp-clj-adprime
  ([ctx set-of-attributes]
   (conexp.fca.contexts/adprime ctx set-of-attributes)))
 (clojure.core/defn
  conexp-clj-concept_p
  ([ctx [set-of-obj set-of-att]]
   (conexp.fca.contexts/concept? ctx [set-of-obj set-of-att])))
 (clojure.core/defn
  conexp-clj-odprime
  ([ctx set-of-objects]
   (conexp.fca.contexts/odprime ctx set-of-objects)))
 (clojure.core/defn
  conexp-clj-make_context
  ([objects attributes incidence]
   (conexp.fca.contexts/make-context objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-dual_context
  ([ctx] (conexp.fca.contexts/dual-context ctx)))
 (clojure.core/defn
  conexp-clj-context_attribute_closure
  ([ctx set-of-attributes]
   (conexp.fca.contexts/context-attribute-closure
    ctx
    set-of-attributes)))
 (clojure.core/defn
  conexp-clj-objects
  ([ctx] (conexp.fca.contexts/objects ctx)))
 (clojure.core/defn
  conexp-clj-rename_objects
  ([ctx old-to-new]
   (conexp.fca.contexts/rename-objects ctx old-to-new)))
 (clojure.core/defn
  conexp-clj-object_derivation
  ([ctx objects] (conexp.fca.contexts/object-derivation ctx objects)))
 (clojure.core/defn
  conexp-clj-context_object_closure
  ([ctx set-of-objects]
   (conexp.fca.contexts/context-object-closure ctx set-of-objects)))
 (clojure.core/defn
  conexp-clj-incidence
  ([ctx] (conexp.fca.contexts/incidence ctx)))
 (clojure.core/defn
  conexp-clj-context_reduced_p
  ([ctx] (conexp.fca.contexts/context-reduced? ctx)))
 (clojure.core/defn
  conexp-clj-object_concept
  ([ctx g] (conexp.fca.contexts/object-concept ctx g)))
 (clojure.core/defn
  conexp-clj-down_arrows
  ([ctx] (conexp.fca.contexts/down-arrows ctx)))
 (clojure.core/defn
  conexp-clj-__gt_Formal_Context
  ([objects attributes incidence]
   (conexp.fca.contexts/->Formal-Context
    objects
    attributes
    incidence)))
 (clojure.core/defn
  conexp-clj-object_clarified_p
  ([ctx] (conexp.fca.contexts/object-clarified? ctx)))
 (clojure.core/defn
  conexp-clj-oprime
  ([ctx objects] (conexp.fca.contexts/oprime ctx objects)))
 (clojure.core/defn
  conexp-clj-incident_p
  ([ctx g m] (conexp.fca.contexts/incident? ctx g m)))
 (clojure.core/defn
  conexp-clj-one_context
  ([base-set] (conexp.fca.contexts/one-context base-set)))
 (clojure.core/defn
  conexp-clj-reduce_context
  ([ctx] (conexp.fca.contexts/reduce-context ctx)))
 (clojure.core/defn
  conexp-clj-subcontext_p
  ([ctx-1 ctx-2] (conexp.fca.contexts/subcontext? ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-restrict_concept
  ([concept subcontext]
   (conexp.fca.contexts/restrict-concept concept subcontext)))
 (clojure.core/defn
  conexp-clj-context_disjoint_union
  ([ctx-1 ctx-2]
   (conexp.fca.contexts/context-disjoint-union ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-context_clarified_p
  ([ctx] (conexp.fca.contexts/context-clarified? ctx)))
 (clojure.core/defn
  conexp-clj-random_contexts
  ([number upper-limit]
   (conexp.fca.contexts/random-contexts number upper-limit)))
 (clojure.core/defn
  conexp-clj-context_size
  ([ctx] (conexp.fca.contexts/context-size ctx)))
 (clojure.core/defn
  conexp-clj-extents
  ([ctx] (conexp.fca.contexts/extents ctx))
  ([ctx pred] (conexp.fca.contexts/extents ctx pred)))
 (clojure.core/defn
  conexp-clj-up_down_arrows
  ([ctx] (conexp.fca.contexts/up-down-arrows ctx)))
 (clojure.core/defn
  conexp-clj-context_subposition
  ([ctx-1 ctx-2]
   (conexp.fca.contexts/context-subposition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-clarify_context
  ([ctx] (conexp.fca.contexts/clarify-context ctx)))
 (clojure.core/defn
  conexp-clj-clarify_objects
  ([ctx] (conexp.fca.contexts/clarify-objects ctx)))
 (clojure.core/defn
  conexp-clj-context_transitive_closure
  ([ctx] (conexp.fca.contexts/context-transitive-closure ctx)))
 nil
 (clojure.core/defn
  conexp-clj-make_context_nc
  ([objects attributes incidence]
   (conexp.fca.contexts/make-context-nc objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-aprime
  ([ctx attributes] (conexp.fca.contexts/aprime ctx attributes)))
 (clojure.core/defn
  conexp-clj-context_to_string
  ([ctx] (conexp.fca.contexts/context-to-string ctx))
  ([ctx order-on-objects order-on-attributes]
   (conexp.fca.contexts/context-to-string
    ctx
    order-on-objects
    order-on-attributes)))
 (clojure.core/defn
  conexp-clj-reduce_objects
  ([ctx] (conexp.fca.contexts/reduce-objects ctx)))
 (clojure.core/defn
  conexp-clj-incidence_relation
  ([ctx] (conexp.fca.contexts/incidence-relation ctx)))
 (clojure.core/defn
  conexp-clj-concepts
  ([ctx] (conexp.fca.contexts/concepts ctx)))
 (clojure.core/defn
  conexp-clj-print_context
  ([ctx ^"[Ljava.lang.Object;" args]
   (conexp.fca.contexts/print-context ctx args)))
 (clojure.core/defn
  conexp-clj-attribute_concept
  ([ctx m] (conexp.fca.contexts/attribute-concept ctx m)))
 (clojure.core/defn
  conexp-clj-context_union
  ([ctx-1 ctx-2] (conexp.fca.contexts/context-union ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-rename_attributes
  ([ctx old-to-new]
   (conexp.fca.contexts/rename-attributes ctx old-to-new)))
 (clojure.core/defn
  conexp-clj-up_arrows
  ([ctx] (conexp.fca.contexts/up-arrows ctx)))
 (clojure.core/defn
  conexp-clj-clarify_attributes
  ([ctx] (conexp.fca.contexts/clarify-attributes ctx)))
 (clojure.core/defn
  conexp-clj-direct_lower_concepts
  ([ctx [A B]] (conexp.fca.contexts/direct-lower-concepts ctx [A B])))
 (clojure.core/defn
  conexp-clj-context_composition
  ([ctx-1 ctx-2]
   (conexp.fca.contexts/context-composition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-reduce_attributes
  ([ctx] (conexp.fca.contexts/reduce-attributes ctx)))
 (clojure.core/defn
  conexp-clj-random_context
  ([base-set fill-rate]
   (conexp.fca.contexts/random-context base-set fill-rate))
  ([objects attributes fill-rate]
   (conexp.fca.contexts/random-context objects attributes fill-rate)))
 (clojure.core/defn
  conexp-clj-attribute_derivation
  ([ctx attributes]
   (conexp.fca.contexts/attribute-derivation ctx attributes)))
 (clojure.core/defn
  conexp-clj-__gt_Many_Valued_Context
  ([objects attributes incidence]
   (conexp.fca.many-valued-contexts/->Many-Valued-Context
    objects
    attributes
    incidence)))
 (clojure.core/defn
  conexp-clj-dichotomic_scale
  ([values] (conexp.fca.many-valued-contexts/dichotomic-scale values)))
 (clojure.core/defn
  conexp-clj-make_mv_context_nc
  ([objects attributes incidence]
   (conexp.fca.many-valued-contexts/make-mv-context-nc
    objects
    attributes
    incidence)))
 (clojure.core/defn
  conexp-clj-make_mv_context
  ([objects attributes incidence]
   (conexp.fca.many-valued-contexts/make-mv-context
    objects
    attributes
    incidence)))
 (clojure.core/defn
  conexp-clj-interordinal_scale
  ([values]
   (conexp.fca.many-valued-contexts/interordinal-scale values))
  ([values <= >=]
   (conexp.fca.many-valued-contexts/interordinal-scale values <= >=))
  ([values others <= >=]
   (conexp.fca.many-valued-contexts/interordinal-scale
    values
    others
    <=
    >=)))
 (clojure.core/defn
  conexp-clj-values_of_object
  ([mv-ctx g]
   (conexp.fca.many-valued-contexts/values-of-object mv-ctx g)))
 (clojure.core/defn
  conexp-clj-values_of_attribute
  ([mv-ctx m]
   (conexp.fca.many-valued-contexts/values-of-attribute mv-ctx m)))
 (clojure.core/defn
  conexp-clj-nominal_scale
  ([values] (conexp.fca.many-valued-contexts/nominal-scale values))
  ([values others]
   (conexp.fca.many-valued-contexts/nominal-scale values others)))
 (clojure.core/defn
  conexp-clj-ordinal_scale
  ([values] (conexp.fca.many-valued-contexts/ordinal-scale values))
  ([values <=]
   (conexp.fca.many-valued-contexts/ordinal-scale values <=))
  ([values others <=]
   (conexp.fca.many-valued-contexts/ordinal-scale values others <=)))
 (clojure.core/defn
  conexp-clj-scale_mv_context
  ([mv-ctx scales]
   (conexp.fca.many-valued-contexts/scale-mv-context mv-ctx scales))
  ([mv-ctx scales default]
   (conexp.fca.many-valued-contexts/scale-mv-context
    mv-ctx
    scales
    default)))
 (clojure.core/defn
  conexp-clj-biordinal_scale
  ([values n]
   (conexp.fca.many-valued-contexts/biordinal-scale values n))
  ([values others n <= >=]
   (conexp.fca.many-valued-contexts/biordinal-scale
    values
    others
    n
    <=
    >=)))
 (clojure.core/defn
  conexp-clj-interval_scale
  ([values] (conexp.fca.many-valued-contexts/interval-scale values))
  ([values others]
   (conexp.fca.many-valued-contexts/interval-scale values others))
  ([values others < >=]
   (conexp.fca.many-valued-contexts/interval-scale
    values
    others
    <
    >=)))
 (clojure.core/defn
  conexp-clj-mv_context_to_string
  ([mv-ctx]
   (conexp.fca.many-valued-contexts/mv-context-to-string mv-ctx))
  ([mv-ctx order-on-objects order-on-attributes]
   (conexp.fca.many-valued-contexts/mv-context-to-string
    mv-ctx
    order-on-objects
    order-on-attributes)))
 (clojure.core/defn
  conexp-clj-make_mv_context_from_matrix
  ([objects attributes values]
   (conexp.fca.many-valued-contexts/make-mv-context-from-matrix
    objects
    attributes
    values)))
 (clojure.core/defn
  conexp-clj-confidence
  ([implication context]
   (conexp.fca.implications/confidence implication context)))
 (clojure.core/defn
  conexp-clj-canonical_base_from_base
  ([implications]
   (conexp.fca.implications/canonical-base-from-base implications)))
 (clojure.core/defn
  conexp-clj-frequent_closed_itemsets
  ([context minsupp]
   (conexp.fca.implications/frequent-closed-itemsets context minsupp)))
 (clojure.core/defn
  conexp-clj-holds_p
  ([impl ctx] (conexp.fca.implications/holds? impl ctx)))
 (clojure.core/defn
  conexp-clj-pseudo_intents
  ([ctx] (conexp.fca.implications/pseudo-intents ctx)))
 (clojure.core/defn
  conexp-clj-parallel_canonical_base_from_clop
  ([clop base]
   (conexp.fca.implications/parallel-canonical-base-from-clop
    clop
    base))
  ([clop base background-knowledge]
   (conexp.fca.implications/parallel-canonical-base-from-clop
    clop
    base
    background-knowledge)))
 (clojure.core/defn
  conexp-clj-ryssel_base
  ([ctx] (conexp.fca.implications/ryssel-base ctx)))
 (clojure.core/defn
  conexp-clj-premise
  ([thing] (conexp.fca.implications/premise thing)))
 (clojure.core/defn
  conexp-clj-proper_premises
  ([ctx] (conexp.fca.implications/proper-premises ctx)))
 (clojure.core/defn
  conexp-clj-respects_p
  ([set impl] (conexp.fca.implications/respects? set impl)))
 (clojure.core/defn
  conexp-clj-proper_premises_for_attribute
  ([ctx m]
   (conexp.fca.implications/proper-premises-for-attribute ctx m)))
 (clojure.core/defn
  conexp-clj-clop_by_implications
  ([implications]
   (conexp.fca.implications/clop-by-implications implications)))
 (clojure.core/defn
  conexp-clj-stem_base
  ([ctx] (conexp.fca.implications/stem-base ctx))
  ([ctx background-knowledge]
   (conexp.fca.implications/stem-base ctx background-knowledge))
  ([ctx background-knowledge predicate]
   (conexp.fca.implications/stem-base
    ctx
    background-knowledge
    predicate)))
 (clojure.core/defn
  conexp-clj-conclusion
  ([thing] (conexp.fca.implications/conclusion thing)))
 (clojure.core/defn
  conexp-clj-pseudo_clop_by_implications
  ([implications]
   (conexp.fca.implications/pseudo-clop-by-implications implications)))
 (clojure.core/defn
  conexp-clj-luxenburger_base
  ([context minsupp-or-predicate minconf]
   (conexp.fca.implications/luxenburger-base
    context
    minsupp-or-predicate
    minconf)))
 (clojure.core/defn
  conexp-clj-parallel_canonical_base
  ([ctx] (conexp.fca.implications/parallel-canonical-base ctx))
  ([ctx background-knowledge]
   (conexp.fca.implications/parallel-canonical-base
    ctx
    background-knowledge)))
 (clojure.core/defn
  conexp-clj-support
  ([thing ctx] (conexp.fca.implications/support thing ctx)))
 (clojure.core/defn
  conexp-clj-proper_conclusion
  ([ctx A] (conexp.fca.implications/proper-conclusion ctx A)))
 (clojure.core/defn
  conexp-clj-equivalent_implications_p
  ([impls-1 impls-2]
   (conexp.fca.implications/equivalent-implications? impls-1 impls-2)))
 (clojure.core/defn
  conexp-clj-canonical_base
  ([ctx] (conexp.fca.implications/canonical-base ctx))
  ([ctx background-knowledge]
   (conexp.fca.implications/canonical-base ctx background-knowledge))
  ([ctx background-knowledge predicate]
   (conexp.fca.implications/canonical-base
    ctx
    background-knowledge
    predicate)))
 (clojure.core/defn
  conexp-clj-intersect_implicational_theories
  ([base-set ^"[Ljava.lang.Object;" implication-sets]
   (conexp.fca.implications/intersect-implicational-theories
    base-set
    implication-sets)))
 (clojure.core/defn
  conexp-clj-complete_implication_set_p
  ([ctx impl-set]
   (conexp.fca.implications/complete-implication-set? ctx impl-set)))
 (clojure.core/defn
  conexp-clj-close_under_implications
  ([implications input-set]
   (conexp.fca.implications/close-under-implications
    implications
    input-set)))
 (clojure.core/defn
  conexp-clj-__gt_Implication
  ([premise conclusion]
   (conexp.fca.implications/->Implication premise conclusion)))
 (clojure.core/defn
  conexp-clj-sound_implication_set_p
  ([ctx impl-set]
   (conexp.fca.implications/sound-implication-set? ctx impl-set)))
 (clojure.core/defn
  conexp-clj-make_implication
  ([premise conclusion]
   (conexp.fca.implications/make-implication premise conclusion)))
 (clojure.core/defn
  conexp-clj-follows_p
  ([implication implications]
   (conexp.fca.implications/follows? implication implications)))
 (clojure.core/defn
  conexp-clj-minimal_implication_set_p
  ([impl-set]
   (conexp.fca.implications/minimal-implication-set? impl-set)))
 (clojure.core/defn
  conexp-clj-proper_premise_implications
  ([ctx] (conexp.fca.implications/proper-premise-implications ctx)))
 (clojure.core/defn
  conexp-clj-luxenburger_basis
  ([context minsupp-or-predicate minconf]
   (conexp.fca.implications/luxenburger-basis
    context
    minsupp-or-predicate
    minconf)))
 (clojure.core/defn
  conexp-clj-pseudo_close_under_implications
  ([implications set]
   (conexp.fca.implications/pseudo-close-under-implications
    implications
    set)))
 (clojure.core/defn
  conexp-clj-irredundant_subset
  ([impls] (conexp.fca.implications/irredundant-subset impls)))
 (clojure.core/defn
  conexp-clj-canonical_base_from_clop
  ([clop base]
   (conexp.fca.implications/canonical-base-from-clop clop base))
  ([clop base background-knowledge]
   (conexp.fca.implications/canonical-base-from-clop
    clop
    base
    background-knowledge))
  ([clop base background-knowledge predicate]
   (conexp.fca.implications/canonical-base-from-clop
    clop
    base
    background-knowledge
    predicate)))
 (clojure.core/defn
  conexp-clj-proper_premise_p
  ([ctx A] (conexp.fca.implications/proper-premise? ctx A)))
 (clojure.core/defn
  conexp-clj-implication_p
  ([thing] (conexp.fca.implications/implication? thing)))
 (clojure.core/defn
  conexp-clj-stem_base_from_base
  ([implications]
   (conexp.fca.implications/stem-base-from-base implications)))
 (clojure.core/defn
  conexp-clj-follows_semantically_p
  ([implication implications]
   (conexp.fca.implications/follows-semantically?
    implication
    implications)))
 (clojure.core/defn
  conexp-clj-default_handler_for_incomplete_counterexamples
  ([possible-ctx certain-ctx known impl]
   (conexp.fca.exploration/default-handler-for-incomplete-counterexamples
    possible-ctx
    certain-ctx
    known
    impl)))
 (clojure.core/defn
  conexp-clj-explore_attributes
  ([{:keys
     [possible-context
      certain-context
      context
      background-knowledge
      handler
      incomplete-counterexamples]}]
   (conexp.fca.exploration/explore-attributes
    {:keys
     [possible-context
      certain-context
      context
      background-knowledge
      handler
      incomplete-counterexamples]})))
 (clojure.core/defn
  conexp-clj-default_handler_for_complete_counterexamples
  ([ctx known impl]
   (conexp.fca.exploration/default-handler-for-complete-counterexamples
    ctx
    known
    impl)))
 (clojure.core/defn
  conexp-clj-make_handler
  ([{:keys [automorphisms incomplete-counterexamples?],
     :or {automorphisms #{}, incomplete-counterexamples? false}}]
   (conexp.fca.exploration/make-handler
    {:keys [automorphisms incomplete-counterexamples?],
     :or {automorphisms #{}, incomplete-counterexamples? false}})))
 (clojure.core/defn
  conexp-clj-standard_context
  ([lat] (conexp.fca.lattices/standard-context lat)))
 (clojure.core/defn
  conexp-clj-dual_lattice
  ([lat] (conexp.fca.lattices/dual-lattice lat)))
 (clojure.core/defn
  conexp-clj-lattice_one
  ([lat] (conexp.fca.lattices/lattice-one lat)))
 (clojure.core/defn
  conexp-clj-has_lattice_order_p
  ([lat] (conexp.fca.lattices/has-lattice-order? lat)))
 (clojure.core/defn
  conexp-clj-base_set
  ([^Lattice lattice] (conexp.fca.lattices/base-set lattice)))
 (clojure.core/defn
  conexp-clj-directly_neighboured_p
  ([lat x y] (conexp.fca.lattices/directly-neighboured? lat x y)))
 (clojure.core/defn
  conexp-clj-sup
  ([^Lattice lattice] (conexp.fca.lattices/sup lattice)))
 (clojure.core/defn
  conexp-clj-concept_lattice
  ([ctx] (conexp.fca.lattices/concept-lattice ctx)))
 (clojure.core/defn
  conexp-clj-modular_p
  ([lat] (conexp.fca.lattices/modular? lat)))
 (clojure.core/defn
  conexp-clj-lattice_zero
  ([lat] (conexp.fca.lattices/lattice-zero lat)))
 (clojure.core/defn
  conexp-clj-make_lattice_nc
  ([base-set order-function]
   (conexp.fca.lattices/make-lattice-nc base-set order-function))
  ([base-set inf sup]
   (conexp.fca.lattices/make-lattice-nc base-set inf sup))
  ([base-set order-function inf sup]
   (conexp.fca.lattices/make-lattice-nc
    base-set
    order-function
    inf
    sup)))
 (clojure.core/defn
  conexp-clj-lattice_sup_irreducibles
  ([lat] (conexp.fca.lattices/lattice-sup-irreducibles lat)))
 (clojure.core/defn
  conexp-clj-distributive_p
  ([lat] (conexp.fca.lattices/distributive? lat)))
 (clojure.core/defn
  conexp-clj-lattice_doubly_irreducibles
  ([lat] (conexp.fca.lattices/lattice-doubly-irreducibles lat)))
 (clojure.core/defn
  conexp-clj-lattice_coatoms
  ([lat] (conexp.fca.lattices/lattice-coatoms lat)))
 (clojure.core/defn
  conexp-clj-lattice_atoms
  ([lat] (conexp.fca.lattices/lattice-atoms lat)))
 (clojure.core/defn
  conexp-clj-__gt_Lattice
  ([base-set order-function inf sup]
   (conexp.fca.lattices/->Lattice base-set order-function inf sup)))
 (clojure.core/defn
  conexp-clj-make_lattice
  ([^"[Ljava.lang.Object;" args]
   (conexp.fca.lattices/make-lattice args)))
 (clojure.core/defn
  conexp-clj-inf
  ([^Lattice lattice] (conexp.fca.lattices/inf lattice)))
 (clojure.core/defn
  conexp-clj-lattice_lower_neighbours
  ([lat y] (conexp.fca.lattices/lattice-lower-neighbours lat y)))
 (clojure.core/defn
  conexp-clj-lattice_upper_neighbours
  ([lat x] (conexp.fca.lattices/lattice-upper-neighbours lat x)))
 (clojure.core/defn
  conexp-clj-lattice_inf_irreducibles
  ([lat] (conexp.fca.lattices/lattice-inf-irreducibles lat)))
 (clojure.core/defn
  conexp-clj-order
  ([^Lattice lattice] (conexp.fca.lattices/order lattice)))
 (clojure.core/defn
  conexp-clj-bond_p
  ([ctx-1 ctx-2 ctx] (conexp.fca.misc/bond? ctx-1 ctx-2 ctx)))
 (clojure.core/defn
  conexp-clj-compatible_subcontexts
  ([ctx] (conexp.fca.misc/compatible-subcontexts ctx)))
 (clojure.core/defn
  conexp-clj-compatible_subcontext_p
  ([ctx-1 ctx-2] (conexp.fca.misc/compatible-subcontext? ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-all_bonds
  ([ctx-1 ctx-2] (conexp.fca.misc/all-bonds ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-context_from_clop
  ([base-set clop] (conexp.fca.misc/context-from-clop base-set clop)))
 (clojure.core/defn
  conexp-clj-all_bonds_by_shared_intents
  ([ctx-1 ctx-2]
   (conexp.fca.misc/all-bonds-by-shared-intents ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-implication_context
  ([n] (conexp.fca.misc/implication-context n)))
 (clojure.core/defn
  conexp-clj-smallest_bond
  ([ctx-1 ctx-2 rel] (conexp.fca.misc/smallest-bond ctx-1 ctx-2 rel)))
 (clojure.core/defn
  conexp-clj-all_shared_intents
  ([ctx-1 ctx-2] (conexp.fca.misc/all-shared-intents ctx-1 ctx-2)))
 nil
 (clojure.core/defn
  conexp-clj-latex
  ([this] (conexp.io.latex/latex this))
  ([this choice] (conexp.io.latex/latex this choice)))
 (clojure.core/defn
  conexp-clj-write_context
  ([format context file]
   (conexp.io.contexts/write-context format context file))
  ([context file] (conexp.io.contexts/write-context context file)))
 (clojure.core/defn
  conexp-clj-get_default_context_format
  ([] (conexp.io.contexts/get-default-context-format)))
 (clojure.core/defn
  conexp-clj-list_context_output_formats
  ([] (conexp.io.contexts/list-context-output-formats)))
 (clojure.core/defn
  conexp-clj-list_context_input_formats
  ([] (conexp.io.contexts/list-context-input-formats)))
 (clojure.core/defn
  conexp-clj-list_context_formats
  ([] (conexp.io.contexts/list-context-formats)))
 (clojure.core/defn
  conexp-clj-read_context
  ([file] (conexp.io.contexts/read-context file))
  ([file explicit-format]
   (conexp.io.contexts/read-context file explicit-format)))
 (clojure.core/defn
  conexp-clj-set_default_context_format_f
  ([format__4140__auto__]
   (conexp.io.contexts/set-default-context-format!
    format__4140__auto__)))
 (clojure.core/defn
  conexp-clj-list_lattice_output_formats
  ([] (conexp.io.lattices/list-lattice-output-formats)))
 (clojure.core/defn
  conexp-clj-list_lattice_input_formats
  ([] (conexp.io.lattices/list-lattice-input-formats)))
 (clojure.core/defn
  conexp-clj-get_default_lattice_format
  ([] (conexp.io.lattices/get-default-lattice-format)))
 (clojure.core/defn
  conexp-clj-set_default_lattice_format_f
  ([format__4140__auto__]
   (conexp.io.lattices/set-default-lattice-format!
    format__4140__auto__)))
 (clojure.core/defn
  conexp-clj-list_lattice_formats
  ([] (conexp.io.lattices/list-lattice-formats)))
 (clojure.core/defn
  conexp-clj-read_lattice
  ([file] (conexp.io.lattices/read-lattice file))
  ([file explicit-format]
   (conexp.io.lattices/read-lattice file explicit-format)))
 (clojure.core/defn
  conexp-clj-write_lattice
  ([format lattice file]
   (conexp.io.lattices/write-lattice format lattice file))
  ([lattice file] (conexp.io.lattices/write-lattice lattice file)))
 (clojure.core/defn
  conexp-clj-list_layout_output_formats
  ([] (conexp.io.layouts/list-layout-output-formats)))
 (clojure.core/defn
  conexp-clj-read_layout
  ([file] (conexp.io.layouts/read-layout file))
  ([file explicit-format]
   (conexp.io.layouts/read-layout file explicit-format)))
 (clojure.core/defn
  conexp-clj-set_default_layout_format_f
  ([format__4140__auto__]
   (conexp.io.layouts/set-default-layout-format!
    format__4140__auto__)))
 (clojure.core/defn
  conexp-clj-get_default_layout_format
  ([] (conexp.io.layouts/get-default-layout-format)))
 (clojure.core/defn
  conexp-clj-list_layout_formats
  ([] (conexp.io.layouts/list-layout-formats)))
 (clojure.core/defn
  conexp-clj-list_layout_input_formats
  ([] (conexp.io.layouts/list-layout-input-formats)))
 (clojure.core/defn
  conexp-clj-write_layout
  ([format layout file]
   (conexp.io.layouts/write-layout format layout file))
  ([layout file] (conexp.io.layouts/write-layout layout file)))
 (clojure.core/defn
  conexp-clj-list_mv_context_output_formats
  ([] (conexp.io.many-valued-contexts/list-mv-context-output-formats)))
 (clojure.core/defn
  conexp-clj-list_mv_context_formats
  ([] (conexp.io.many-valued-contexts/list-mv-context-formats)))
 (clojure.core/defn
  conexp-clj-get_default_mv_context_format
  ([] (conexp.io.many-valued-contexts/get-default-mv-context-format)))
 (clojure.core/defn
  conexp-clj-list_mv_context_input_formats
  ([] (conexp.io.many-valued-contexts/list-mv-context-input-formats)))
 (clojure.core/defn
  conexp-clj-write_mv_context
  ([format mv-context file]
   (conexp.io.many-valued-contexts/write-mv-context
    format
    mv-context
    file))
  ([mv-context file]
   (conexp.io.many-valued-contexts/write-mv-context mv-context file)))
 (clojure.core/defn
  conexp-clj-set_default_mv_context_format_f
  ([format__4140__auto__]
   (conexp.io.many-valued-contexts/set-default-mv-context-format!
    format__4140__auto__)))
 (clojure.core/defn
  conexp-clj-read_mv_context
  ([file] (conexp.io.many-valued-contexts/read-mv-context file))
  ([file explicit-format]
   (conexp.io.many-valued-contexts/read-mv-context
    file
    explicit-format)))
 (clojure.core/defn
  conexp-clj-write_many_valued_context
  ([format mv-context file]
   (conexp.io.many-valued-contexts/write-many-valued-context
    format
    mv-context
    file))
  ([mv-context file]
   (conexp.io.many-valued-contexts/write-many-valued-context
    mv-context
    file)))
 (clojure.core/defn
  conexp-clj-read_many_valued_context
  ([file]
   (conexp.io.many-valued-contexts/read-many-valued-context file))
  ([file explicit-format]
   (conexp.io.many-valued-contexts/read-many-valued-context
    file
    explicit-format)))
 (clojure.core/defn
  conexp-clj-inf_additive_layout
  ([lattice] (conexp.layouts/inf-additive-layout lattice)))
 nil)
