(do
 (clojure.core/ns
  conexp.contrib.java.Main
  (:require conexp.main)
  (:gen-class
   :prefix
   conexp-clj-
   :methods
   (^{:static true}
    [zip [Object Object] Object]
    ^{:static true}
    [attribute_reduced_p [Object] Object]
    ^{:static true}
    [list_context_input_formats [] Object]
    ^{:static true}
    [transitive_closure [Object] Object]
    ^{:static true}
    [default_handler [Object Object Object] Object]
    ^{:static true}
    [context_object_closure [Object Object] Object]
    ^{:static true}
    [yes_or_no_p [Object] Object]
    ^{:static true}
    [expt [Object Object] Object]
    ^{:static true}
    [sqrt [Object] Object]
    ^{:static true}
    [null_context [Object] Object]
    ^{:static true}
    [list_mv_context_formats [] Object]
    ^{:static true}
    [odprime [Object Object] Object]
    ^{:static true}
    [values_of_object [Object Object] Object]
    ^{:static true}
    [lattice_coatoms [Object] Object]
    ^{:static true}
    [subcontext_p [Object Object] Object]
    ^{:static true}
    [reduce_f [Object Object Object] Object]
    ^{:static true}
    [random_contexts [Object Object] Object]
    ^{:static true}
    [interordinal_scale [Object] Object]
    ^{:static true}
    [interordinal_scale [Object Object Object] Object]
    ^{:static true}
    [interordinal_scale [Object Object Object Object] Object]
    ^{:static true}
    [list_mv_context_output_formats [] Object]
    ^{:static true}
    [order [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [up_down_arrows [Object] Object]
    ^{:static true}
    [conexp_version [] Object]
    ^{:static true}
    [tails [Object] Object]
    ^{:static true}
    [sup [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [base_set [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [bond_p [Object Object Object] Object]
    ^{:static true}
    [sort_by_first [Object Object] Object]
    ^{:static true}
    [get_default_context_format [] Object]
    ^{:static true}
    [topological_sort [Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object Object] Object]
    ^{:static true}
    [quit [] Object]
    ^{:static true}
    [object_clarified_p [Object] Object]
    ^{:static true}
    [clarified_p [Object] Object]
    ^{:static true}
    [proper_subset_p [Object Object] Object]
    ^{:static true}
    [union [] Object]
    ^{:static true}
    [union [Object] Object]
    ^{:static true}
    [union [Object Object] Object]
    ^{:static true}
    [union [Object Object] Object]
    ^{:static true}
    [support [Object Object] Object]
    ^{:static true}
    [minimal_generating_subsets [Object Object] Object]
    ^{:static true}
    [write_context [Object Object Object] Object]
    ^{:static true}
    [write_context [Object Object] Object]
    ^{:static true}
    [all_closed_sets [Object Object] Object]
    ^{:static true}
    [all_closed_sets [Object Object Object] Object]
    ^{:static true}
    [select [Object Object] Object]
    ^{:static true}
    [partial_max [Object Object] Object]
    ^{:static true}
    [ordinal_scale [Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object Object] Object]
    ^{:static true}
    [minimal_implication_set_p [Object] Object]
    ^{:static true}
    [context_composition [Object Object] Object]
    ^{:static true}
    [distinct_by_key [Object Object] Object]
    ^{:static true}
    [split_at_last [Object Object] Object]
    ^{:static true}
    [oprime [Object Object] Object]
    ^{:static true}
    [cross_product [Object Object] Object]
    ^{:static true}
    [nominal_scale [Object] Object]
    ^{:static true}
    [nominal_scale [Object Object] Object]
    ^{:static true}
    [directly_neighboured_p [Object Object Object] Object]
    ^{:static true}
    [__gt_Lattice [Object Object Object Object] Object]
    ^{:static true}
    [dual_lattice [Object] Object]
    ^{:static true}
    [read_layout [Object] Object]
    ^{:static true}
    [read_layout [Object Object] Object]
    ^{:static true}
    [project [Object Object] Object]
    ^{:static true}
    [map_invert [Object] Object]
    ^{:static true}
    [context_intersection [Object Object] Object]
    ^{:static true}
    [abs [Object] Object]
    ^{:static true}
    [all_bonds [Object Object] Object]
    ^{:static true}
    [list_layout_output_formats [] Object]
    ^{:static true}
    [attribute_clarified_p [Object] Object]
    ^{:static true}
    [implication_p [Object] Object]
    ^{:static true}
    [make_mv_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [follows_semantically_p [Object Object] Object]
    ^{:static true}
    [compatible_subcontext_p [Object Object] Object]
    ^{:static true}
    [exact_integer_sqrt [Object] Object]
    ^{:static true}
    [list_context_formats [] Object]
    ^{:static true}
    [make_implication [Object Object] Object]
    ^{:static true}
    [join [Object Object] Object]
    ^{:static true}
    [join [Object Object Object] Object]
    ^{:static true}
    [standard_context [Object] Object]
    ^{:static true}
    [up_arrows [Object] Object]
    ^{:static true}
    [reflexive_transitive_closure [Object Object] Object]
    ^{:static true}
    [set_default_context_format_f [Object] Object]
    ^{:static true}
    [partial_min [Object Object] Object]
    ^{:static true}
    [objects [Object] Object]
    ^{:static true}
    [scale_mv_context [Object Object] Object]
    ^{:static true}
    [scale_mv_context [Object Object Object] Object]
    ^{:static true}
    [set_default_mv_context_format_f [Object] Object]
    ^{:static true}
    [incident_p [Object Object Object] Object]
    ^{:static true}
    [split_at_first [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object Object] Object]
    ^{:static true}
    [object_reduced_p [Object] Object]
    ^{:static true}
    [reduced_p [Object] Object]
    ^{:static true}
    [make_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [test_conexp [] Object]
    ^{:static true}
    [test_conexp [Object] Object]
    ^{:static true}
    [list_mv_context_input_formats [] Object]
    ^{:static true}
    [biordinal_scale [Object Object] Object]
    ^{:static true}
    [biordinal_scale [Object Object Object Object Object] Object]
    ^{:static true}
    [mv_context_to_string [Object] Object]
    ^{:static true}
    [mv_context_to_string [Object Object Object] Object]
    ^{:static true}
    [context_size [Object] Object]
    ^{:static true}
    [has_lattice_order_p [Object] Object]
    ^{:static true}
    [lattice_zero [Object] Object]
    ^{:static true}
    [proper_premises [Object] Object]
    ^{:static true}
    [sort_by_second [Object Object] Object]
    ^{:static true}
    [__gt_Formal_Context [Object Object Object] Object]
    ^{:static true}
    [lattice_doubly_irreducibles [Object] Object]
    ^{:static true}
    [invert_context [Object] Object]
    ^{:static true}
    [modular_p [Object] Object]
    ^{:static true}
    [subsets [Object] Object]
    ^{:static true}
    [premise [Object] Object]
    ^{:static true}
    [div [Object Object] Object]
    ^{:static true}
    [lectic__lt__i [Object Object Object Object] Object]
    ^{:static true}
    [down_arrows [Object] Object]
    ^{:static true}
    [lattice_lower_neighbours [Object Object] Object]
    ^{:static true}
    [unsupported_operation [Object Object] Object]
    ^{:static true}
    [first_non_nil [Object] Object]
    ^{:static true}
    [lattice_one [Object] Object]
    ^{:static true}
    [with_str_out [Object Object] Object]
    ^{:static true}
    [incidence [Object] Object]
    ^{:static true}
    [make_handler [Object Object] Object]
    ^{:static true}
    [lattice_atoms [Object] Object]
    ^{:static true}
    [proper_premise_p [Object Object] Object]
    ^{:static true}
    [ask [Object Object] Object]
    ^{:static true}
    [make_context [Object Object Object] Object]
    ^{:static true}
    [list_layout_input_formats [] Object]
    ^{:static true}
    [write_mv_context [Object Object Object] Object]
    ^{:static true}
    [write_mv_context [Object Object] Object]
    ^{:static true}
    [rename_objects [Object Object] Object]
    ^{:static true}
    [subset_p [Object Object] java.lang.Boolean]
    ^{:static true}
    [compatible_subcontexts [Object] Object]
    ^{:static true}
    [ensure_seq [Object] Object]
    ^{:static true}
    [rename_attributes [Object Object] Object]
    ^{:static true}
    [sound_implication_set_p [Object Object] Object]
    ^{:static true}
    [latex [Object] Object]
    ^{:static true}
    [latex [Object Object] Object]
    ^{:static true}
    [context_to_string [Object] java.lang.String]
    ^{:static true}
    [context_to_string [Object Object Object] java.lang.String]
    ^{:static true}
    [attribute_concept [Object Object] Object]
    ^{:static true}
    [set_of_range [Object] Object]
    ^{:static true}
    [set_of_range [Object Object] Object]
    ^{:static true}
    [set_of_range [Object Object Object] Object]
    ^{:static true}
    [_lt__eq__gt_ [Object Object] Object]
    ^{:static true}
    [context_product [Object Object] Object]
    ^{:static true}
    [concept_lattice [Object] Object]
    ^{:static true}
    [distributive_p [Object] Object]
    ^{:static true}
    [dichotomic_scale [Object] Object]
    ^{:static true}
    [clop_by_implications [Object] Object]
    ^{:static true}
    [inf [conexp.fca.lattices.Lattice] Object]
    ^{:static true}
    [get_default_mv_context_format [] Object]
    ^{:static true}
    [make_context_nc [Object Object Object] Object]
    ^{:static true}
    [floor [Object] Object]
    ^{:static true}
    [improve_basic_order [Object Object] Object]
    ^{:static true}
    [concepts [Object] Object]
    ^{:static true}
    [disjoint_union [Object Object] Object]
    ^{:static true}
    [make_mv_context [Object Object Object] Object]
    ^{:static true}
    [read_lattice [Object] Object]
    ^{:static true}
    [read_lattice [Object Object] Object]
    ^{:static true}
    [confidence [Object Object] Object]
    ^{:static true}
    [write_layout [Object Object Object] Object]
    ^{:static true}
    [write_layout [Object Object] Object]
    ^{:static true}
    [clarify_objects [Object] Object]
    ^{:static true}
    [make_mv_context_nc [Object Object Object] Object]
    ^{:static true}
    [map_by_fn [Object Object] Object]
    ^{:static true}
    [object_derivation [Object Object] Object]
    ^{:static true}
    [context_p [Object] Object]
    ^{:static true}
    [intersection [Object] Object]
    ^{:static true}
    [intersection [Object Object] Object]
    ^{:static true}
    [intersection [Object Object] Object]
    ^{:static true}
    [round [Object] Object]
    ^{:static true}
    [inits [Object] Object]
    ^{:static true}
    [stem_base_from_base [Object] Object]
    ^{:static true}
    [smallest_bond [Object Object Object] Object]
    ^{:static true}
    [extents [Object] Object]
    ^{:static true}
    [complete_implication_set_p [Object Object] Object]
    ^{:static true}
    [clojure_type [Object] Object]
    ^{:static true}
    [gcd [Object Object] Object]
    ^{:static true}
    [read_mv_context [Object] Object]
    ^{:static true}
    [read_mv_context [Object Object] Object]
    ^{:static true}
    [illegal_state [Object Object] Object]
    ^{:static true}
    [list_lattice_output_formats [] Object]
    ^{:static true}
    [context_attribute_closure [Object Object] Object]
    ^{:static true}
    [list_layout_formats [] Object]
    ^{:static true}
    [name_with_attributes [Object Object] Object]
    ^{:static true}
    [next_closed_set [Object Object Object] Object]
    ^{:static true}
    [available_formats [Object] Object]
    ^{:static true}
    [frequent_closed_itemsets [Object Object] Object]
    ^{:static true}
    [context_transitive_closure [Object] Object]
    ^{:static true}
    [singleton_p [Object] Object]
    ^{:static true}
    [values_of_attribute [Object Object] Object]
    ^{:static true}
    [adprime [Object Object] Object]
    ^{:static true}
    [adiag_context [Object] Object]
    ^{:static true}
    [illegal_argument [Object Object] Object]
    ^{:static true}
    [all_shared_intents [Object Object] Object]
    ^{:static true}
    [diag_context [Object] Object]
    ^{:static true}
    [context_semiproduct [Object Object] Object]
    ^{:static true}
    [list_lattice_input_formats [] Object]
    ^{:static true}
    [next_closed_set_in_family [Object Object Object Object] Object]
    ^{:static true}
    [context_xia_product [Object Object] Object]
    ^{:static true}
    [difference [Object] Object]
    ^{:static true}
    [difference [Object Object] Object]
    ^{:static true}
    [difference [Object Object] Object]
    ^{:static true}
    [get_default_lattice_format [] Object]
    ^{:static true}
    [pseudo_close_under_implications [Object Object] Object]
    ^{:static true}
    [rename [Object Object] Object]
    ^{:static true}
    [random_context [Object Object] Object]
    ^{:static true}
    [random_context [Object Object Object] Object]
    ^{:static true}
    [aprime [Object Object] Object]
    ^{:static true}
    [transitive_reduction [Object] Object]
    ^{:static true}
    [transitive_reduction [Object Object] Object]
    ^{:static true}
    [lectic__lt_ [Object Object Object] Object]
    ^{:static true}
    [concept_p [Object Object] Object]
    ^{:static true}
    [equivalent_implications_p [Object Object] Object]
    ^{:static true}
    [context_subposition [Object Object] Object]
    ^{:static true}
    [has_version_p [Object] Object]
    ^{:static true}
    [not_yet_implemented [] Object]
    ^{:static true}
    [all_closed_sets_in_family [Object Object Object] Object]
    ^{:static true}
    [all_closed_sets_in_family [Object Object Object Object] Object]
    ^{:static true}
    [restrict_concept [Object Object] Object]
    ^{:static true}
    [context_union [Object Object] Object]
    ^{:static true}
    [all_bonds_by_shared_intents [Object Object] Object]
    ^{:static true}
    [make_lattice [Object Object] Object]
    ^{:static true}
    [list_lattice_formats [] Object]
    ^{:static true}
    [one_context [Object] Object]
    ^{:static true}
    [write_lattice [Object Object Object] Object]
    ^{:static true}
    [write_lattice [Object Object] Object]
    ^{:static true}
    [dissoc_in [Object Object] Object]
    ^{:static true}
    [reduce_context [Object] Object]
    ^{:static true}
    [dual_context [Object] Object]
    ^{:static true}
    [lcm [Object Object] Object]
    ^{:static true}
    [pseudo_intents [Object] Object]
    ^{:static true}
    [luxenburger_basis [Object Object Object] Object]
    ^{:static true}
    [now [] Object]
    ^{:static true}
    [implication_context [Object] Object]
    ^{:static true}
    [pseudo_clop_by_implications [Object] Object]
    ^{:static true}
    [attributes [Object] Object]
    ^{:static true}
    [seqable_p [Object] Object]
    ^{:static true}
    [minimum_set_covers [Object Object] Object]
    ^{:static true}
    [intents [Object] Object]
    ^{:static true}
    [graph_of_function_p [Object Object Object] Object]
    ^{:static true}
    [clarify_attributes [Object] Object]
    ^{:static true}
    [new_by_name [Object Object] Object]
    ^{:static true}
    [explore_attributes [Object Object] Object]
    ^{:static true}
    [proper_conclusion [Object Object] Object]
    ^{:static true}
    [__gt_Implication [Object Object] Object]
    ^{:static true}
    [list_context_output_formats [] Object]
    ^{:static true}
    [object_concept [Object Object] Object]
    ^{:static true}
    [context_from_clop [Object Object] Object]
    ^{:static true}
    [context_apposition [Object Object] Object]
    ^{:static true}
    [rename_keys [Object Object] Object]
    ^{:static true}
    [set_default_lattice_format_f [Object] Object]
    ^{:static true}
    [proper_premise_implications [Object] Object]
    ^{:static true}
    [reduce_objects [Object] Object]
    ^{:static true}
    [lattice_upper_neighbours [Object Object] Object]
    ^{:static true}
    [warn [Object] Object]
    ^{:static true}
    [context_disjoint_union [Object Object] Object]
    ^{:static true}
    [__gt_Many_Valued_Context [Object Object Object] Object]
    ^{:static true}
    [context_sum [Object Object] Object]
    ^{:static true}
    [read_context [Object] Object]
    ^{:static true}
    [read_context [Object Object] Object]
    ^{:static true}
    [close_under_implications [Object Object] Object]
    ^{:static true}
    [attribute_derivation [Object Object] Object]
    ^{:static true}
    [reduce_attributes [Object] Object]
    ^{:static true}
    [immigrate [Object Object] Object]
    ^{:static true}
    [stem_base [Object] Object]
    ^{:static true}
    [stem_base [Object Object] Object]
    ^{:static true}
    [proper_superset_p [Object Object] Object]
    ^{:static true}
    [print_context [Object Object] Object]
    ^{:static true}
    [ensure_length [Object Object] Object]
    ^{:static true}
    [ensure_length [Object Object Object] Object]
    ^{:static true}
    [respects_p [Object Object] Object]
    ^{:static true}
    [to_set [Object] Object]
    ^{:static true}
    [direct_upper_concepts [Object Object] Object]
    ^{:static true}
    [conclusion [Object] Object]
    ^{:static true}
    [ceil [Object] Object]
    ^{:static true}
    [lattice_inf_irreducibles [Object] Object]
    ^{:static true}
    [index [Object Object] Object]
    ^{:static true}
    [clarify_context [Object] Object]
    ^{:static true}
    [get_default_layout_format [] Object]
    ^{:static true}
    [direct_lower_concepts [Object Object] Object]
    ^{:static true}
    [superset_p [Object Object] java.lang.Boolean]
    ^{:static true}
    [set_default_layout_format_f [Object] Object]
    ^{:static true}
    [inf_additive_layout [Object] Object]
    ^{:static true}
    [holds_p [Object Object] Object]
    ^{:static true}
    [lattice_sup_irreducibles [Object] Object]
    ^{:static true}
    [hash_combine_hash [Object Object] Object])))
 (clojure.core/import 'conexp.fca.contexts.Context)
 (clojure.core/import 'conexp.fca.lattices.Lattice)
 (clojure.core/defn
  conexp-clj-zip
  ([seq-1 seq-2] (clojure.core/apply conexp.main/zip [seq-1 seq-2])))
 (clojure.core/defn
  conexp-clj-attribute_reduced_p
  ([ctx] (clojure.core/apply conexp.main/attribute-reduced? [ctx])))
 (clojure.core/defn
  conexp-clj-list_context_input_formats
  ([] (clojure.core/apply conexp.main/list-context-input-formats [])))
 (clojure.core/defn
  conexp-clj-transitive_closure
  ([pairs]
   (clojure.core/apply conexp.main/transitive-closure [pairs])))
 (clojure.core/defn
  conexp-clj-default_handler
  ([ctx known impl]
   (clojure.core/apply conexp.main/default-handler [ctx known impl])))
 (clojure.core/defn
  conexp-clj-context_object_closure
  ([ctx set-of-objects]
   (clojure.core/apply
    conexp.main/context-object-closure
    [ctx set-of-objects])))
 (clojure.core/defn
  conexp-clj-yes_or_no_p
  ([question] (clojure.core/apply conexp.main/yes-or-no? [question])))
 (clojure.core/defn
  conexp-clj-expt
  ([a b] (clojure.core/apply conexp.main/expt [a b])))
 (clojure.core/defn
  conexp-clj-sqrt
  ([n] (clojure.core/apply conexp.main/sqrt [n])))
 (clojure.core/defn
  conexp-clj-null_context
  ([base-set]
   (clojure.core/apply conexp.main/null-context [base-set])))
 (clojure.core/defn
  conexp-clj-list_mv_context_formats
  ([] (clojure.core/apply conexp.main/list-mv-context-formats [])))
 (clojure.core/defn
  conexp-clj-odprime
  ([ctx set-of-objects]
   (clojure.core/apply conexp.main/odprime [ctx set-of-objects])))
 (clojure.core/defn
  conexp-clj-values_of_object
  ([mv-ctx g]
   (clojure.core/apply conexp.main/values-of-object [mv-ctx g])))
 (clojure.core/defn
  conexp-clj-lattice_coatoms
  ([lat] (clojure.core/apply conexp.main/lattice-coatoms [lat])))
 (clojure.core/defn
  conexp-clj-subcontext_p
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/subcontext? [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-reduce_f
  ([fn initial-value coll]
   (clojure.core/apply conexp.main/reduce! [fn initial-value coll])))
 (clojure.core/defn
  conexp-clj-random_contexts
  ([number upper-limit]
   (clojure.core/apply
    conexp.main/random-contexts
    [number upper-limit])))
 (clojure.core/defn
  conexp-clj-interordinal_scale
  ([values]
   (clojure.core/apply conexp.main/interordinal-scale [values]))
  ([values <= >=]
   (clojure.core/apply conexp.main/interordinal-scale [values <= >=]))
  ([values others <= >=]
   (clojure.core/apply
    conexp.main/interordinal-scale
    [values others <= >=])))
 (clojure.core/defn
  conexp-clj-list_mv_context_output_formats
  ([]
   (clojure.core/apply conexp.main/list-mv-context-output-formats [])))
 (clojure.core/defn
  conexp-clj-order
  ([^Lattice lattice]
   (clojure.core/apply conexp.main/order [^Lattice lattice])))
 (clojure.core/defn
  conexp-clj-up_down_arrows
  ([ctx] (clojure.core/apply conexp.main/up-down-arrows [ctx])))
 (clojure.core/defn
  conexp-clj-conexp_version
  ([] (clojure.core/apply conexp.main/conexp-version [])))
 (clojure.core/defn
  conexp-clj-tails
  ([sqn] (clojure.core/apply conexp.main/tails [sqn])))
 (clojure.core/defn
  conexp-clj-sup
  ([^Lattice lattice]
   (clojure.core/apply conexp.main/sup [^Lattice lattice])))
 (clojure.core/defn
  conexp-clj-base_set
  ([^Lattice lattice]
   (clojure.core/apply conexp.main/base-set [^Lattice lattice])))
 (clojure.core/defn
  conexp-clj-bond_p
  ([ctx-1 ctx-2 ctx]
   (clojure.core/apply conexp.main/bond? [ctx-1 ctx-2 ctx])))
 (clojure.core/defn
  conexp-clj-sort_by_first
  ([x y] (clojure.core/apply conexp.main/sort-by-first [x y])))
 (clojure.core/defn
  conexp-clj-get_default_context_format
  ([] (clojure.core/apply conexp.main/get-default-context-format [])))
 (clojure.core/defn
  conexp-clj-topological_sort
  ([comp coll]
   (clojure.core/apply conexp.main/topological-sort [comp coll])))
 (clojure.core/defn
  conexp-clj-make_lattice_nc
  ([base-set order-function]
   (clojure.core/apply
    conexp.main/make-lattice-nc
    [base-set order-function]))
  ([base-set inf sup]
   (clojure.core/apply conexp.main/make-lattice-nc [base-set inf sup]))
  ([base-set order-function inf sup]
   (clojure.core/apply
    conexp.main/make-lattice-nc
    [base-set order-function inf sup])))
 (clojure.core/defn
  conexp-clj-quit
  ([] (clojure.core/apply conexp.main/quit [])))
 (clojure.core/defn
  conexp-clj-object_clarified_p
  ([ctx] (clojure.core/apply conexp.main/object-clarified? [ctx])))
 (clojure.core/defn
  conexp-clj-clarified_p
  ([ctx] (clojure.core/apply conexp.main/clarified? [ctx])))
 (clojure.core/defn
  conexp-clj-proper_subset_p
  ([set-1 set-2]
   (clojure.core/apply conexp.main/proper-subset? [set-1 set-2])))
 (clojure.core/defn
  conexp-clj-union
  ([] (clojure.core/apply conexp.main/union []))
  ([s1] (clojure.core/apply conexp.main/union [s1]))
  ([s1 s2] (clojure.core/apply conexp.main/union [s1 s2]))
  ([s1 s2 & sets]
   (clojure.core/apply
    conexp.main/union
    (clojure.core/vec (clojure.core/list* s1 s2 sets)))))
 (clojure.core/defn
  conexp-clj-support
  ([thing ctx] (clojure.core/apply conexp.main/support [thing ctx])))
 (clojure.core/defn
  conexp-clj-minimal_generating_subsets
  ([clop A]
   (clojure.core/apply
    conexp.main/minimal-generating-subsets
    [clop A])))
 (clojure.core/defn
  conexp-clj-write_context
  ([format context file]
   (clojure.core/apply
    conexp.main/write-context
    [format context file]))
  ([context file]
   (clojure.core/apply conexp.main/write-context [context file])))
 (clojure.core/defn
  conexp-clj-all_closed_sets
  ([base clop]
   (clojure.core/apply conexp.main/all-closed-sets [base clop]))
  ([base clop initial]
   (clojure.core/apply
    conexp.main/all-closed-sets
    [base clop initial])))
 (clojure.core/defn
  conexp-clj-select
  ([pred xset] (clojure.core/apply conexp.main/select [pred xset])))
 (clojure.core/defn
  conexp-clj-partial_max
  ([<= xs] (clojure.core/apply conexp.main/partial-max [<= xs])))
 (clojure.core/defn
  conexp-clj-ordinal_scale
  ([values] (clojure.core/apply conexp.main/ordinal-scale [values]))
  ([values <=]
   (clojure.core/apply conexp.main/ordinal-scale [values <=]))
  ([values others <=]
   (clojure.core/apply conexp.main/ordinal-scale [values others <=])))
 (clojure.core/defn
  conexp-clj-minimal_implication_set_p
  ([impl-set]
   (clojure.core/apply
    conexp.main/minimal-implication-set?
    [impl-set])))
 (clojure.core/defn
  conexp-clj-context_composition
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-composition [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-distinct_by_key
  ([sequence key]
   (clojure.core/apply conexp.main/distinct-by-key [sequence key])))
 (clojure.core/defn
  conexp-clj-split_at_last
  ([predicate sequence]
   (clojure.core/apply
    conexp.main/split-at-last
    [predicate sequence])))
 (clojure.core/defn
  conexp-clj-oprime
  ([ctx objects]
   (clojure.core/apply conexp.main/oprime [ctx objects])))
 (clojure.core/defn
  conexp-clj-cross_product
  ([& sets]
   (clojure.core/apply
    conexp.main/cross-product
    (clojure.core/vec (clojure.core/list* sets)))))
 (clojure.core/defn
  conexp-clj-nominal_scale
  ([values] (clojure.core/apply conexp.main/nominal-scale [values]))
  ([values others]
   (clojure.core/apply conexp.main/nominal-scale [values others])))
 (clojure.core/defn
  conexp-clj-directly_neighboured_p
  ([lat x y]
   (clojure.core/apply conexp.main/directly-neighboured? [lat x y])))
 (clojure.core/defn
  conexp-clj-__gt_Lattice
  ([base-set order-function inf sup]
   (clojure.core/apply
    conexp.main/->Lattice
    [base-set order-function inf sup])))
 (clojure.core/defn
  conexp-clj-dual_lattice
  ([lat] (clojure.core/apply conexp.main/dual-lattice [lat])))
 (clojure.core/defn
  conexp-clj-read_layout
  ([file] (clojure.core/apply conexp.main/read-layout [file]))
  ([file explicit-format]
   (clojure.core/apply
    conexp.main/read-layout
    [file explicit-format])))
 (clojure.core/defn
  conexp-clj-project
  ([xrel ks] (clojure.core/apply conexp.main/project [xrel ks])))
 (clojure.core/defn
  conexp-clj-map_invert
  ([m] (clojure.core/apply conexp.main/map-invert [m])))
 (clojure.core/defn
  conexp-clj-context_intersection
  ([ctx1 ctx2]
   (clojure.core/apply conexp.main/context-intersection [ctx1 ctx2])))
 (clojure.core/defn
  conexp-clj-abs
  ([n] (clojure.core/apply conexp.main/abs [n])))
 (clojure.core/defn
  conexp-clj-all_bonds
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/all-bonds [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-list_layout_output_formats
  ([] (clojure.core/apply conexp.main/list-layout-output-formats [])))
 (clojure.core/defn
  conexp-clj-attribute_clarified_p
  ([ctx] (clojure.core/apply conexp.main/attribute-clarified? [ctx])))
 (clojure.core/defn
  conexp-clj-implication_p
  ([thing] (clojure.core/apply conexp.main/implication? [thing])))
 (clojure.core/defn
  conexp-clj-make_mv_context_from_matrix
  ([objects attributes values]
   (clojure.core/apply
    conexp.main/make-mv-context-from-matrix
    [objects attributes values])))
 (clojure.core/defn
  conexp-clj-follows_semantically_p
  ([implication implications]
   (clojure.core/apply
    conexp.main/follows-semantically?
    [implication implications])))
 (clojure.core/defn
  conexp-clj-compatible_subcontext_p
  ([ctx-1 ctx-2]
   (clojure.core/apply
    conexp.main/compatible-subcontext?
    [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-exact_integer_sqrt
  ([n] (clojure.core/apply conexp.main/exact-integer-sqrt [n])))
 (clojure.core/defn
  conexp-clj-list_context_formats
  ([] (clojure.core/apply conexp.main/list-context-formats [])))
 (clojure.core/defn
  conexp-clj-make_implication
  ([premise conclusion]
   (clojure.core/apply
    conexp.main/make-implication
    [premise conclusion])))
 (clojure.core/defn
  conexp-clj-join
  ([xrel yrel] (clojure.core/apply conexp.main/join [xrel yrel]))
  ([xrel yrel km]
   (clojure.core/apply conexp.main/join [xrel yrel km])))
 (clojure.core/defn
  conexp-clj-standard_context
  ([lat] (clojure.core/apply conexp.main/standard-context [lat])))
 (clojure.core/defn
  conexp-clj-up_arrows
  ([ctx] (clojure.core/apply conexp.main/up-arrows [ctx])))
 (clojure.core/defn
  conexp-clj-reflexive_transitive_closure
  ([base-set pairs]
   (clojure.core/apply
    conexp.main/reflexive-transitive-closure
    [base-set pairs])))
 (clojure.core/defn
  conexp-clj-set_default_context_format_f
  ([format__3361__auto__]
   (clojure.core/apply
    conexp.main/set-default-context-format!
    [format__3361__auto__])))
 (clojure.core/defn
  conexp-clj-partial_min
  ([<= xs] (clojure.core/apply conexp.main/partial-min [<= xs])))
 (clojure.core/defn
  conexp-clj-objects
  ([ctx] (clojure.core/apply conexp.main/objects [ctx])))
 (clojure.core/defn
  conexp-clj-scale_mv_context
  ([mv-ctx scales]
   (clojure.core/apply conexp.main/scale-mv-context [mv-ctx scales]))
  ([mv-ctx scales default]
   (clojure.core/apply
    conexp.main/scale-mv-context
    [mv-ctx scales default])))
 (clojure.core/defn
  conexp-clj-set_default_mv_context_format_f
  ([format__3361__auto__]
   (clojure.core/apply
    conexp.main/set-default-mv-context-format!
    [format__3361__auto__])))
 (clojure.core/defn
  conexp-clj-incident_p
  ([ctx g m] (clojure.core/apply conexp.main/incident? [ctx g m])))
 (clojure.core/defn
  conexp-clj-split_at_first
  ([predicate sequence]
   (clojure.core/apply
    conexp.main/split-at-first
    [predicate sequence])))
 (clojure.core/defn
  conexp-clj-rand_context
  ([base-set fill-rate]
   (clojure.core/apply conexp.main/rand-context [base-set fill-rate]))
  ([objects attributes fill-rate]
   (clojure.core/apply
    conexp.main/rand-context
    [objects attributes fill-rate])))
 (clojure.core/defn
  conexp-clj-object_reduced_p
  ([ctx] (clojure.core/apply conexp.main/object-reduced? [ctx])))
 (clojure.core/defn
  conexp-clj-reduced_p
  ([ctx] (clojure.core/apply conexp.main/reduced? [ctx])))
 (clojure.core/defn
  conexp-clj-make_context_from_matrix
  ([G M bits]
   (clojure.core/apply
    conexp.main/make-context-from-matrix
    [G M bits])))
 (clojure.core/defn
  conexp-clj-test_conexp
  ([] (clojure.core/apply conexp.main/test-conexp []))
  ([with-contrib?]
   (clojure.core/apply conexp.main/test-conexp [with-contrib?])))
 (clojure.core/defn
  conexp-clj-list_mv_context_input_formats
  ([]
   (clojure.core/apply conexp.main/list-mv-context-input-formats [])))
 (clojure.core/defn
  conexp-clj-biordinal_scale
  ([values n]
   (clojure.core/apply conexp.main/biordinal-scale [values n]))
  ([values others n <= >=]
   (clojure.core/apply
    conexp.main/biordinal-scale
    [values others n <= >=])))
 (clojure.core/defn
  conexp-clj-mv_context_to_string
  ([mv-ctx]
   (clojure.core/apply conexp.main/mv-context-to-string [mv-ctx]))
  ([mv-ctx order-on-objects order-on-attributes]
   (clojure.core/apply
    conexp.main/mv-context-to-string
    [mv-ctx order-on-objects order-on-attributes])))
 (clojure.core/defn
  conexp-clj-context_size
  ([ctx] (clojure.core/apply conexp.main/context-size [ctx])))
 (clojure.core/defn
  conexp-clj-has_lattice_order_p
  ([lat] (clojure.core/apply conexp.main/has-lattice-order? [lat])))
 (clojure.core/defn
  conexp-clj-lattice_zero
  ([lat] (clojure.core/apply conexp.main/lattice-zero [lat])))
 (clojure.core/defn
  conexp-clj-proper_premises
  ([ctx] (clojure.core/apply conexp.main/proper-premises [ctx])))
 (clojure.core/defn
  conexp-clj-sort_by_second
  ([x y] (clojure.core/apply conexp.main/sort-by-second [x y])))
 (clojure.core/defn
  conexp-clj-__gt_Formal_Context
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/->Formal-Context
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-lattice_doubly_irreducibles
  ([lat]
   (clojure.core/apply conexp.main/lattice-doubly-irreducibles [lat])))
 nil
 (clojure.core/defn
  conexp-clj-invert_context
  ([ctx] (clojure.core/apply conexp.main/invert-context [ctx])))
 (clojure.core/defn
  conexp-clj-modular_p
  ([lat] (clojure.core/apply conexp.main/modular? [lat])))
 (clojure.core/defn
  conexp-clj-subsets
  ([set] (clojure.core/apply conexp.main/subsets [set])))
 (clojure.core/defn
  conexp-clj-premise
  ([thing] (clojure.core/apply conexp.main/premise [thing])))
 (clojure.core/defn
  conexp-clj-div
  ([a b] (clojure.core/apply conexp.main/div [a b])))
 (clojure.core/defn
  conexp-clj-lectic__lt__i
  ([base i A B]
   (clojure.core/apply conexp.main/lectic-<_i [base i A B])))
 (clojure.core/defn
  conexp-clj-down_arrows
  ([ctx] (clojure.core/apply conexp.main/down-arrows [ctx])))
 (clojure.core/defn
  conexp-clj-lattice_lower_neighbours
  ([lat y]
   (clojure.core/apply conexp.main/lattice-lower-neighbours [lat y])))
 (clojure.core/defn
  conexp-clj-unsupported_operation
  ([& strings]
   (clojure.core/apply
    conexp.main/unsupported-operation
    (clojure.core/vec (clojure.core/list* strings)))))
 (clojure.core/defn
  conexp-clj-first_non_nil
  ([seq] (clojure.core/apply conexp.main/first-non-nil [seq])))
 (clojure.core/defn
  conexp-clj-lattice_one
  ([lat] (clojure.core/apply conexp.main/lattice-one [lat])))
 (clojure.core/defn
  conexp-clj-with_str_out
  ([& body]
   (clojure.core/apply
    conexp.main/with-str-out
    (clojure.core/vec (clojure.core/list* body)))))
 (clojure.core/defn
  conexp-clj-incidence
  ([ctx] (clojure.core/apply conexp.main/incidence [ctx])))
 (clojure.core/defn
  conexp-clj-make_handler
  ([& options__201__auto__]
   (clojure.core/apply
    conexp.main/make-handler
    (clojure.core/vec (clojure.core/list* options__201__auto__)))))
 (clojure.core/defn
  conexp-clj-lattice_atoms
  ([lat] (clojure.core/apply conexp.main/lattice-atoms [lat])))
 (clojure.core/defn
  conexp-clj-proper_premise_p
  ([ctx A] (clojure.core/apply conexp.main/proper-premise? [ctx A])))
 (clojure.core/defn
  conexp-clj-ask
  ([prompt read & preds-and-fail-messages]
   (clojure.core/apply
    conexp.main/ask
    (clojure.core/vec
     (clojure.core/list* prompt read preds-and-fail-messages)))))
 (clojure.core/defn
  conexp-clj-make_context
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/make-context
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-list_layout_input_formats
  ([] (clojure.core/apply conexp.main/list-layout-input-formats [])))
 (clojure.core/defn
  conexp-clj-write_mv_context
  ([format mv-context file]
   (clojure.core/apply
    conexp.main/write-mv-context
    [format mv-context file]))
  ([mv-context file]
   (clojure.core/apply
    conexp.main/write-mv-context
    [mv-context file])))
 (clojure.core/defn
  conexp-clj-rename_objects
  ([ctx old-to-new]
   (clojure.core/apply conexp.main/rename-objects [ctx old-to-new])))
 (clojure.core/defn
  conexp-clj-subset_p
  ([set1 set2] (clojure.core/apply conexp.main/subset? [set1 set2])))
 (clojure.core/defn
  conexp-clj-compatible_subcontexts
  ([ctx]
   (clojure.core/apply conexp.main/compatible-subcontexts [ctx])))
 (clojure.core/defn
  conexp-clj-ensure_seq
  ([x] (clojure.core/apply conexp.main/ensure-seq [x])))
 (clojure.core/defn
  conexp-clj-rename_attributes
  ([ctx old-to-new]
   (clojure.core/apply
    conexp.main/rename-attributes
    [ctx old-to-new])))
 (clojure.core/defn
  conexp-clj-sound_implication_set_p
  ([ctx impl-set]
   (clojure.core/apply
    conexp.main/sound-implication-set?
    [ctx impl-set])))
 (clojure.core/defn
  conexp-clj-latex
  ([this] (clojure.core/apply conexp.main/latex [this]))
  ([this choice] (clojure.core/apply conexp.main/latex [this choice])))
 (clojure.core/defn
  conexp-clj-context_to_string
  ([ctx] (clojure.core/apply conexp.main/context-to-string [ctx]))
  ([ctx order-on-objects order-on-attributes]
   (clojure.core/apply
    conexp.main/context-to-string
    [ctx order-on-objects order-on-attributes])))
 (clojure.core/defn
  conexp-clj-attribute_concept
  ([ctx m] (clojure.core/apply conexp.main/attribute-concept [ctx m])))
 (clojure.core/defn
  conexp-clj-set_of_range
  ([end] (clojure.core/apply conexp.main/set-of-range [end]))
  ([start end]
   (clojure.core/apply conexp.main/set-of-range [start end]))
  ([start end step]
   (clojure.core/apply conexp.main/set-of-range [start end step])))
 (clojure.core/defn
  conexp-clj-_lt__eq__gt_
  ([a b] (clojure.core/apply conexp.main/<=> [a b])))
 (clojure.core/defn
  conexp-clj-context_product
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-product [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-concept_lattice
  ([ctx] (clojure.core/apply conexp.main/concept-lattice [ctx])))
 (clojure.core/defn
  conexp-clj-distributive_p
  ([lat] (clojure.core/apply conexp.main/distributive? [lat])))
 (clojure.core/defn
  conexp-clj-dichotomic_scale
  ([values]
   (clojure.core/apply conexp.main/dichotomic-scale [values])))
 (clojure.core/defn
  conexp-clj-clop_by_implications
  ([implications]
   (clojure.core/apply
    conexp.main/clop-by-implications
    [implications])))
 (clojure.core/defn
  conexp-clj-inf
  ([^Lattice lattice]
   (clojure.core/apply conexp.main/inf [^Lattice lattice])))
 (clojure.core/defn
  conexp-clj-get_default_mv_context_format
  ([]
   (clojure.core/apply conexp.main/get-default-mv-context-format [])))
 (clojure.core/defn
  conexp-clj-make_context_nc
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/make-context-nc
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-floor
  ([n] (clojure.core/apply conexp.main/floor [n])))
 (clojure.core/defn
  conexp-clj-improve_basic_order
  ([base clop]
   (clojure.core/apply conexp.main/improve-basic-order [base clop])))
 (clojure.core/defn
  conexp-clj-concepts
  ([ctx] (clojure.core/apply conexp.main/concepts [ctx])))
 (clojure.core/defn
  conexp-clj-disjoint_union
  ([& sets]
   (clojure.core/apply
    conexp.main/disjoint-union
    (clojure.core/vec (clojure.core/list* sets)))))
 (clojure.core/defn
  conexp-clj-make_mv_context
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/make-mv-context
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-read_lattice
  ([file] (clojure.core/apply conexp.main/read-lattice [file]))
  ([file explicit-format]
   (clojure.core/apply
    conexp.main/read-lattice
    [file explicit-format])))
 (clojure.core/defn
  conexp-clj-confidence
  ([implication context]
   (clojure.core/apply conexp.main/confidence [implication context])))
 (clojure.core/defn
  conexp-clj-write_layout
  ([format layout file]
   (clojure.core/apply conexp.main/write-layout [format layout file]))
  ([layout file]
   (clojure.core/apply conexp.main/write-layout [layout file])))
 (clojure.core/defn
  conexp-clj-clarify_objects
  ([ctx] (clojure.core/apply conexp.main/clarify-objects [ctx])))
 (clojure.core/defn
  conexp-clj-make_mv_context_nc
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/make-mv-context-nc
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-map_by_fn
  ([function keys]
   (clojure.core/apply conexp.main/map-by-fn [function keys])))
 (clojure.core/defn
  conexp-clj-object_derivation
  ([ctx objects]
   (clojure.core/apply conexp.main/object-derivation [ctx objects])))
 (clojure.core/defn
  conexp-clj-context_p
  ([thing] (clojure.core/apply conexp.main/context? [thing])))
 (clojure.core/defn
  conexp-clj-intersection
  ([s1] (clojure.core/apply conexp.main/intersection [s1]))
  ([s1 s2] (clojure.core/apply conexp.main/intersection [s1 s2]))
  ([s1 s2 & sets]
   (clojure.core/apply
    conexp.main/intersection
    (clojure.core/vec (clojure.core/list* s1 s2 sets)))))
 (clojure.core/defn
  conexp-clj-round
  ([n] (clojure.core/apply conexp.main/round [n])))
 (clojure.core/defn
  conexp-clj-inits
  ([sqn] (clojure.core/apply conexp.main/inits [sqn])))
 (clojure.core/defn
  conexp-clj-stem_base_from_base
  ([implications]
   (clojure.core/apply
    conexp.main/stem-base-from-base
    [implications])))
 (clojure.core/defn
  conexp-clj-smallest_bond
  ([ctx-1 ctx-2 rel]
   (clojure.core/apply conexp.main/smallest-bond [ctx-1 ctx-2 rel])))
 (clojure.core/defn
  conexp-clj-extents
  ([ctx] (clojure.core/apply conexp.main/extents [ctx])))
 (clojure.core/defn
  conexp-clj-complete_implication_set_p
  ([ctx impl-set]
   (clojure.core/apply
    conexp.main/complete-implication-set?
    [ctx impl-set])))
 (clojure.core/defn
  conexp-clj-clojure_type
  ([thing] (clojure.core/apply conexp.main/clojure-type [thing])))
 (clojure.core/defn
  conexp-clj-gcd
  ([a b] (clojure.core/apply conexp.main/gcd [a b])))
 (clojure.core/defn
  conexp-clj-read_mv_context
  ([file] (clojure.core/apply conexp.main/read-mv-context [file]))
  ([file explicit-format]
   (clojure.core/apply
    conexp.main/read-mv-context
    [file explicit-format])))
 (clojure.core/defn
  conexp-clj-illegal_state
  ([& strings]
   (clojure.core/apply
    conexp.main/illegal-state
    (clojure.core/vec (clojure.core/list* strings)))))
 nil
 (clojure.core/defn
  conexp-clj-list_lattice_output_formats
  ([] (clojure.core/apply conexp.main/list-lattice-output-formats [])))
 (clojure.core/defn
  conexp-clj-context_attribute_closure
  ([ctx set-of-attributes]
   (clojure.core/apply
    conexp.main/context-attribute-closure
    [ctx set-of-attributes])))
 (clojure.core/defn
  conexp-clj-list_layout_formats
  ([] (clojure.core/apply conexp.main/list-layout-formats [])))
 (clojure.core/defn
  conexp-clj-name_with_attributes
  ([name macro-args]
   (clojure.core/apply
    conexp.main/name-with-attributes
    [name macro-args])))
 (clojure.core/defn
  conexp-clj-next_closed_set
  ([base clop A]
   (clojure.core/apply conexp.main/next-closed-set [base clop A])))
 (clojure.core/defn
  conexp-clj-available_formats
  ([type] (clojure.core/apply conexp.main/available-formats [type])))
 (clojure.core/defn
  conexp-clj-frequent_closed_itemsets
  ([context minsupp]
   (clojure.core/apply
    conexp.main/frequent-closed-itemsets
    [context minsupp])))
 (clojure.core/defn
  conexp-clj-context_transitive_closure
  ([ctx]
   (clojure.core/apply conexp.main/context-transitive-closure [ctx])))
 (clojure.core/defn
  conexp-clj-singleton_p
  ([x] (clojure.core/apply conexp.main/singleton? [x])))
 (clojure.core/defn
  conexp-clj-values_of_attribute
  ([mv-ctx m]
   (clojure.core/apply conexp.main/values-of-attribute [mv-ctx m])))
 (clojure.core/defn
  conexp-clj-adprime
  ([ctx set-of-attributes]
   (clojure.core/apply conexp.main/adprime [ctx set-of-attributes])))
 (clojure.core/defn
  conexp-clj-adiag_context
  ([base-set]
   (clojure.core/apply conexp.main/adiag-context [base-set])))
 (clojure.core/defn
  conexp-clj-illegal_argument
  ([& strings]
   (clojure.core/apply
    conexp.main/illegal-argument
    (clojure.core/vec (clojure.core/list* strings)))))
 (clojure.core/defn
  conexp-clj-all_shared_intents
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/all-shared-intents [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-diag_context
  ([base-set]
   (clojure.core/apply conexp.main/diag-context [base-set])))
 (clojure.core/defn
  conexp-clj-context_semiproduct
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-semiproduct [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-list_lattice_input_formats
  ([] (clojure.core/apply conexp.main/list-lattice-input-formats [])))
 (clojure.core/defn
  conexp-clj-next_closed_set_in_family
  ([predicate base clop A]
   (clojure.core/apply
    conexp.main/next-closed-set-in-family
    [predicate base clop A])))
 nil
 (clojure.core/defn
  conexp-clj-context_xia_product
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-xia-product [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-difference
  ([s1] (clojure.core/apply conexp.main/difference [s1]))
  ([s1 s2] (clojure.core/apply conexp.main/difference [s1 s2]))
  ([s1 s2 & sets]
   (clojure.core/apply
    conexp.main/difference
    (clojure.core/vec (clojure.core/list* s1 s2 sets)))))
 (clojure.core/defn
  conexp-clj-get_default_lattice_format
  ([] (clojure.core/apply conexp.main/get-default-lattice-format [])))
 (clojure.core/defn
  conexp-clj-pseudo_close_under_implications
  ([implications set]
   (clojure.core/apply
    conexp.main/pseudo-close-under-implications
    [implications set])))
 (clojure.core/defn
  conexp-clj-rename
  ([xrel kmap] (clojure.core/apply conexp.main/rename [xrel kmap])))
 (clojure.core/defn
  conexp-clj-random_context
  ([base-set fill-rate]
   (clojure.core/apply
    conexp.main/random-context
    [base-set fill-rate]))
  ([objects attributes fill-rate]
   (clojure.core/apply
    conexp.main/random-context
    [objects attributes fill-rate])))
 (clojure.core/defn
  conexp-clj-aprime
  ([ctx attributes]
   (clojure.core/apply conexp.main/aprime [ctx attributes])))
 (clojure.core/defn
  conexp-clj-transitive_reduction
  ([pairs]
   (clojure.core/apply conexp.main/transitive-reduction [pairs]))
  ([base pred]
   (clojure.core/apply conexp.main/transitive-reduction [base pred])))
 (clojure.core/defn
  conexp-clj-lectic__lt_
  ([base A B] (clojure.core/apply conexp.main/lectic-< [base A B])))
 (clojure.core/defn
  conexp-clj-concept_p
  ([ctx [set-of-obj set-of-att]]
   (clojure.core/apply
    conexp.main/concept?
    [ctx [set-of-obj set-of-att]])))
 (clojure.core/defn
  conexp-clj-equivalent_implications_p
  ([impls-1 impls-2]
   (clojure.core/apply
    conexp.main/equivalent-implications?
    [impls-1 impls-2])))
 (clojure.core/defn
  conexp-clj-context_subposition
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-subposition [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-has_version_p
  ([{my-major :major, my-minor :minor, my-patch :patch}]
   (clojure.core/apply
    conexp.main/has-version?
    [{my-major :major, my-minor :minor, my-patch :patch}])))
 (clojure.core/defn
  conexp-clj-not_yet_implemented
  ([] (clojure.core/apply conexp.main/not-yet-implemented [])))
 (clojure.core/defn
  conexp-clj-all_closed_sets_in_family
  ([predicate base clop]
   (clojure.core/apply
    conexp.main/all-closed-sets-in-family
    [predicate base clop]))
  ([predicate base clop initial]
   (clojure.core/apply
    conexp.main/all-closed-sets-in-family
    [predicate base clop initial])))
 (clojure.core/defn
  conexp-clj-restrict_concept
  ([concept subcontext]
   (clojure.core/apply
    conexp.main/restrict-concept
    [concept subcontext])))
 (clojure.core/defn
  conexp-clj-context_union
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-union [ctx-1 ctx-2])))
 nil
 (clojure.core/defn
  conexp-clj-all_bonds_by_shared_intents
  ([ctx-1 ctx-2]
   (clojure.core/apply
    conexp.main/all-bonds-by-shared-intents
    [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-make_lattice
  ([& args]
   (clojure.core/apply
    conexp.main/make-lattice
    (clojure.core/vec (clojure.core/list* args)))))
 (clojure.core/defn
  conexp-clj-list_lattice_formats
  ([] (clojure.core/apply conexp.main/list-lattice-formats [])))
 (clojure.core/defn
  conexp-clj-one_context
  ([base-set] (clojure.core/apply conexp.main/one-context [base-set])))
 (clojure.core/defn
  conexp-clj-write_lattice
  ([format lattice file]
   (clojure.core/apply
    conexp.main/write-lattice
    [format lattice file]))
  ([lattice file]
   (clojure.core/apply conexp.main/write-lattice [lattice file])))
 (clojure.core/defn
  conexp-clj-dissoc_in
  ([m [k & ks :as keys]]
   (clojure.core/apply
    conexp.main/dissoc-in
    [m (clojure.core/vec (clojure.core/list* k ks))])))
 nil
 (clojure.core/defn
  conexp-clj-reduce_context
  ([ctx] (clojure.core/apply conexp.main/reduce-context [ctx])))
 (clojure.core/defn
  conexp-clj-dual_context
  ([ctx] (clojure.core/apply conexp.main/dual-context [ctx])))
 (clojure.core/defn
  conexp-clj-lcm
  ([a b] (clojure.core/apply conexp.main/lcm [a b])))
 nil
 (clojure.core/defn
  conexp-clj-pseudo_intents
  ([ctx] (clojure.core/apply conexp.main/pseudo-intents [ctx])))
 (clojure.core/defn
  conexp-clj-luxenburger_basis
  ([context minsupp minconf]
   (clojure.core/apply
    conexp.main/luxenburger-basis
    [context minsupp minconf])))
 (clojure.core/defn
  conexp-clj-now
  ([] (clojure.core/apply conexp.main/now [])))
 (clojure.core/defn
  conexp-clj-implication_context
  ([n] (clojure.core/apply conexp.main/implication-context [n])))
 (clojure.core/defn
  conexp-clj-pseudo_clop_by_implications
  ([implications]
   (clojure.core/apply
    conexp.main/pseudo-clop-by-implications
    [implications])))
 (clojure.core/defn
  conexp-clj-attributes
  ([ctx] (clojure.core/apply conexp.main/attributes [ctx])))
 (clojure.core/defn
  conexp-clj-seqable_p
  ([x] (clojure.core/apply conexp.main/seqable? [x])))
 (clojure.core/defn
  conexp-clj-minimum_set_covers
  ([base-set sets]
   (clojure.core/apply
    conexp.main/minimum-set-covers
    [base-set sets])))
 (clojure.core/defn
  conexp-clj-intents
  ([ctx] (clojure.core/apply conexp.main/intents [ctx])))
 (clojure.core/defn
  conexp-clj-graph_of_function_p
  ([relation source target]
   (clojure.core/apply
    conexp.main/graph-of-function?
    [relation source target])))
 (clojure.core/defn
  conexp-clj-clarify_attributes
  ([ctx] (clojure.core/apply conexp.main/clarify-attributes [ctx])))
 (clojure.core/defn
  conexp-clj-new_by_name
  ([class-name & args]
   (clojure.core/apply
    conexp.main/new-by-name
    (clojure.core/vec (clojure.core/list* class-name args)))))
 nil
 (clojure.core/defn
  conexp-clj-explore_attributes
  ([ctx & options__201__auto__]
   (clojure.core/apply
    conexp.main/explore-attributes
    (clojure.core/vec (clojure.core/list* ctx options__201__auto__)))))
 (clojure.core/defn
  conexp-clj-proper_conclusion
  ([ctx A] (clojure.core/apply conexp.main/proper-conclusion [ctx A])))
 (clojure.core/defn
  conexp-clj-__gt_Implication
  ([premise conclusion]
   (clojure.core/apply
    conexp.main/->Implication
    [premise conclusion])))
 (clojure.core/defn
  conexp-clj-list_context_output_formats
  ([] (clojure.core/apply conexp.main/list-context-output-formats [])))
 (clojure.core/defn
  conexp-clj-object_concept
  ([ctx g] (clojure.core/apply conexp.main/object-concept [ctx g])))
 (clojure.core/defn
  conexp-clj-context_from_clop
  ([base-set clop]
   (clojure.core/apply conexp.main/context-from-clop [base-set clop])))
 (clojure.core/defn
  conexp-clj-context_apposition
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-apposition [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-rename_keys
  ([map kmap] (clojure.core/apply conexp.main/rename-keys [map kmap])))
 (clojure.core/defn
  conexp-clj-set_default_lattice_format_f
  ([format__3361__auto__]
   (clojure.core/apply
    conexp.main/set-default-lattice-format!
    [format__3361__auto__])))
 (clojure.core/defn
  conexp-clj-proper_premise_implications
  ([ctx]
   (clojure.core/apply conexp.main/proper-premise-implications [ctx])))
 (clojure.core/defn
  conexp-clj-reduce_objects
  ([ctx] (clojure.core/apply conexp.main/reduce-objects [ctx])))
 (clojure.core/defn
  conexp-clj-lattice_upper_neighbours
  ([lat x]
   (clojure.core/apply conexp.main/lattice-upper-neighbours [lat x])))
 (clojure.core/defn
  conexp-clj-warn
  ([message] (clojure.core/apply conexp.main/warn [message])))
 (clojure.core/defn
  conexp-clj-context_disjoint_union
  ([ctx-1 ctx-2]
   (clojure.core/apply
    conexp.main/context-disjoint-union
    [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-__gt_Many_Valued_Context
  ([objects attributes incidence]
   (clojure.core/apply
    conexp.main/->Many-Valued-Context
    [objects attributes incidence])))
 (clojure.core/defn
  conexp-clj-context_sum
  ([ctx-1 ctx-2]
   (clojure.core/apply conexp.main/context-sum [ctx-1 ctx-2])))
 (clojure.core/defn
  conexp-clj-read_context
  ([file] (clojure.core/apply conexp.main/read-context [file]))
  ([file explicit-format]
   (clojure.core/apply
    conexp.main/read-context
    [file explicit-format])))
 (clojure.core/defn
  conexp-clj-close_under_implications
  ([implications set]
   (clojure.core/apply
    conexp.main/close-under-implications
    [implications set])))
 (clojure.core/defn
  conexp-clj-attribute_derivation
  ([ctx attributes]
   (clojure.core/apply
    conexp.main/attribute-derivation
    [ctx attributes])))
 (clojure.core/defn
  conexp-clj-reduce_attributes
  ([ctx] (clojure.core/apply conexp.main/reduce-attributes [ctx])))
 (clojure.core/defn
  conexp-clj-immigrate
  ([& ns-names]
   (clojure.core/apply
    conexp.main/immigrate
    (clojure.core/vec (clojure.core/list* ns-names)))))
 (clojure.core/defn
  conexp-clj-stem_base
  ([ctx] (clojure.core/apply conexp.main/stem-base [ctx]))
  ([ctx background-knowledge]
   (clojure.core/apply
    conexp.main/stem-base
    [ctx background-knowledge])))
 nil
 (clojure.core/defn
  conexp-clj-proper_superset_p
  ([set-1 set-2]
   (clojure.core/apply conexp.main/proper-superset? [set-1 set-2])))
 (clojure.core/defn
  conexp-clj-print_context
  ([ctx & args]
   (clojure.core/apply
    conexp.main/print-context
    (clojure.core/vec (clojure.core/list* ctx args)))))
 (clojure.core/defn
  conexp-clj-ensure_length
  ([string length]
   (clojure.core/apply conexp.main/ensure-length [string length]))
  ([string length padding]
   (clojure.core/apply
    conexp.main/ensure-length
    [string length padding])))
 nil
 (clojure.core/defn
  conexp-clj-respects_p
  ([set impl] (clojure.core/apply conexp.main/respects? [set impl])))
 nil
 (clojure.core/defn
  conexp-clj-to_set
  ([thing] (clojure.core/apply conexp.main/to-set [thing])))
 (clojure.core/defn
  conexp-clj-direct_upper_concepts
  ([ctx [A B]]
   (clojure.core/apply conexp.main/direct-upper-concepts [ctx [A B]])))
 (clojure.core/defn
  conexp-clj-conclusion
  ([thing] (clojure.core/apply conexp.main/conclusion [thing])))
 (clojure.core/defn
  conexp-clj-ceil
  ([n] (clojure.core/apply conexp.main/ceil [n])))
 (clojure.core/defn
  conexp-clj-lattice_inf_irreducibles
  ([lat]
   (clojure.core/apply conexp.main/lattice-inf-irreducibles [lat])))
 (clojure.core/defn
  conexp-clj-index
  ([xrel ks] (clojure.core/apply conexp.main/index [xrel ks])))
 (clojure.core/defn
  conexp-clj-clarify_context
  ([ctx] (clojure.core/apply conexp.main/clarify-context [ctx])))
 (clojure.core/defn
  conexp-clj-get_default_layout_format
  ([] (clojure.core/apply conexp.main/get-default-layout-format [])))
 (clojure.core/defn
  conexp-clj-direct_lower_concepts
  ([ctx [A B]]
   (clojure.core/apply conexp.main/direct-lower-concepts [ctx [A B]])))
 (clojure.core/defn
  conexp-clj-superset_p
  ([set1 set2] (clojure.core/apply conexp.main/superset? [set1 set2])))
 (clojure.core/defn
  conexp-clj-set_default_layout_format_f
  ([format__3361__auto__]
   (clojure.core/apply
    conexp.main/set-default-layout-format!
    [format__3361__auto__])))
 (clojure.core/defn
  conexp-clj-inf_additive_layout
  ([lattice]
   (clojure.core/apply conexp.main/inf-additive-layout [lattice])))
 (clojure.core/defn
  conexp-clj-holds_p
  ([impl ctx] (clojure.core/apply conexp.main/holds? [impl ctx])))
 (clojure.core/defn
  conexp-clj-lattice_sup_irreducibles
  ([lat]
   (clojure.core/apply conexp.main/lattice-sup-irreducibles [lat])))
 (clojure.core/defn
  conexp-clj-hash_combine_hash
  ([& args]
   (clojure.core/apply
    conexp.main/hash-combine-hash
    (clojure.core/vec (clojure.core/list* args))))))
