(do
 (clojure.core/ns
  conexp.contrib.JavaInterface
  (:require conexp.main)
  (:gen-class
   :prefix
   conexp-clj-
   :methods
   (^{:static true}
    [zip [Object Object] Object]
    ^{:static true}
    [list_context_input_formats [] Object]
    ^{:static true}
    [transitive_closure [Object] Object]
    ^{:static true}
    [default_handler [Object Object Object] Object]
    ^{:static true}
    [context_object_closure [Object Object] Object]
    ^{:static true}
    [expt [Object Object] Object]
    ^{:static true}
    [sqrt [Object] Object]
    ^{:static true}
    [null_context [Object] Object]
    ^{:static true}
    [list_mv_context_formats [] Object]
    ^{:static true}
    [values_of_object [Object Object] Object]
    ^{:static true}
    [has_lattice_order [Object] Object]
    ^{:static true}
    [lattice_coatoms [Object] Object]
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
    [sort_by_first [Object Object] Object]
    ^{:static true}
    [get_default_context_format [] Object]
    ^{:static true}
    [make_association_rule_nc [Object Object Object] Object]
    ^{:static true}
    [make_association_rule_nc [Object Object Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object] Object]
    ^{:static true}
    [make_lattice_nc [Object Object Object Object] Object]
    ^{:static true}
    [proper_premise [Object Object] Object]
    ^{:static true}
    [graph_of_function [Object Object Object] Object]
    ^{:static true}
    [reduce_clarified_context [Object] Object]
    ^{:static true}
    [union [] Object]
    ^{:static true}
    [union [Object] Object]
    ^{:static true}
    [union [Object Object] Object]
    ^{:static true}
    [union [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [subset [Object Object] java.lang.Boolean]
    ^{:static true}
    [support [Object Object] Object]
    ^{:static true}
    [support [conexp.fca.association_rules.Association-Rule] Object]
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
    [partial_max [Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [object_clarified [Object] Object]
    ^{:static true}
    [ordinal_scale [Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object] Object]
    ^{:static true}
    [ordinal_scale [Object Object Object] Object]
    ^{:static true}
    [proper_subset [Object Object] Object]
    ^{:static true}
    [attribute_reduced [Object] Object]
    ^{:static true}
    [clarified [Object] Object]
    ^{:static true}
    [context_composition [Object Object] Object]
    ^{:static true}
    [distinct_by_key [Object Object] Object]
    ^{:static true}
    [split_at_last [Object Object] Object]
    ^{:static true}
    [oprime [Object Object] Object]
    ^{:static true}
    [cross_product ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [nominal_scale [Object] Object]
    ^{:static true}
    [nominal_scale [Object Object] Object]
    ^{:static true}
    [directly_neighboured [Object Object Object] Object]
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
    [complete_implication_set [Object Object] Object]
    ^{:static true}
    [make_mv_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [proper_superset [Object Object] Object]
    ^{:static true}
    [exact_integer_sqrt [Object] Object]
    ^{:static true}
    [make_association_rule [Object Object Object] Object]
    ^{:static true}
    [make_association_rule [Object Object Object Object] Object]
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
    [holds [Object Object] Object]
    ^{:static true}
    [up_arrows [Object] Object]
    ^{:static true}
    [reflexive_transitive_closure [Object Object] Object]
    ^{:static true}
    [partial_min [Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [objects [Object] Object]
    ^{:static true}
    [scale_mv_context [Object Object] Object]
    ^{:static true}
    [bond [Object Object Object] Object]
    ^{:static true}
    [split_at_first [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object] Object]
    ^{:static true}
    [rand_context [Object Object Object] Object]
    ^{:static true}
    [make_context_from_matrix [Object Object Object] Object]
    ^{:static true}
    [test_conexp [] Object]
    ^{:static true}
    [test_conexp [Object] Object]
    ^{:static true}
    [list_mv_context_input_formats [] Object]
    ^{:static true}
    [set_default_mv_context_format [Object] Object]
    ^{:static true}
    [biordinal_scale [Object Object] Object]
    ^{:static true}
    [biordinal_scale [Object Object Object Object Object] Object]
    ^{:static true}
    [set_default_context_format [Object] Object]
    ^{:static true}
    [context [Object] Object]
    ^{:static true}
    [mv_context_to_string [Object] Object]
    ^{:static true}
    [mv_context_to_string [Object Object Object] Object]
    ^{:static true}
    [context_size [Object] Object]
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
    [follows_semantically [Object Object] Object]
    ^{:static true}
    [invert_context [Object] Object]
    ^{:static true}
    [subsets [Object] Object]
    ^{:static true}
    [set_default_layout_format [Object] Object]
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
    [unsupported_operation ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [first_non_nil [Object] Object]
    ^{:static true}
    [lattice_one [Object] Object]
    ^{:static true}
    [with_str_out ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [incidence [Object] Object]
    ^{:static true}
    [make_handler ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [lattice_atoms [Object] Object]
    ^{:static true}
    [ask [Object Object "[Ljava.lang.Object;"] Object]
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
    [compatible_subcontexts [Object] Object]
    ^{:static true}
    [ensure_seq [Object] Object]
    ^{:static true}
    [rename_attributes [Object Object] Object]
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
    [yes_or_no [Object] Object]
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
    [singleton [Object] Object]
    ^{:static true}
    [disjoint_union ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [make_mv_context [Object Object Object] Object]
    ^{:static true}
    [equivalent_implications [Object Object] Object]
    ^{:static true}
    [read_lattice [Object] Object]
    ^{:static true}
    [read_lattice [Object Object] Object]
    ^{:static true}
    [confidence [Object Object Object] Object]
    ^{:static true}
    [confidence [conexp.fca.association_rules.Association-Rule] Object]
    ^{:static true}
    [write_layout [Object Object Object] Object]
    ^{:static true}
    [write_layout [Object Object] Object]
    ^{:static true}
    [clarify_objects [Object] Object]
    ^{:static true}
    [make_mv_context_nc [Object Object Object] Object]
    ^{:static true}
    [modular [Object] Object]
    ^{:static true}
    [map_by_fn [Object Object] Object]
    ^{:static true}
    [object_derivation [Object Object] Object]
    ^{:static true}
    [intersection [Object] Object]
    ^{:static true}
    [intersection [Object Object] Object]
    ^{:static true}
    [intersection [Object Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [round [Object] Object]
    ^{:static true}
    [inits [Object] Object]
    ^{:static true}
    [attribute_clarified [Object] Object]
    ^{:static true}
    [stem_base_from_base [Object] Object]
    ^{:static true}
    [smallest_bond [Object Object Object] Object]
    ^{:static true}
    [extents [Object] Object]
    ^{:static true}
    [clojure_type [Object] Object]
    ^{:static true}
    [gcd [Object Object] Object]
    ^{:static true}
    [read_mv_context [Object] Object]
    ^{:static true}
    [read_mv_context [Object Object] Object]
    ^{:static true}
    [illegal_state ["[Ljava.lang.Object;"] Object]
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
    [context_transitive_closure [Object] Object]
    ^{:static true}
    [values_of_attribute [Object Object] Object]
    ^{:static true}
    [reduce [Object Object Object] Object]
    ^{:static true}
    [adiag_context [Object] Object]
    ^{:static true}
    [illegal_argument ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [all_shared_intents [Object Object] Object]
    ^{:static true}
    [diag_context [Object] Object]
    ^{:static true}
    [context_semiproduct [Object Object] Object]
    ^{:static true}
    [set_default_lattice_format [Object] Object]
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
    [difference [Object Object "[Ljava.lang.Object;"] Object]
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
    [reduced [Object] Object]
    ^{:static true}
    [transitive_reduction [Object] Object]
    ^{:static true}
    [transitive_reduction [Object Object] Object]
    ^{:static true}
    [has_version [Object] Object]
    ^{:static true}
    [lectic__lt_ [Object Object Object] Object]
    ^{:static true}
    [object_reduced [Object] Object]
    ^{:static true}
    [context_subposition [Object Object] Object]
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
    [make_lattice ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [list_lattice_formats [] Object]
    ^{:static true}
    [one_context [Object] Object]
    ^{:static true}
    [write_lattice [Object Object Object] Object]
    ^{:static true}
    [write_lattice [Object Object] Object]
    ^{:static true}
    [reduce_context [Object] Object]
    ^{:static true}
    [dual_context [Object] Object]
    ^{:static true}
    [__gt_Association_Rule [Object Object Object Object] Object]
    ^{:static true}
    [lcm [Object Object] Object]
    ^{:static true}
    [pseudo_intents [Object] Object]
    ^{:static true}
    [luxenburger_basis [Object Object Object] Object]
    ^{:static true}
    [now [] Object]
    ^{:static true}
    [pseudo_clop_by_implications [Object] Object]
    ^{:static true}
    [attributes [Object] Object]
    ^{:static true}
    [minimal_implication_set [Object] Object]
    ^{:static true}
    [intents [Object] Object]
    ^{:static true}
    [respects [Object Object] Object]
    ^{:static true}
    [die_with_error [Class Object] Object]
    ^{:static true}
    [clarify_attributes [Object] Object]
    ^{:static true}
    [distributive [Object] Object]
    ^{:static true}
    [sound_implication_set [Object Object] Object]
    ^{:static true}
    [explore_attributes [Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [__gt_Implication [Object Object] Object]
    ^{:static true}
    [concept [Object Object] Object]
    ^{:static true}
    [list_context_output_formats [] Object]
    ^{:static true}
    [object_concept [Object Object] Object]
    ^{:static true}
    [context_from_clop [Object Object] Object]
    ^{:static true}
    [subcontext [Object Object] Object]
    ^{:static true}
    [context_apposition [Object Object] Object]
    ^{:static true}
    [rename_keys [Object Object] Object]
    ^{:static true}
    [proper_premise_implications [Object] Object]
    ^{:static true}
    [reduce_objects [Object] Object]
    ^{:static true}
    [lattice_upper_neighbours [Object Object] Object]
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
    [immigrate ["[Ljava.lang.Object;"] Object]
    ^{:static true}
    [stem_base [Object] Object]
    ^{:static true}
    [stem_base [Object Object] Object]
    ^{:static true}
    [compatible_subcontext [Object Object] Object]
    ^{:static true}
    [superset [Object Object] java.lang.Boolean]
    ^{:static true}
    [print_context [Object "[Ljava.lang.Object;"] Object]
    ^{:static true}
    [ensure_length [Object Object] Object]
    ^{:static true}
    [ensure_length [Object Object Object] Object]
    ^{:static true}
    [to_set [Object] Object]
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
    [inf_additive_layout [Object] Object]
    ^{:static true}
    [lattice_sup_irreducibles [Object] Object]
    ^{:static true}
    [incident [Object Object Object] Object]
    ^{:static true}
    [iceberg_intent_seq [Object Object] Object]
    ^{:static true}
    [hash_combine_hash ["[Ljava.lang.Object;"] Object])))
 (clojure.core/import 'conexp.fca.contexts.Context)
 (clojure.core/import 'conexp.fca.lattices.Lattice)
 (clojure.core/import 'conexp.fca.association_rules.Association-Rule)
 (clojure.core/defn
  conexp-clj-zip
  ([seq-1 seq-2] (conexp.main/zip seq-1 seq-2)))
 (clojure.core/defn
  conexp-clj-list_context_input_formats
  ([] (conexp.main/list-context-input-formats)))
 (clojure.core/defn
  conexp-clj-transitive_closure
  ([pairs] (conexp.main/transitive-closure pairs)))
 (clojure.core/defn
  conexp-clj-default_handler
  ([ctx known impl] (conexp.main/default-handler ctx known impl)))
 (clojure.core/defn
  conexp-clj-context_object_closure
  ([ctx set-of-objects]
   (conexp.main/context-object-closure ctx set-of-objects)))
 (clojure.core/defn
  conexp-clj-expt
  ([base pow] (conexp.main/expt base pow)))
 (clojure.core/defn conexp-clj-sqrt ([n] (conexp.main/sqrt n)))
 (clojure.core/defn
  conexp-clj-null_context
  ([base-set] (conexp.main/null-context base-set)))
 (clojure.core/defn
  conexp-clj-list_mv_context_formats
  ([] (conexp.main/list-mv-context-formats)))
 (clojure.core/defn
  conexp-clj-values_of_object
  ([mv-ctx g] (conexp.main/values-of-object mv-ctx g)))
 (clojure.core/defn
  conexp-clj-has_lattice_order
  ([lat] (conexp.main/has-lattice-order? lat)))
 (clojure.core/defn
  conexp-clj-lattice_coatoms
  ([lat] (conexp.main/lattice-coatoms lat)))
 (clojure.core/defn
  conexp-clj-random_contexts
  ([number upper-limit]
   (conexp.main/random-contexts number upper-limit)))
 (clojure.core/defn
  conexp-clj-interordinal_scale
  ([values] (conexp.main/interordinal-scale values))
  ([values <= >=] (conexp.main/interordinal-scale values <= >=))
  ([values others <= >=]
   (conexp.main/interordinal-scale values others <= >=)))
 (clojure.core/defn
  conexp-clj-list_mv_context_output_formats
  ([] (conexp.main/list-mv-context-output-formats)))
 (clojure.core/defn
  conexp-clj-order
  ([^Lattice lattice] (conexp.main/order ^Lattice lattice)))
 (clojure.core/defn
  conexp-clj-up_down_arrows
  ([ctx] (conexp.main/up-down-arrows ctx)))
 (clojure.core/defn
  conexp-clj-conexp_version
  ([] (conexp.main/conexp-version)))
 (clojure.core/defn conexp-clj-tails ([sqn] (conexp.main/tails sqn)))
 (clojure.core/defn
  conexp-clj-sup
  ([^Lattice lattice] (conexp.main/sup ^Lattice lattice)))
 (clojure.core/defn
  conexp-clj-base_set
  ([^Lattice lattice] (conexp.main/base-set ^Lattice lattice)))
 (clojure.core/defn
  conexp-clj-sort_by_first
  ([x y] (conexp.main/sort-by-first x y)))
 (clojure.core/defn
  conexp-clj-get_default_context_format
  ([] (conexp.main/get-default-context-format)))
 (clojure.core/defn
  conexp-clj-make_association_rule_nc
  ([context premise conclusion]
   (conexp.main/make-association-rule-nc context premise conclusion))
  ([premise conclusion support confidence]
   (conexp.main/make-association-rule-nc
    premise
    conclusion
    support
    confidence)))
 (clojure.core/defn
  conexp-clj-make_lattice_nc
  ([base-set order-function]
   (conexp.main/make-lattice-nc base-set order-function))
  ([base-set inf sup] (conexp.main/make-lattice-nc base-set inf sup))
  ([base-set order-function inf sup]
   (conexp.main/make-lattice-nc base-set order-function inf sup)))
 (clojure.core/defn
  conexp-clj-proper_premise
  ([ctx A] (conexp.main/proper-premise? ctx A)))
 (clojure.core/defn
  conexp-clj-graph_of_function
  ([relation source target]
   (conexp.main/graph-of-function? relation source target)))
 (clojure.core/defn
  conexp-clj-reduce_clarified_context
  ([ctx] (conexp.main/reduce-clarified-context ctx)))
 (clojure.core/defn
  conexp-clj-union
  ([] (conexp.main/union))
  ([s1] (conexp.main/union s1))
  ([s1 s2] (conexp.main/union s1 s2))
  ([s1 s2 & sets] (clojure.core/apply conexp.main/union s1 s2 sets)))
 (clojure.core/defn
  conexp-clj-subset
  ([set1 set2] (conexp.main/subset? set1 set2)))
 (clojure.core/defn
  conexp-clj-support
  ([set ctx] (conexp.main/support set ctx))
  ([^Association-Rule ar] (conexp.main/support ^Association-Rule ar)))
 (clojure.core/defn
  conexp-clj-minimal_generating_subsets
  ([clop A] (conexp.main/minimal-generating-subsets clop A)))
 (clojure.core/defn
  conexp-clj-write_context
  ([format context file]
   (conexp.main/write-context format context file))
  ([context file] (conexp.main/write-context context file)))
 (clojure.core/defn
  conexp-clj-all_closed_sets
  ([base clop] (conexp.main/all-closed-sets base clop))
  ([base clop initial]
   (conexp.main/all-closed-sets base clop initial)))
 (clojure.core/defn
  conexp-clj-select
  ([pred xset] (conexp.main/select pred xset)))
 (clojure.core/defn
  conexp-clj-partial_max
  ([<= & xs] (clojure.core/apply conexp.main/partial-max <= xs)))
 (clojure.core/defn
  conexp-clj-object_clarified
  ([ctx] (conexp.main/object-clarified? ctx)))
 (clojure.core/defn
  conexp-clj-ordinal_scale
  ([values] (conexp.main/ordinal-scale values))
  ([values <=] (conexp.main/ordinal-scale values <=))
  ([values others <=] (conexp.main/ordinal-scale values others <=)))
 (clojure.core/defn
  conexp-clj-proper_subset
  ([set1 set2] (conexp.main/proper-subset? set1 set2)))
 (clojure.core/defn
  conexp-clj-attribute_reduced
  ([ctx] (conexp.main/attribute-reduced? ctx)))
 (clojure.core/defn
  conexp-clj-clarified
  ([ctx] (conexp.main/clarified? ctx)))
 (clojure.core/defn
  conexp-clj-context_composition
  ([ctx-1 ctx-2] (conexp.main/context-composition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-distinct_by_key
  ([sequence key] (conexp.main/distinct-by-key sequence key)))
 (clojure.core/defn
  conexp-clj-split_at_last
  ([predicate sequence]
   (conexp.main/split-at-last predicate sequence)))
 (clojure.core/defn
  conexp-clj-oprime
  ([ctx objects] (conexp.main/oprime ctx objects)))
 (clojure.core/defn
  conexp-clj-cross_product
  ([& sets] (clojure.core/apply conexp.main/cross-product sets)))
 (clojure.core/defn
  conexp-clj-nominal_scale
  ([values] (conexp.main/nominal-scale values))
  ([values others] (conexp.main/nominal-scale values others)))
 (clojure.core/defn
  conexp-clj-directly_neighboured
  ([lat x y] (conexp.main/directly-neighboured? lat x y)))
 (clojure.core/defn
  conexp-clj-__gt_Lattice
  ([base-set order-function inf sup]
   (conexp.main/->Lattice base-set order-function inf sup)))
 (clojure.core/defn
  conexp-clj-dual_lattice
  ([lat] (conexp.main/dual-lattice lat)))
 (clojure.core/defn
  conexp-clj-read_layout
  ([file] (conexp.main/read-layout file))
  ([file explicit-format]
   (conexp.main/read-layout file explicit-format)))
 (clojure.core/defn
  conexp-clj-project
  ([xrel ks] (conexp.main/project xrel ks)))
 (clojure.core/defn
  conexp-clj-map_invert
  ([m] (conexp.main/map-invert m)))
 (clojure.core/defn
  conexp-clj-context_intersection
  ([ctx1 ctx2] (conexp.main/context-intersection ctx1 ctx2)))
 (clojure.core/defn conexp-clj-abs ([n] (conexp.main/abs n)))
 (clojure.core/defn
  conexp-clj-all_bonds
  ([ctx-1 ctx-2] (conexp.main/all-bonds ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-list_layout_output_formats
  ([] (conexp.main/list-layout-output-formats)))
 (clojure.core/defn
  conexp-clj-complete_implication_set
  ([ctx impl-set]
   (conexp.main/complete-implication-set? ctx impl-set)))
 (clojure.core/defn
  conexp-clj-make_mv_context_from_matrix
  ([objects attributes values]
   (conexp.main/make-mv-context-from-matrix
    objects
    attributes
    values)))
 (clojure.core/defn
  conexp-clj-proper_superset
  ([set1 set2] (conexp.main/proper-superset? set1 set2)))
 (clojure.core/defn
  conexp-clj-exact_integer_sqrt
  ([n] (conexp.main/exact-integer-sqrt n)))
 (clojure.core/defn
  conexp-clj-make_association_rule
  ([context premise conclusion]
   (conexp.main/make-association-rule context premise conclusion))
  ([premise conclusion support confidence]
   (conexp.main/make-association-rule
    premise
    conclusion
    support
    confidence)))
 (clojure.core/defn
  conexp-clj-list_context_formats
  ([] (conexp.main/list-context-formats)))
 (clojure.core/defn
  conexp-clj-make_implication
  ([premise conclusion]
   (conexp.main/make-implication premise conclusion)))
 (clojure.core/defn
  conexp-clj-join
  ([xrel yrel] (conexp.main/join xrel yrel))
  ([xrel yrel km] (conexp.main/join xrel yrel km)))
 (clojure.core/defn
  conexp-clj-standard_context
  ([lat] (conexp.main/standard-context lat)))
 (clojure.core/defn
  conexp-clj-holds
  ([impl ctx] (conexp.main/holds? impl ctx)))
 (clojure.core/defn
  conexp-clj-up_arrows
  ([ctx] (conexp.main/up-arrows ctx)))
 (clojure.core/defn
  conexp-clj-reflexive_transitive_closure
  ([base-set pairs]
   (conexp.main/reflexive-transitive-closure base-set pairs)))
 (clojure.core/defn
  conexp-clj-partial_min
  ([<= & xs] (clojure.core/apply conexp.main/partial-min <= xs)))
 (clojure.core/defn
  conexp-clj-objects
  ([ctx] (conexp.main/objects ctx)))
 (clojure.core/defn
  conexp-clj-scale_mv_context
  ([mv-ctx scales] (conexp.main/scale-mv-context mv-ctx scales)))
 (clojure.core/defn
  conexp-clj-bond
  ([ctx-1 ctx-2 ctx] (conexp.main/bond? ctx-1 ctx-2 ctx)))
 (clojure.core/defn
  conexp-clj-split_at_first
  ([predicate sequence]
   (conexp.main/split-at-first predicate sequence)))
 (clojure.core/defn
  conexp-clj-rand_context
  ([base-set fill-rate] (conexp.main/rand-context base-set fill-rate))
  ([objects attributes fill-rate]
   (conexp.main/rand-context objects attributes fill-rate)))
 (clojure.core/defn
  conexp-clj-make_context_from_matrix
  ([G M bits] (conexp.main/make-context-from-matrix G M bits)))
 (clojure.core/defn
  conexp-clj-test_conexp
  ([] (conexp.main/test-conexp))
  ([with-contrib?] (conexp.main/test-conexp with-contrib?)))
 (clojure.core/defn
  conexp-clj-list_mv_context_input_formats
  ([] (conexp.main/list-mv-context-input-formats)))
 (clojure.core/defn
  conexp-clj-set_default_mv_context_format
  ([format__3123__auto__]
   (conexp.main/set-default-mv-context-format! format__3123__auto__)))
 (clojure.core/defn
  conexp-clj-biordinal_scale
  ([values n] (conexp.main/biordinal-scale values n))
  ([values others n <= >=]
   (conexp.main/biordinal-scale values others n <= >=)))
 (clojure.core/defn
  conexp-clj-set_default_context_format
  ([format__3123__auto__]
   (conexp.main/set-default-context-format! format__3123__auto__)))
 (clojure.core/defn
  conexp-clj-context
  ([thing] (conexp.main/context? thing)))
 (clojure.core/defn
  conexp-clj-mv_context_to_string
  ([mv-ctx] (conexp.main/mv-context-to-string mv-ctx))
  ([mv-ctx order-on-objects order-on-attributes]
   (conexp.main/mv-context-to-string
    mv-ctx
    order-on-objects
    order-on-attributes)))
 (clojure.core/defn
  conexp-clj-context_size
  ([ctx] (conexp.main/context-size ctx)))
 (clojure.core/defn
  conexp-clj-lattice_zero
  ([lat] (conexp.main/lattice-zero lat)))
 (clojure.core/defn
  conexp-clj-proper_premises
  ([ctx] (conexp.main/proper-premises ctx)))
 (clojure.core/defn
  conexp-clj-sort_by_second
  ([x y] (conexp.main/sort-by-second x y)))
 (clojure.core/defn
  conexp-clj-__gt_Formal_Context
  ([objects attributes incidence]
   (conexp.main/->Formal-Context objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-lattice_doubly_irreducibles
  ([lat] (conexp.main/lattice-doubly-irreducibles lat)))
 (clojure.core/defn conexp-clj-conexp_namespaces)
 (clojure.core/defn
  conexp-clj-follows_semantically
  ([implication implications]
   (conexp.main/follows-semantically? implication implications)))
 (clojure.core/defn
  conexp-clj-invert_context
  ([ctx] (conexp.main/invert-context ctx)))
 (clojure.core/defn
  conexp-clj-subsets
  ([set] (conexp.main/subsets set)))
 (clojure.core/defn
  conexp-clj-set_default_layout_format
  ([format__3123__auto__]
   (conexp.main/set-default-layout-format! format__3123__auto__)))
 (clojure.core/defn
  conexp-clj-premise
  ([thing] (conexp.main/premise thing)))
 (clojure.core/defn conexp-clj-div ([a b] (conexp.main/div a b)))
 (clojure.core/defn
  conexp-clj-lectic__lt__i
  ([base i A B] (conexp.main/lectic-<_i base i A B)))
 (clojure.core/defn
  conexp-clj-down_arrows
  ([ctx] (conexp.main/down-arrows ctx)))
 (clojure.core/defn
  conexp-clj-lattice_lower_neighbours
  ([lat y] (conexp.main/lattice-lower-neighbours lat y)))
 (clojure.core/defn
  conexp-clj-unsupported_operation
  ([& strings]
   (clojure.core/apply conexp.main/unsupported-operation strings)))
 (clojure.core/defn
  conexp-clj-first_non_nil
  ([seq] (conexp.main/first-non-nil seq)))
 (clojure.core/defn
  conexp-clj-lattice_one
  ([lat] (conexp.main/lattice-one lat)))
 (clojure.core/defn
  conexp-clj-with_str_out
  ([& body] (clojure.core/apply conexp.main/with-str-out body)))
 (clojure.core/defn
  conexp-clj-incidence
  ([ctx] (conexp.main/incidence ctx)))
 (clojure.core/defn
  conexp-clj-make_handler
  ([& options__78__auto__]
   (clojure.core/apply conexp.main/make-handler options__78__auto__)))
 (clojure.core/defn
  conexp-clj-lattice_atoms
  ([lat] (conexp.main/lattice-atoms lat)))
 (clojure.core/defn
  conexp-clj-ask
  ([prompt read & preds-and-fail-messages]
   (clojure.core/apply
    conexp.main/ask
    prompt
    read
    preds-and-fail-messages)))
 (clojure.core/defn
  conexp-clj-make_context
  ([objects attributes incidence]
   (conexp.main/make-context objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-list_layout_input_formats
  ([] (conexp.main/list-layout-input-formats)))
 (clojure.core/defn
  conexp-clj-write_mv_context
  ([format mv-context file]
   (conexp.main/write-mv-context format mv-context file))
  ([mv-context file] (conexp.main/write-mv-context mv-context file)))
 (clojure.core/defn
  conexp-clj-rename_objects
  ([ctx old-to-new] (conexp.main/rename-objects ctx old-to-new)))
 (clojure.core/defn
  conexp-clj-compatible_subcontexts
  ([ctx] (conexp.main/compatible-subcontexts ctx)))
 (clojure.core/defn
  conexp-clj-ensure_seq
  ([x] (conexp.main/ensure-seq x)))
 (clojure.core/defn
  conexp-clj-rename_attributes
  ([ctx old-to-new] (conexp.main/rename-attributes ctx old-to-new)))
 (clojure.core/defn
  conexp-clj-latex
  ([this] (conexp.main/latex this))
  ([this choice] (conexp.main/latex this choice)))
 (clojure.core/defn
  conexp-clj-context_to_string
  ([ctx] (conexp.main/context-to-string ctx))
  ([ctx order-on-objects order-on-attributes]
   (conexp.main/context-to-string
    ctx
    order-on-objects
    order-on-attributes)))
 (clojure.core/defn
  conexp-clj-attribute_concept
  ([ctx m] (conexp.main/attribute-concept ctx m)))
 (clojure.core/defn
  conexp-clj-yes_or_no
  ([question] (conexp.main/yes-or-no? question)))
 (clojure.core/defn
  conexp-clj-set_of_range
  ([end] (conexp.main/set-of-range end))
  ([start end] (conexp.main/set-of-range start end))
  ([start end step] (conexp.main/set-of-range start end step)))
 (clojure.core/defn
  conexp-clj-_lt__eq__gt_
  ([a b] (conexp.main/<=> a b)))
 (clojure.core/defn
  conexp-clj-context_product
  ([ctx-1 ctx-2] (conexp.main/context-product ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-concept_lattice
  ([ctx] (conexp.main/concept-lattice ctx)))
 (clojure.core/defn
  conexp-clj-dichotomic_scale
  ([values] (conexp.main/dichotomic-scale values)))
 (clojure.core/defn
  conexp-clj-clop_by_implications
  ([implications] (conexp.main/clop-by-implications implications)))
 (clojure.core/defn
  conexp-clj-inf
  ([^Lattice lattice] (conexp.main/inf ^Lattice lattice)))
 (clojure.core/defn
  conexp-clj-get_default_mv_context_format
  ([] (conexp.main/get-default-mv-context-format)))
 (clojure.core/defn
  conexp-clj-make_context_nc
  ([objects attributes incidence]
   (conexp.main/make-context-nc objects attributes incidence)))
 (clojure.core/defn conexp-clj-floor ([n] (conexp.main/floor n)))
 (clojure.core/defn
  conexp-clj-improve_basic_order
  ([base clop] (conexp.main/improve-basic-order base clop)))
 (clojure.core/defn
  conexp-clj-concepts
  ([ctx] (conexp.main/concepts ctx)))
 (clojure.core/defn
  conexp-clj-singleton
  ([x] (conexp.main/singleton? x)))
 (clojure.core/defn
  conexp-clj-disjoint_union
  ([& sets] (clojure.core/apply conexp.main/disjoint-union sets)))
 (clojure.core/defn
  conexp-clj-make_mv_context
  ([objects attributes incidence]
   (conexp.main/make-mv-context objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-equivalent_implications
  ([impls-1 impls-2]
   (conexp.main/equivalent-implications? impls-1 impls-2)))
 (clojure.core/defn
  conexp-clj-read_lattice
  ([file] (conexp.main/read-lattice file))
  ([file explicit-format]
   (conexp.main/read-lattice file explicit-format)))
 (clojure.core/defn
  conexp-clj-confidence
  ([premise conclusion context]
   (conexp.main/confidence premise conclusion context))
  ([^Association-Rule ar]
   (conexp.main/confidence ^Association-Rule ar)))
 (clojure.core/defn
  conexp-clj-write_layout
  ([format layout file] (conexp.main/write-layout format layout file))
  ([layout file] (conexp.main/write-layout layout file)))
 (clojure.core/defn
  conexp-clj-clarify_objects
  ([ctx] (conexp.main/clarify-objects ctx)))
 (clojure.core/defn
  conexp-clj-make_mv_context_nc
  ([objects attributes incidence]
   (conexp.main/make-mv-context-nc objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-modular
  ([lat] (conexp.main/modular? lat)))
 (clojure.core/defn
  conexp-clj-map_by_fn
  ([function keys] (conexp.main/map-by-fn function keys)))
 (clojure.core/defn
  conexp-clj-object_derivation
  ([ctx objects] (conexp.main/object-derivation ctx objects)))
 (clojure.core/defn
  conexp-clj-intersection
  ([s1] (conexp.main/intersection s1))
  ([s1 s2] (conexp.main/intersection s1 s2))
  ([s1 s2 & sets]
   (clojure.core/apply conexp.main/intersection s1 s2 sets)))
 (clojure.core/defn conexp-clj-round ([n] (conexp.main/round n)))
 (clojure.core/defn conexp-clj-inits ([sqn] (conexp.main/inits sqn)))
 (clojure.core/defn
  conexp-clj-attribute_clarified
  ([ctx] (conexp.main/attribute-clarified? ctx)))
 (clojure.core/defn
  conexp-clj-stem_base_from_base
  ([implications] (conexp.main/stem-base-from-base implications)))
 (clojure.core/defn
  conexp-clj-smallest_bond
  ([ctx-1 ctx-2 rel] (conexp.main/smallest-bond ctx-1 ctx-2 rel)))
 (clojure.core/defn
  conexp-clj-extents
  ([ctx] (conexp.main/extents ctx)))
 (clojure.core/defn
  conexp-clj-clojure_type
  ([thing] (conexp.main/clojure-type thing)))
 (clojure.core/defn conexp-clj-gcd ([a b] (conexp.main/gcd a b)))
 (clojure.core/defn
  conexp-clj-read_mv_context
  ([file] (conexp.main/read-mv-context file))
  ([file explicit-format]
   (conexp.main/read-mv-context file explicit-format)))
 (clojure.core/defn
  conexp-clj-illegal_state
  ([& strings] (clojure.core/apply conexp.main/illegal-state strings)))
 (clojure.core/defn conexp-clj-Context)
 (clojure.core/defn
  conexp-clj-list_lattice_output_formats
  ([] (conexp.main/list-lattice-output-formats)))
 (clojure.core/defn
  conexp-clj-context_attribute_closure
  ([ctx set-of-attributes]
   (conexp.main/context-attribute-closure ctx set-of-attributes)))
 (clojure.core/defn
  conexp-clj-list_layout_formats
  ([] (conexp.main/list-layout-formats)))
 (clojure.core/defn
  conexp-clj-name_with_attributes
  ([name macro-args]
   (conexp.main/name-with-attributes name macro-args)))
 (clojure.core/defn
  conexp-clj-next_closed_set
  ([base clop A] (conexp.main/next-closed-set base clop A)))
 (clojure.core/defn
  conexp-clj-available_formats
  ([type] (conexp.main/available-formats type)))
 (clojure.core/defn
  conexp-clj-context_transitive_closure
  ([ctx] (conexp.main/context-transitive-closure ctx)))
 (clojure.core/defn
  conexp-clj-values_of_attribute
  ([mv-ctx m] (conexp.main/values-of-attribute mv-ctx m)))
 (clojure.core/defn
  conexp-clj-reduce
  ([fn initial-value coll]
   (conexp.main/reduce! fn initial-value coll)))
 (clojure.core/defn
  conexp-clj-adiag_context
  ([base-set] (conexp.main/adiag-context base-set)))
 (clojure.core/defn
  conexp-clj-illegal_argument
  ([& strings]
   (clojure.core/apply conexp.main/illegal-argument strings)))
 (clojure.core/defn
  conexp-clj-all_shared_intents
  ([ctx-1 ctx-2] (conexp.main/all-shared-intents ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-diag_context
  ([base-set] (conexp.main/diag-context base-set)))
 (clojure.core/defn
  conexp-clj-context_semiproduct
  ([ctx-1 ctx-2] (conexp.main/context-semiproduct ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-set_default_lattice_format
  ([format__3123__auto__]
   (conexp.main/set-default-lattice-format! format__3123__auto__)))
 (clojure.core/defn
  conexp-clj-list_lattice_input_formats
  ([] (conexp.main/list-lattice-input-formats)))
 (clojure.core/defn
  conexp-clj-next_closed_set_in_family
  ([predicate base clop A]
   (conexp.main/next-closed-set-in-family predicate base clop A)))
 (clojure.core/defn conexp-clj-clojure_coll)
 (clojure.core/defn
  conexp-clj-context_xia_product
  ([ctx-1 ctx-2] (conexp.main/context-xia-product ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-difference
  ([s1] (conexp.main/difference s1))
  ([s1 s2] (conexp.main/difference s1 s2))
  ([s1 s2 & sets]
   (clojure.core/apply conexp.main/difference s1 s2 sets)))
 (clojure.core/defn
  conexp-clj-get_default_lattice_format
  ([] (conexp.main/get-default-lattice-format)))
 (clojure.core/defn
  conexp-clj-pseudo_close_under_implications
  ([implications set]
   (conexp.main/pseudo-close-under-implications implications set)))
 (clojure.core/defn
  conexp-clj-rename
  ([xrel kmap] (conexp.main/rename xrel kmap)))
 (clojure.core/defn
  conexp-clj-random_context
  ([base-set fill-rate]
   (conexp.main/random-context base-set fill-rate))
  ([objects attributes fill-rate]
   (conexp.main/random-context objects attributes fill-rate)))
 (clojure.core/defn
  conexp-clj-aprime
  ([ctx attributes] (conexp.main/aprime ctx attributes)))
 (clojure.core/defn
  conexp-clj-reduced
  ([ctx] (conexp.main/reduced? ctx)))
 (clojure.core/defn
  conexp-clj-transitive_reduction
  ([pairs] (conexp.main/transitive-reduction pairs))
  ([base pred] (conexp.main/transitive-reduction base pred)))
 (clojure.core/defn
  conexp-clj-has_version
  ([{my-major :major, my-minor :minor, my-patch :patch}]
   (conexp.main/has-version?
    {my-major :major, my-minor :minor, my-patch :patch})))
 (clojure.core/defn
  conexp-clj-lectic__lt_
  ([base A B] (conexp.main/lectic-< base A B)))
 (clojure.core/defn
  conexp-clj-object_reduced
  ([ctx] (conexp.main/object-reduced? ctx)))
 (clojure.core/defn
  conexp-clj-context_subposition
  ([ctx-1 ctx-2] (conexp.main/context-subposition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-all_closed_sets_in_family
  ([predicate base clop]
   (conexp.main/all-closed-sets-in-family predicate base clop))
  ([predicate base clop initial]
   (conexp.main/all-closed-sets-in-family
    predicate
    base
    clop
    initial)))
 (clojure.core/defn
  conexp-clj-restrict_concept
  ([concept subcontext]
   (conexp.main/restrict-concept concept subcontext)))
 (clojure.core/defn
  conexp-clj-context_union
  ([ctx-1 ctx-2] (conexp.main/context-union ctx-1 ctx-2)))
 (clojure.core/defn conexp-clj-LaTeX)
 (clojure.core/defn
  conexp-clj-all_bonds_by_shared_intents
  ([ctx-1 ctx-2]
   (conexp.main/all-bonds-by-shared-intents ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-make_lattice
  ([& args] (clojure.core/apply conexp.main/make-lattice args)))
 (clojure.core/defn
  conexp-clj-list_lattice_formats
  ([] (conexp.main/list-lattice-formats)))
 (clojure.core/defn
  conexp-clj-one_context
  ([base-set] (conexp.main/one-context base-set)))
 (clojure.core/defn
  conexp-clj-write_lattice
  ([format lattice file]
   (conexp.main/write-lattice format lattice file))
  ([lattice file] (conexp.main/write-lattice lattice file)))
 (clojure.core/defn conexp-clj-standard_layout)
 (clojure.core/defn
  conexp-clj-reduce_context
  ([ctx] (conexp.main/reduce-context ctx)))
 (clojure.core/defn
  conexp-clj-dual_context
  ([ctx] (conexp.main/dual-context ctx)))
 (clojure.core/defn
  conexp-clj-__gt_Association_Rule
  ([premise conclusion support confidence]
   (conexp.main/->Association-Rule
    premise
    conclusion
    support
    confidence)))
 (clojure.core/defn conexp-clj-lcm ([a b] (conexp.main/lcm a b)))
 (clojure.core/defn conexp-clj-clojure_map)
 (clojure.core/defn
  conexp-clj-pseudo_intents
  ([ctx] (conexp.main/pseudo-intents ctx)))
 (clojure.core/defn
  conexp-clj-luxenburger_basis
  ([context minsupp minconf]
   (conexp.main/luxenburger-basis context minsupp minconf)))
 (clojure.core/defn conexp-clj-now ([] (conexp.main/now)))
 (clojure.core/defn
  conexp-clj-pseudo_clop_by_implications
  ([implications]
   (conexp.main/pseudo-clop-by-implications implications)))
 (clojure.core/defn
  conexp-clj-attributes
  ([ctx] (conexp.main/attributes ctx)))
 (clojure.core/defn
  conexp-clj-minimal_implication_set
  ([impl-set] (conexp.main/minimal-implication-set? impl-set)))
 (clojure.core/defn
  conexp-clj-intents
  ([ctx] (conexp.main/intents ctx)))
 (clojure.core/defn
  conexp-clj-respects
  ([set impl] (conexp.main/respects? set impl)))
 (clojure.core/defn
  conexp-clj-die_with_error
  ([^Class error strings]
   (conexp.main/die-with-error ^Class error strings)))
 (clojure.core/defn
  conexp-clj-clarify_attributes
  ([ctx] (conexp.main/clarify-attributes ctx)))
 (clojure.core/defn
  conexp-clj-distributive
  ([lat] (conexp.main/distributive? lat)))
 (clojure.core/defn
  conexp-clj-sound_implication_set
  ([ctx impl-set] (conexp.main/sound-implication-set? ctx impl-set)))
 (clojure.core/defn conexp-clj-clojure_fn)
 (clojure.core/defn
  conexp-clj-explore_attributes
  ([ctx & options__78__auto__]
   (clojure.core/apply
    conexp.main/explore-attributes
    ctx
    options__78__auto__)))
 (clojure.core/defn
  conexp-clj-__gt_Implication
  ([premise conclusion]
   (conexp.main/->Implication premise conclusion)))
 (clojure.core/defn
  conexp-clj-concept
  ([ctx [set-of-obj set-of-att]]
   (conexp.main/concept? ctx [set-of-obj set-of-att])))
 (clojure.core/defn
  conexp-clj-list_context_output_formats
  ([] (conexp.main/list-context-output-formats)))
 (clojure.core/defn
  conexp-clj-object_concept
  ([ctx g] (conexp.main/object-concept ctx g)))
 (clojure.core/defn
  conexp-clj-context_from_clop
  ([base-set clop] (conexp.main/context-from-clop base-set clop)))
 (clojure.core/defn
  conexp-clj-subcontext
  ([ctx-1 ctx-2] (conexp.main/subcontext? ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-context_apposition
  ([ctx-1 ctx-2] (conexp.main/context-apposition ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-rename_keys
  ([map kmap] (conexp.main/rename-keys map kmap)))
 (clojure.core/defn
  conexp-clj-proper_premise_implications
  ([ctx] (conexp.main/proper-premise-implications ctx)))
 (clojure.core/defn
  conexp-clj-reduce_objects
  ([ctx] (conexp.main/reduce-objects ctx)))
 (clojure.core/defn
  conexp-clj-lattice_upper_neighbours
  ([lat x] (conexp.main/lattice-upper-neighbours lat x)))
 (clojure.core/defn
  conexp-clj-context_disjoint_union
  ([ctx-1 ctx-2] (conexp.main/context-disjoint-union ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-__gt_Many_Valued_Context
  ([objects attributes incidence]
   (conexp.main/->Many-Valued-Context objects attributes incidence)))
 (clojure.core/defn
  conexp-clj-context_sum
  ([ctx-1 ctx-2] (conexp.main/context-sum ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-read_context
  ([file] (conexp.main/read-context file))
  ([file explicit-format]
   (conexp.main/read-context file explicit-format)))
 (clojure.core/defn
  conexp-clj-close_under_implications
  ([implications set]
   (conexp.main/close-under-implications implications set)))
 (clojure.core/defn
  conexp-clj-attribute_derivation
  ([ctx attributes] (conexp.main/attribute-derivation ctx attributes)))
 (clojure.core/defn
  conexp-clj-reduce_attributes
  ([ctx] (conexp.main/reduce-attributes ctx)))
 (clojure.core/defn
  conexp-clj-immigrate
  ([& ns-names] (clojure.core/apply conexp.main/immigrate ns-names)))
 (clojure.core/defn
  conexp-clj-stem_base
  ([ctx] (conexp.main/stem-base ctx))
  ([ctx background-knowledge]
   (conexp.main/stem-base ctx background-knowledge)))
 (clojure.core/defn conexp-clj-clojure_set)
 (clojure.core/defn
  conexp-clj-compatible_subcontext
  ([ctx-1 ctx-2] (conexp.main/compatible-subcontext? ctx-1 ctx-2)))
 (clojure.core/defn
  conexp-clj-superset
  ([set1 set2] (conexp.main/superset? set1 set2)))
 (clojure.core/defn
  conexp-clj-print_context
  ([ctx & args]
   (clojure.core/apply conexp.main/print-context ctx args)))
 (clojure.core/defn
  conexp-clj-ensure_length
  ([string length] (conexp.main/ensure-length string length))
  ([string length padding]
   (conexp.main/ensure-length string length padding)))
 (clojure.core/defn conexp-clj-clojure_seq)
 (clojure.core/defn conexp-clj-clojure_vec)
 (clojure.core/defn
  conexp-clj-to_set
  ([thing] (conexp.main/to-set thing)))
 (clojure.core/defn
  conexp-clj-conclusion
  ([thing] (conexp.main/conclusion thing)))
 (clojure.core/defn conexp-clj-ceil ([n] (conexp.main/ceil n)))
 (clojure.core/defn
  conexp-clj-lattice_inf_irreducibles
  ([lat] (conexp.main/lattice-inf-irreducibles lat)))
 (clojure.core/defn
  conexp-clj-index
  ([xrel ks] (conexp.main/index xrel ks)))
 (clojure.core/defn
  conexp-clj-clarify_context
  ([ctx] (conexp.main/clarify-context ctx)))
 (clojure.core/defn
  conexp-clj-get_default_layout_format
  ([] (conexp.main/get-default-layout-format)))
 (clojure.core/defn
  conexp-clj-inf_additive_layout
  ([lattice] (conexp.main/inf-additive-layout lattice)))
 (clojure.core/defn
  conexp-clj-lattice_sup_irreducibles
  ([lat] (conexp.main/lattice-sup-irreducibles lat)))
 (clojure.core/defn
  conexp-clj-incident
  ([ctx g m] (conexp.main/incident? ctx g m)))
 (clojure.core/defn
  conexp-clj-iceberg_intent_seq
  ([context minsupp] (conexp.main/iceberg-intent-seq context minsupp)))
 (clojure.core/defn
  conexp-clj-hash_combine_hash
  ([& args] (clojure.core/apply conexp.main/hash-combine-hash args))))
