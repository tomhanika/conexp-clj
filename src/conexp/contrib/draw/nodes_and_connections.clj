;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.nodes-and-connections
  (:use [conexp.base :only (ns-doc,defvar-, defvar, round, union, difference)]
	conexp.contrib.draw.scenes)
  (:use	[clojure.contrib.core :only (-?>)])
  (:import [java.awt Color]
	   [no.geosoft.cc.graphics GWindow GScene GObject GSegment
	                           GStyle GInteraction ZoomInteraction
	                           GText GPosition]))

(ns-doc
 "Namespace for representing nodes and their connections for drawing
 lattice diagrams.")

;;; nodes and connections

(defn node?
  "Tests whether thing is a node of a lattice diagram or not."
  [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData ^GObject thing)) :node)))

(defn connection?
  "Tests whether thing is a connection of a lattice diagram or not."
  [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData ^GObject thing)) :connection)))

(defn position
  "Returns the position of a node in a lattice diagram."
  [^GObject node]
  (:position @(.getUserData node)))

(defn radius
  "Returns the radius of a node in a lattice diagram."
  [^GObject node]
  (:radius @(.getUserData node)))

(defn set-node-radius!
  "Sets radius of node."
  [^GObject node, radius]
  (dosync
   (alter (.getUserData node) assoc :radius radius)))

(defn get-name
  "Returns name of thing."
  [^GObject thing]
  (:name @(.getUserData thing)))

(defn lower-node
  "Returns for a connection conn the lower node in a lattice diagram."
  [^GObject conn]
  (:lower @(.getUserData conn)))

(defn upper-node
  "Returns for a connection conn the upper node in a lattice diagram."
  [^GObject conn]
  (:upper @(.getUserData conn)))

(defn upper-connections
  "Returns all upper connections for node in a lattice diagram."
  [^GObject node]
  (:upper @(.getUserData node)))

(defn lower-connections
  "Returns all lower connections of node in a lattice diagram."
  [^GObject node]
  (:lower @(.getUserData node)))

(defn upper-neighbors
  "Returns all upper neighbors of node in a lattice diagram."
  [node]
  (map upper-node (upper-connections node)))

(defn lower-neighbors
  "Returns all lower neighbors of node in a lattice diagram."
  [node]
  (map lower-node (lower-connections node)))


;;; create and connect nodes

(defvar- *default-node-style* (doto (GStyle.)
				(.setForegroundColor Color/BLACK)
				(.setBackgroundColor Color/WHITE)
				(.setLineWidth 1.0))
  "Default node style for lattice diagrams.")

(defvar- *default-object-concept-style* (doto (GStyle.)
					  (.setBackgroundColor Color/BLACK))
  "Default style for nodes being an object concept.")

(defvar- *default-attribute-concept-style* (doto (GStyle.)
					     (.setBackgroundColor Color/BLUE))
  "Default style for nodes being an attribute concept.")

(defvar- *default-node-label-style* (doto (GStyle.)
				      (.setBackgroundColor Color/WHITE))
  "Default style for node labels.")

(defn- create-two-halfcircles
  "Creates points for two half circles, lower circle points first."
  [x y radius]
  (let [x (double x),
	y (double y),
	radius (double radius),
	samples 100,
	angle (/ Math/PI samples),
	[upper-points, lower-points] (loop [current-angle (double (- (/ Math/PI 2.0))),
					    acc-samples 0,
					    upper-points [],
					    lower-points []]
				       (if (> acc-samples samples)
					 [upper-points, lower-points]
					 (let [new-x (+ x (* radius (Math/sin current-angle))),
					       new-y (+ y (* radius (Math/cos current-angle)))]
					   (recur (double (+ current-angle angle))
						  (inc acc-samples)
						  (conj upper-points new-x new-y)
						  (conj lower-points new-x (+ y y (- new-y)))))))]
    [(into-array Double/TYPE lower-points), (into-array Double/TYPE upper-points)]))

(defvar *default-node-radius* 5.0
  "Initial node radius when drawing lattices.")

(defn- add-node
  "Adds a node to scn at position [x y]."
  [^GScene scn, x, y, name, [upper-label lower-label]]
  (let [^GSegment upper-segment (GSegment.),
	^GSegment lower-segment (GSegment.),
	object (proxy [GObject] []
		 (draw []
		   (let [upper-style (if (= 1 (-?> this upper-neighbors count))
				       *default-attribute-concept-style*
				       nil),
			 lower-style (if (= 1 (-?> this lower-neighbors count))
				       *default-object-concept-style*
				       nil),
			 [x y] (position this),
			 [l u] (create-two-halfcircles x y (radius this))]
		     (.setGeometryXy lower-segment l)
		     (.setGeometryXy upper-segment u)
		     (.setStyle lower-segment lower-style)
		     (.setStyle upper-segment upper-style))))
	style (GStyle.)]
    (doto object
      (.setStyle *default-node-style*)
      (.addSegment lower-segment)
      (.addSegment upper-segment)
      (.setUserData (ref {:type :node,
			  :position [(double x), (double y)],
			  :radius *default-node-radius*,
			  :name name})))
    (doto scn
      (.add object))

    (let [^GText upper-text (GText. (print-str upper-label) GPosition/NORTH),
	  ^GText lower-text (GText. (print-str lower-label) GPosition/SOUTH)]
	(.setStyle upper-text *default-node-label-style*)
	(.setStyle lower-text *default-node-label-style*)
	(.addText upper-segment upper-text)
	(.addText lower-segment lower-text))

    object))

;;

(defvar- *default-highlighted-node-style* (doto (GStyle.)
                                            (.setForegroundColor Color/RED)
                                            (.setBackgroundColor Color/WHITE)
                                            (.setLineWidth 3.0))
  "Default node style for lattice diagrams for highlighted nodes.")

(defn highlight-node
  "Toggles the highlight-state of the given node."
  [node]
  (if (= (.getStyle node) *default-node-style*)
    (.setStyle node *default-highlighted-node-style*)
    (.setStyle node *default-node-style*)))

;;

(defvar- *default-line-style* (doto (GStyle.)
				(.setLineWidth 2.0)
				(.setForegroundColor Color/BLACK))
  "Default line style.")

(defn- connect-nodes
  "Connects two nodes on scene."
  ([^GScene scn, ^GObject x, ^GObject y]
    (connect-nodes scn x y (str (get-name x) " -> " (get-name y))))
  ([^GScene scn, ^GObject x, ^GObject y, name]
     (let [line (GSegment.),
	   c    (proxy [GObject] []
		  (draw []
		    (let [[x1 y1] (position (lower-node this))
			  [x2 y2] (position (upper-node this))]
		      (.setGeometry line
				    (double x1) (double y1)
				    (double x2) (double y2)))))]
       (doto scn
	 (.add c))
       (doto line
	 (.setStyle *default-line-style*))
       (doto c
	 (.addSegment line)
	 (.toBack)
	 (.setUserData (ref {:type :connection,
			     :lower x,
			     :upper y,
			     :name name})))
       (dosync
	(alter (.getUserData x) update-in [:upper] conj c)
	(alter (.getUserData y) update-in [:lower] conj c)))))


;;; move nodes around

(defn- height-of-lower-neighbors
  "Returns maximal height of all lower neighbors."
  [node]
  (let [lowers (lower-neighbors node)]
    (if (empty? lowers)
      nil
      (reduce max (map (fn [node]
			 ((position node) 1))
		       lowers)))))

(defn- height-of-upper-neighbors
  "Returns minimal height of all upper neighbors."
  [node]
  (let [uppers (upper-neighbors node)]
    (if (empty? uppers)
      nil
      (reduce min (map (fn [node]
			 ((position node) 1))
		       uppers)))))

(defn move-node-unchecked-to
  "Moves node to [new-x new-y]."
  [^GObject node, new-x, new-y]
  ;; update self position
  (dosync
   (alter (.getUserData node) assoc :position [new-x new-y]))

  ;; move node on the device
  (.redraw node)

  ;; update connections to upper neighbors
  (doseq [^GObject c (upper-connections node)]
    (.redraw c))

  ;; update connections to lower neighbors
  (doseq [^GObject c (lower-connections node)]
    (.redraw c))

  ;; done
  [new-x new-y])

(defn move-node-by
  "Moves node by [dx dy] making sure it will not be over some of its
  upper neighbors or under some of its lower neighbors."
  [^GObject node, dx, dy]
  (let [[x y] (position node),

	;; make sure nodes don't go too far
	max-y (height-of-upper-neighbors node),
	min-y (height-of-lower-neighbors node),
	dy    (if max-y (min dy (- max-y y)) dy),
	dy    (if min-y (max dy (- min-y y)) dy)]
    (move-node-unchecked-to node (+ x dx) (+ y dy))))


;;; moving utilities

(defn- all-neighbored-nodes
  "Returns all directly and indirectly neighbored nodes of node."
  ([node neighbors]
     (all-neighbored-nodes neighbors (set (neighbors node)) #{}))
  ([neighbors to-process visited]
     (if (empty? to-process)
       visited
       (let [next (first to-process)]
	 (if (contains? visited next)
	   (recur neighbors (rest to-process) visited)
	   (let [neighs (neighbors next)]
	     (recur neighbors
		    (into (rest to-process) neighs)
		    (conj visited next))))))))

(defn all-nodes-above
  "Returns the set of all nodes above node."
  [node]
  (all-neighbored-nodes node upper-neighbors))

(defn all-nodes-below
  "Returns the set of all nodes below node."
  [node]
  (all-neighbored-nodes node lower-neighbors))

(defn- all-irreducible-neighbored-nodes
  "Returns all directly and indirectly neighbored nodes of node being
  irreducible."
  ;; copy and paste, how can this be changed?
  ([node neighbors]
     (all-irreducible-neighbored-nodes neighbors #{node} #{}))
  ([neighbors to-process visited]
     (if (empty? to-process)
       visited
       (let [next (first to-process)]
	 (if (contains? visited next)
	   (recur neighbors (rest to-process) visited)
	   (let [neighs (neighbors next)]
	     (if (= 1 (count neighs))
	       (recur neighbors (into (rest to-process) neighs) (conj visited next))
	       (recur neighbors (into (rest to-process) neighs) visited))))))))

(defn- group-by-function
  "Categorizes elements in coll by their value under f."
  [f coll]
  (loop [elements coll,
	 category {}]
    (if (empty? elements)
      (vals category)
      (let [next (first elements)]
	(recur (rest elements) (update-in category [(f next)] conj next))))))

(defn- all-additively-influenced-nodes
  "Returns all nodes which are additively influenced by node. upper
  and lower are functions returning the upper and lower neighbors
  respectively (these roles can be interchanged without any harm). The
  nodes are given with weights (as pair of node and weight)
  representing the influence by node."
  [node uppers lowers]
  (let [irrs (all-irreducible-neighbored-nodes node uppers),
	others (group-by-function identity
			 (apply concat (map #(all-neighbored-nodes % lowers) irrs))),

	irr-count (count irrs)]
    (concat (for [n irrs
		  :when (not= n node)]
	      [n (/ irr-count)])
	    (for [nodes others
		  :when (not= (first nodes) node)]
	      [(first nodes) (/ (count nodes) irr-count)]))))

(defn all-inf-add-influenced-nodes
  "Returns all nodes (with weights) which are infimum-additively
  influenced by node."
  [node]
  (all-additively-influenced-nodes node upper-neighbors lower-neighbors))

(defn all-sup-add-influenced-nodes
  "Returns all nodes (with weights) which are supremum-additively
  influenced by node."
  [node]
  (all-additively-influenced-nodes node lower-neighbors upper-neighbors))

;;; Interactions

(defn move-interaction
  "Standard move interaction for lattice diagrams. Installs
  :move-start, :move-drag and :move-stop hooks on scene to be called
  whenever a node is moved. Callbacks get the moved vertex as
  argument, :move-drag additionally gets the vector by which the given
  vertex has been moved."
  [scene]
  (add-hook scene :move-start)
  (add-hook scene :move-drag)
  (add-hook scene :move-stop)
  (let [interaction-obj (atom nil)]
    (proxy [GInteraction] []
      (event [^GScene scn, evt, x, y]
	(condp = evt
	   GWindow/BUTTON1_DOWN  (let [thing (.find scn x y)]
				   (when (node? thing)
				     (reset! interaction-obj thing)
				     (call-hook-with scn :move-start thing))),
	   GWindow/BUTTON1_DRAG  (when @interaction-obj
				   (let [[a b] (position @interaction-obj),
					 [x y] (device-to-world scn x y)]
				     (move-node-by @interaction-obj (- x a) (- y b))
				     (call-hook-with scn :move-drag @interaction-obj (- x a) (- y b))
				     (.refresh scn))),
	   GWindow/BUTTON1_UP    (do
				   (call-hook-with scn :move-stop @interaction-obj)
				   (reset! interaction-obj nil)),
           GWindow/BUTTON3_DOWN  (let [thing (.find scn x y)]
                                   (when (node? thing)
                                     (highlight-node thing)
                                     (.refresh scn))),
	   nil)))))

(defn zoom-interaction
  "Standrd zoom interaction for lattice diagrams. Installs
  :zoom-event hook called whenever view changes. Callbacks
  take no arguments."
  [scene]
  (add-hook scene :zoom-event)
  (let [^ZoomInteraction zoom-obj (ZoomInteraction. scene)]
    (proxy [GInteraction] []
      (event [^GScene scn, evt, x, y]
	(.event zoom-obj scn evt x y)
	(when scn
	  (call-hook-with scn :zoom-event)
	  (call-hook-with scn :image-changed))))))

;;;

(defn add-nodes-with-connections
  "Adds to scene scn nodes placed by node-coordinate-map and connected
  via pairs in the sequence node-connections."
  [scn node-coordinate-map node-connections annotation]
  (let [node-map (apply hash-map
			(apply concat (map (fn [[node [x y]]]
					     [node
					      (add-node scn x y node (annotation node))])
					   node-coordinate-map)))]
    (doseq [[node-1 node-2] node-connections]
      (connect-nodes scn (node-map node-1) (node-map node-2)))
    node-map))

;;;

nil
