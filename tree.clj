(ns clojure-btree.tree)


(defrecord Node [value left right])


(defn add-value-to-node
  ;; first form, a single node and value
  ([node value]
   (if (nil? node)
     ;; if the node is empty, return an empty node with the provided
     ;; value and two niul values
     (Node. value nil nil)

     ;; otherwise
     (let [nodeValue (:value node)
           comparison (compare nodeValue value)]
       (cond
         (neg? comparison) (assoc node :right (add-value-to-node (:right node) value))
         (pos? comparison) (assoc node :left (add-value-to-node (:left node) value))
         :else node))))
  ([node value & more]
   (reduce add-value-to-node (add-value-to-node node value) more)))

(defn make-tree
  "Creates a tree with one or more values, in order"
  [& values]
  (apply add-value-to-node nil values))

(defn count-nodes
  "Counts all the child nodes of the argument, including itself"
  [node]
  )

(defn count-nodes
  "counts all the nodes under the right of the root"
  [node side]
  (if (nil? side)
    ;; both sides:
    (if (nil? node)
      0
      (+ 1 (count-nodes (:left node)) (count-nodes (:right node))))
    ;; a single side, left or right.
    (or (nil? node) (nil? ((keyword side) node side)))
    0
    (count-nodes ((keyword side) node side))))


(defn inorder
  [node]
  (if (nil? node)
    '()
    (concat
     (inorder (:left node))
     (list (:value node))
     (inorder (:right node)))))


(defn preorder
  [node]
  (if (nil? node)
    '()
    (concat
     (list (:value node))
     (inorder (:left node))
     (inorder (:right node)))))


(defn postorder
  [node]
  (if (nil? node)
    '()
    (concat
     (inorder (:right node))
     (list (:value node))
     (inorder (:left node)))))


(defn breadth-first
  [& nodes]
  (if (or (empty? nodes) (nil? nodes) (= nodes '(nil)))
    '()
    (concat
     (map :value nodes)
     (apply breadth-first
            (filter identity (mapcat (juxt :left :right) nodes))))))


(defn depth
  [node]
  (if (nil? node)
    0
    (+ 1 (max (tree-depth (:left node)) (tree-depth (:right node))))))



(defn remove-value
  "Removes a value from the tree"
  [node value]
  (let [current-value (:value node)]
    (cond
      (nil? node) nil
      (< value current-value) (assoc node :left (remove-value (:left node) value))
      (> value current-value) (assoc node :right (remove-value (:right node) value))
      :else (let [left (:left node)
                  right (:right node)]
              (cond
                (and (nil? left) (nil? right)) nil ; remove current node
                (nil? left) right ; replace current with right
                (nil? right) left ; replace current with left
                :else (let [smallest-value (find-min-value right)]
                        (assoc node :value smallest-value
                               :right (remove-value right smallest-value)))
                )))))


(defn rotate-right
  "Rotates the tree to the right"
  [node]
  (cond
    (nil? node) nil
    (nil? (:left node)) node
    :else (let [left-child (:left node)]
            (assoc left-child :right (assoc node :left (:right left-child))))))

(defn rotate-left
  "Rotates the tree to the left"
  [node]
  (cond
    (nil? node) nil
    (nil? (:right node)) node
    :else (let [right-child (:right node)]
                        (assoc right-child :left (assoc node :right (:left right-child))))))
