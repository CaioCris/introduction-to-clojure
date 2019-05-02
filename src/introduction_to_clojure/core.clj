(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& msgs]
  (apply println msgs)
  :error)

(def baking {:recipes     {:cake     {:ingredients {:egg 2 :flour 2 :milk 1 :sugar 1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 25]
                                                    [:cool]]
                                      }
                           :cookies  {:ingredients {:egg 1 :flour 1 :butter 1 :sugar 1}
                                      :steps       [[:add :all]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 30]
                                                    [:cool]]
                                      }
                           :brownies {:ingredients {:butter 2 :sugar 1 :cocoa 2 :flour 2 :egg 2 :milk 1}
                                      :steps       [[:add :butter]
                                                    [:add :sugar]
                                                    [:add :cocoa]
                                                    [:mix]
                                                    [:add :flour]
                                                    [:add :egg]
                                                    [:add :milk]
                                                    [:mix]
                                                    [:pour]
                                                    [:bake 35]
                                                    [:cool]]}}
             :ingredients {:egg    {:storage :fridge
                                    :usage   :squeezed}
                           :butter {:storage :fridge
                                    :usage   :simple}
                           :milk   {:storage :fridge
                                    :usage   :scooped}
                           :flour  {:storage :pantry
                                    :usage   :scooped}
                           :cocoa  {:storage :pantry
                                    :usage   :scooped}
                           :sugar  {:storage :pantry
                                    :usage   :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [_ amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple   (fn [ingredient amount]
                        (dotimes [_ amount]
                          (grab ingredient)
                          (add-to-bowl)))
            :scooped  (fn [ingredient amount]
                        (grab :cup)
                        (dotimes [_ amount]
                          (scoop ingredient)
                          (add-to-bowl))
                        (release))})

(defn usage-type [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :usage)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (let [ingredient-type (usage-type ingredient)]
     (if (contains? usage ingredient-type)
       (let [func (get usage ingredient-type)]
         (func ingredient amount))
       (error "I do not know the ingredient" ingredient)))))

(defn perform [ingredients step]
  (cond
    (= :mix (first step))
    (mix)
    (= :pour (first step))
    (pour-into-pan)
    (= :bake (first step))
    (bake-pan (second step))
    (= :cool (first step))
    (cool-pan)
    (= :add (first step))
    (cond
      (and (= 2 (count step))
           (= :all (second step)))
      (doseq [kv ingredients]
        (add (first kv) (second kv)))
      (and (= 2 (count step))
           (contains? ingredients (second step)))
      (add (second step) (get ingredients (second step)))
      (= 3 (count step))
      (add (second step) (get step 2))
      :else
      (error "I don't know how to add" (second step) (get step 2)))
    :else
    (error "I don't know how to" (first step))))

(defn bake-recipe [recipe]
  (let [ingredients (get recipe :ingredients)]
    (last
      (for [step (get recipe :steps)]
        (perform ingredients step)))))

(defn from-pantry? [ingredient]
  (let [ingredients (get baking :ingredients)
        ingredient-info (get ingredients ingredient)]
    (= :pantry (get ingredient-info :storage))))

(defn from-fridge? [ingredient]
  (let [ingredients (get baking :ingredients)
        ingredient-info (get ingredients ingredient)]
    (= :fridge (get ingredient-info :storage))))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [_ amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [_ amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the pantry. You asked me to fetch" ingredient))))

(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [_ amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [_ amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the fridge. You asked me to fetch" ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [_ amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [_ amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (let [ingredients (get baking :ingredients)
         ingredient-info (get ingredients ingredient)]
     (if (contains? ingredients ingredient)
       (do
         (go-to (get ingredient-info :storage))
         (load-up-amount ingredient amount)
         (go-to :prep-area)
         (unload-amount ingredient amount))
       (error "I don't know the ingredient" ingredient)))))

(defn storage-location [item]
  (let [ingredients (get baking :ingredients)
        info (get ingredients item)]
    (get info :storage)))

(defn fetch-list [shopping]
  (let [locations (group-by (fn [item-amount]
                              (storage-location (first item-amount)))
                            shopping)]
    (doseq [location (keys locations)]
      (go-to location)
      (doseq [item-amout (get locations location)]
        (load-up-amount (first item-amout) (second item-amout))))
    (go-to :prep-area)
    (doseq [location (keys locations)]
      (doseq [item-amout (get locations location)]
        (unload-amount (first item-amout) (second item-amout))))))

(defn send-delivery [order racks]
  {:orderid (get order :orderid)
   :address (get order :address)
   :rackids racks})

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [quantity recipe-list]
  (into {}
        (for [kv recipe-list]
          [(first kv) (* quantity (second kv))])))

(defn order->ingredients [order]
  (let [items (get order :items)
        recipes (get baking :recipes)]
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipe (get recipes (first kv))
                    ingredients (get recipe :ingredients)]
                (multiply-ingredients (second kv) ingredients))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn bake [item]
  (let [recipes (get baking :recipes)]
    (if (contains? recipes item)
      (bake-recipe (get recipes item))
      (error "I don't know how to bake" item))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders-day3)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (println "Cooking the order number:" (get order :orderid))
      (let [items (get order :items)
            racks (for [kv items
                        _ (range (second kv))]
                    (bake (first kv)))]
        (delivery (send-delivery order racks))
        (println "Delivery of number:" (get order :orderid))))))


(defn -main []
  (day-at-the-bakery))