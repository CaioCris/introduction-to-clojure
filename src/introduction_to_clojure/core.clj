(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& msgs]
  (apply println msgs)
  :error)

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))

(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))

(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))

(defn add-butter []
  (grab :butter)
  (add-to-bowl))

(def scooped-ingredients #{:milk :flour :sugar})

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn add-eggs [n]
  (dotimes [_ n]
    (add-egg))
  :ok)

(defn add-flour-cups [n]
  (dotimes [_ n]
    (add-flour))
  :ok)

(defn add-milk-cups [n]
  (dotimes [_ n]
    (add-milk))
  :ok)

(defn add-sugar-cups [n]
  (dotimes [_ n]
    (add-sugar))
  :ok)

(defn add-butters [n]
  (dotimes [_ n]
    (add-butter))
  :ok)

(defn add-squeezed
  ([ingredient amount]
   (if (squeezed? ingredient)
     (do
       (dotimes [_ amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       :ok)
     (error "This function only works on squeezed ingredients. You asked me to squeeze" ingredient)))
  ([ingredient]
   (add-squeezed ingredient 1)))

(defn add-scooped
  ([ingredient amount]
   (if (scooped? ingredient)
     (do
       (dotimes [_ amount]
         (grab :cup)
         (scoop ingredient)
         (add-to-bowl)
         (release))
       :ok)
     (error "This function only works on scooped ingredients. You asked me to scoop" ingredient)))
  ([ingredient]
   (add-scooped ingredient 1)))

(defn add-simple
  ([ingredient amount]
   (if (simple? ingredient)
     (do
       (dotimes [_ amount]
         (grab ingredient)
         (add-to-bowl))
       :ok)
     (error "This function only works on simple ingredients. You asked me to add" ingredient)))
  ([ingredient]
   (add-simple ingredient 1)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (cond
     (squeezed? ingredient)
     (add-squeezed ingredient amount)
     (scooped? ingredient)
     (add-scooped ingredient amount)
     (simple? ingredient)
     (add-simple ingredient amount)
     :else
     (error "I do not know the ingredient" ingredient))))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :sugar 1)
  (add :butter 1)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(def pantry-ingredients #{:flour :sugar})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def fridge-ingredients #{:milk :egg :butter})

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

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

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (cond
     (from-pantry? ingredient)
     (fetch-from-pantry ingredient amount)
     (from-fridge? ingredient)
     (fetch-from-fridge ingredient amount)
     :else
     (error "I don't know where to get" ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [_ amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [_ amount]
    (unload ingredient)))

(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})

(defn fetch-list [shopping]
  (doseq [location (keys locations)]
    (go-to location)
    (doseq [ingredient (get locations location)]
      (load-up-amount ingredient (get shopping ingredient 0))))
  (go-to :prep-area)
  (doseq [location (keys locations)]
    (doseq [ingredient (get locations location)]
      (unload-amount ingredient (get shopping ingredient 0)))))

(def cake-recipe {:egg 2 :flour 2 :milk 1 :sugar 1})
(def cookies-recipe {:egg 1 :flour 1 :butter 1 :sugar 1})

(defn send-delivery [order rack-id]
  {:orderid (get order :orderid)
   :address (get order :address)
   :rackids [rack-id]})

;(defn day-at-the-bakery []
;  (let [orders (get-morning-orders)]
;    (doseq [order orders]
;      (let [items (get order :items)]
;        (dotimes [_ (get items :cake 0)]
;          (fetch-list cake-recipe)
;          (let [rack-id (bake-cake)]
;            (delivery (send-delivery order rack-id))))
;        (dotimes [_ (get items :cookies 0)]
;          (fetch-list cookies-recipe)
;          (let [rack-id (bake-cookies)]
;            (delivery (send-delivery order rack-id))))))))

(defn add-ingredients [cake-ingredients cookies-ingredients]
  (merge-with + cake-ingredients cookies-ingredients))

(defn multiply-ingredients [quantity recipe-list]
  (into {}
        (for [kv recipe-list]
          [(first kv) (* quantity (second kv))])))

(defn order->ingredients [order]
  (let [items (get order :items)
        cake-ingredients (multiply-ingredients (get items :cake 0) cake-recipe)
        cookies-ingredients (multiply-ingredients (get items :cookies 0) cookies-recipe)]
    (add-ingredients cake-ingredients cookies-ingredients)))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)]
        (dotimes [_ (get items :cake 0)]
          (let [rack-id (bake-cake)]
            (delivery (send-delivery order rack-id))))
        (dotimes [_ (get items :cookies)]
          (let [rack-id (bake-cookies)]
            (delivery (send-delivery order rack-id))))))))

(defn -main []
  (day-at-the-bakery))