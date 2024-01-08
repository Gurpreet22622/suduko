(ns starter.browser
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            ))

;; start is called by init and after code reloading finishes



;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))


(defn home-page
  []
  [:div
   [:h1 "Suduko"]])



(defn init []
  [:div
   [home-page]
   
   ])




(defn start []
  (rdom/render [init] (js/document.getElementById "app")))

(start)
