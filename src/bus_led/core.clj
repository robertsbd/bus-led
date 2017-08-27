(ns bus-led.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [gpio.core :refer :all]))

;; application ID and app key, send with each API request.
(def app-id "965b845c")
(def app-key "054fe673c1dc4990bbb7c0de795caf64")

;; Bus stop and route information
(def stop-east "490005275E1") ;; heading east
(def stop-west "490005275W1") ;; heading west
(def bus-routes "144,41") ;; string of the bus routes of interest

;; led values
(def led-vals (ref {:green nil :amber nil :white nil}))

;; Which pins are we using
(def port-green-led (open-port 15))
(def port-amber-led (open-port 14))
(def port-white-led (open-port 18))

;; functions to work with the data from the tfl api

(defn mins-between [start-date end-date]
  "Returns the difference in minutes between two dates,
  returns a negative value if end-date is before start-date"
  (if (t/after? end-date start-date)
    (t/in-minutes (t/interval start-date end-date))
    (* -1 (t/in-minutes (t/interval end-date start-date)))))

(defn list-of-arrival-times [data-from-api]
  "Will provide a list of arrival times from api output"
  (map #(mins-between (t/now) (c/from-string (% "expectedArrival"))) data-from-api))

(defn get-bus-times-from-api [stop-id bus-routes]
  "Read line status information from API as JSON, detail is false as the
  information is not required, returns a clojure sequence of maps"
nn  (->> (str "https://api.tfl.gov.uk/Line/" bus-routes "/Arrivals?stopPointId=" stop-id  "&app_id=" app-id "&app_key=" app-key)
      slurp
      json/read-str
      list-of-arrival-times
      ))

;; functions associated with the gpio and led values

(defn close-all-ports! []
  "will close all the open ports"
  (do
    (close! port-green-led)
    (close! port-amber-led)
    (close! port-white-led)))

(defn new-led-vals! [mins-to-bus-list]
  "set the values of the refs that specify the values of the leds 
  taking the minimum value of the bus"
  (let [num-bus (count (filter #(and (<= % 10) (>= % 3)) mins-to-bus-list))]
   (dosync
    (alter led-vals assoc-in [:green] (if (> num-bus 1) :high :low))
    (alter led-vals assoc-in [:amber] (if (= num-bus 1) :high :low))
    (alter led-vals assoc-in [:white] (if (> (count (filter #(and (<= % 4) (>= % 3)) mins-to-bus-list)) 0) :high :low))
    )))

(defn display-buses-with-leds []
  "Set the leds to output sand then every 10 seconds change the leds depending on bus timing. Loop forever."
  (let [bus-times (get-bus-times-from-api stop-east bus-routes)]
    (do
      (set-direction! port-green-led :out)
      (set-direction! port-amber-led :out)
      (set-direction! port-white-led :out)
      (while true
        (new-led-vals! bus-times)
        (gpio/write-value! port-green-led (@led-vals :green))
        (gpio/write-value! port-amber-led (@led-vals :amber))
        (gpio/write-value! port-white-led (@led-vals :white))
        (Thread/sleep 10000)))))

(defn -main []
  (do
    (display-buses-with-leds)))
