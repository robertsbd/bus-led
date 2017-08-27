(ns bus-led.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [gpio.core :as gpio]))

;; clojure version 1.8.0 on mac 2017-710.616
;; CIDER 0.15.0snapshot
;; nREPL 0.2.12

;; now I just need to change the state of the leds based on when the next bus is coming

;; application ID and app key, send with each API request.
(def app-id "965b845c")
(def app-key "054fe673c1dc4990bbb7c0de795caf64")

;; Bus stop and route information
(def stop-dir-east "490005275E1") ;; heading east
(def stop-dir-west "490005275W1") ;; heading west
(def bus-routes "144,41") ;; string of the bus routes of interest

;; Which pins are we using
(def port-green-led (gpio/open-port 15))
(def port-amber-led (gpio/open-port 14))
(def port-white-led (gpio/open-port 18))

;; set the ports to out mode
(gpio/set-direction! port-green-led :out)
(gpio/set-direction! port-amber-led :out)
(gpio/set-direction! port-white-led :out)

;; a reference for each of the pin values
(def val-green-led (ref nil))
(def val-amber-led (ref nil))
(def val-white-led (ref nil))

;; functions to work with the data from the tfl api

(defn mins-between
  "Returns the difference in minutes between two dates,
  returns a negative value if end-date is before start-date"
  [start-date end-date]
  (if (t/after? end-date start-date)
    (t/in-minutes (t/interval start-date end-date))
    (* -1 (t/in-minutes (t/interval end-date start-date)))))

(defn list-of-arrival-times
  "Will provide a list of arrival times from api output"
  [data-from-api]
  (map #(mins-between (t/now) (c/from-string (% "expectedArrival"))) data-from-api))

(defn get-bus-times-from-api
  "Read line status information from API as JSON, detail is false as the
  information is not required, returns a clojure sequence of maps"
  [stop-id bus-routes]
  (->> (str "https://api.tfl.gov.uk/Line/" bus-routes "/Arrivals?stopPointId=" stop-id  "&app_id=" app-id "&app_key=" app-key)
      slurp
      json/read-str
      list-of-arrival-times
      ))

;; functions to work with the leds. I would like to create pulse function in here

(defn close-all-ports
  "will close all the open ports"
  []
  (do
    (gpio/close! port-green-led)
    (gpio/close! port-amber-led)
    (gpio/close! port-white-led)))

(defn new-led-vals
  "set the values of the refs that specify the values of the leds 
  taking the minimum value of the bus"
  [mins-to-bus-list]
  (dosync
   (def num-bus-next-10min (count (filter #(and (<= % 10) (>= % 3)) mins-to-bus-list)))

   (if (> num-bus-next-10min 1) ;; if there is more than 1 bus between 3 and 10 mins
     (ref-set val-green-led :high)
     (ref-set val-green-led :low))

   (if (= num-bus-next-10min 1) ;; if there is only 1 bus in next 10 mins
     (ref-set val-amber-led :high)
     (ref-set val-amber-led :low))

   (if (> (count (filter #(and (<= % 4) (>= % 3)) mins-to-bus-list)) 0) ;; if there is a bus in the next 3 to 4 minutes
     (ref-set val-white-led :high)
     (ref-set val-white-led :low))))

(defn change-leds
  "change the leds to the specified vals"
  []
  (do
   (gpio/write-value! port-green-led @val-green-led)
   (gpio/write-value! port-amber-led @val-amber-led)
   (gpio/write-value! port-white-led @val-white-led)))
  
(defn -main
  "Every 10 seconds return list of minutes until next bus"
  []
  (while true
    (def bus-times (get-bus-times-from-api stop-dir-east bus-routes))
    (println (str "Minutes until bus arrivals: " (pr-str (sort bus-times))))
    (new-led-vals bus-times)
    (change-leds)
    (Thread/sleep 10000)
    )
  )
