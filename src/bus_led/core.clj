(ns bus-led.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]))

;; get application ID and app key, send with each API request.
(def config-info (json/read-str (slurp (io/resource "config.json"))))

;; Bus stop and route information
(def bus-stop (config-info "bus_stop")) ;; heading east
(def bus-routes (config-info "bus_routes")) ;; string of the bus routes of interest

;; Which pins are we using
(def pin-green "4")
(def pin-amber "1")
(def pin-white "6")

(def run-state (atom nil)) ;; this will receive commands to be passed around and provide thread control

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
  (->> (str "https://api.tfl.gov.uk/Line/" bus-routes "/Arrivals?stopPointId=" stop-id  "&app_id=" (config-info "app-id") "&app_key=" (config-info "app-key"))
      slurp
      json/read-str
      list-of-arrival-times
      ))

;; functions associated with the gpio and led values
(defn init-pins! []
  (do
    (sh "gpio" "mode" pin-green "out")
    (sh "gpio" "mode" pin-amber "out")
    (sh "gpio" "mode" pin-white "out")))
  
(defn reset-pins! []
  "resets the ports to their original values"
  (do
    (sh "gpio" "write" pin-green "0")
    (sh "gpio" "write" pin-amber "0")
    (sh "gpio" "write" pin-white "0")
    (sh "gpio" "mode" pin-green "in")
    (sh "gpio" "mode" pin-amber "in")
    (sh "gpio" "mode" pin-white "in")))

(defn write-pins! [new-pin-vals]
  "writes the new values to the pins"
  (do
    (sh "gpio" "write" pin-green (new-pin-vals :green))
    (sh "gpio" "write" pin-amber (new-pin-vals :amber))
    (sh "gpio" "write" pin-white (new-pin-vals :white))))

(defn new-led-vals [mins-to-bus-list]
  "get the new values for the leds given the time until 
  the bus arrives"
  (let [num-bus (count (filter #(and (<= % 10) (>= % 3)) mins-to-bus-list))]
   (conj {}
    {:green (if (> num-bus 1) "1" "0")}
    {:amber (if (= num-bus 1) "1" "0")}
    {:white (if (> (count (filter #(and (<= % 4) (>= % 3)) mins-to-bus-list)) 0) "1" "0")})))

(defn display-buses-with-leds []
  "Set the leds to output and then every 10 seconds change the leds depending on bus timing. Loop forever."
  (do
    (println "Bus monitoring thread open")
    (init-pins!)
    (loop [bus-times (get-bus-times-from-api bus-stop bus-routes)]
        (when (= @run-state :go)
          (write-pins! (new-led-vals bus-times))
          (Thread/sleep 10000)
          (recur (get-bus-times-from-api bus-stop bus-routes))))
    (reset-pins!)
    (println "Bus monitoring thread closed")))

(defn flash-led! [pin]
  "This just run a simple display to show that a message has been received"
  (do
    (init-pins!)
    (sh "gpio" "write" pin "1")
    (Thread/sleep 100)
    (sh "gpio" "write" pin "0")
    (Thread/sleep 100)
    (sh "gpio" "write" pin "1")
    (Thread/sleep 100)
    (sh "gpio" "write" pin "0")
    (Thread/sleep 100)
    (sh "gpio" "write" pin "1")
    (Thread/sleep 100)
    (reset-pins!)
    ))

(defn -main []
  "main function to coordinate running the LEd bus-countdown"
  (do
    (flash-led! pin-amber)
    (flash-led! pin-green)
    (flash-led! pin-white)
    (println "LED bus countdown (commands :start/:stop/:quit)")
    (loop [input (read-line)]
      (if (= input ":quit")
        (do
          (reset! run-state :stop)
          (println "QUIT")
          (flash-led! pin-amber)
          (flash-led! pin-green)
          (flash-led! pin-white))
        (do
          (case input
            ":start" (do
                       (flash-led! pin-green)
                       (reset! run-state :go)
                       (future (display-buses-with-leds)))
            ":stop" (do
                        (flash-led! pin-amber)
                        (reset! run-state :stop))
            (println "Unknown command"))
          (recur (read-line)))))))
        
